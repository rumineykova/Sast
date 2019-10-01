module ScribbleGenerativeTypeProvider.CFSMModel

open ITransport

(*Decalre the basic types for handling Scribble CFSM *)
type Role = string 
type State = int
type Label = string




type Transition = 
  | Send of Role*Label*(string list)
  | Recv of Role*Label*(string list)
  | End 

type CFSM = {
  Transitions: Map<State, Map<Transition, State>>; 
  CurrentState: State;
  InitState: State} 

(*Basic CFSM operations*)
let addTransition state transition next (fsm: CFSM)= 
  // get all transitions from the current state, including the new one
  let all_transitions = 
    match (Map.tryFind state fsm.Transitions) with 
    | None -> Map.add transition next Map.empty
    | Some m -> Map.add transition next m
  // update the set of transitions adding the new transition
  {fsm with Transitions = Map.add state all_transitions fsm.Transitions}

// Get all transitions for a state
let getNext state (fsm: CFSM) = 
  match fsm.Transitions|> Map.tryFind state with 
  | None -> failwith (sprintf "The current state is not defined %A %A" state fsm)
  | Some tr -> tr

let initFsm initState =
  {InitState = initState
   CurrentState = initState
   Transitions = Map.empty}  


type TransitionProcessor (transport: TCPRuntime) = 
  member x.handleInput(state, role, label, payloadNames)  = 
      printfn "In handle Input: %i" state
      let inputL = transport.recv role label
      let input = Async.RunSynchronously inputL
      printfn "In handle Input: %i receiving %i" state input
      Runtime.addToContext (List.item 0 payloadNames) input
      let handler = Runtime.getFromRecvHandlers state 
      handler input
    
  member x.handleOutput(state, role, label, payloadNames)  = 
    printfn "In handle Output: %i" state
    let handler = Runtime.getFromSendHandlers state
    handler (Runtime.IContext()) |> ignore
    let payload = Runtime.getFromContext(List.item 0 payloadNames) 
    printfn "In handle Output: %i sendin %i" state payload
    let t = transport.send role label payload
    Async.RunSynchronously t

  member x.handleSelectLabel(state)  = 
    // get the selector function from the list of selectors
    // the selector function returns the label that has to be sent
    printfn "In handle Select label: %i" state 
    let selector = Runtime.getFromSelectorHandlers state
    // Execute the selector! 
    // Executing the selector populates selectedLables with the chosen label               
    selector (Runtime.StateType()) |> ignore
    // Extract the selected label
    let label = Runtime.selectedLabels.[0]
    let register_cont = Runtime.getFromSelectHandlers label
    register_cont (Runtime.StateType()) |> ignore
    // execute send
    label

  member x.handleBranchLabel(role)  = 
    printfn "In handleBranchLabel: %s" role
    // read the label that is received  
    let label = transport.lookUpLabel role
    // get a function that will register the continuation (CFSM states) for for this branch 
    let register_cont = Runtime.getFromBranchHandlers label 
    // TODO: we need a proper way to give the state here
    // similar to the selector case, this call will only register handlers for the continuation (CFSM states)
    register_cont (Runtime.StateType()) |> ignore 
    label

let eq_transitions(tr,selected_tr) = 
  match (tr, selected_tr) with 
  |(Send(role, label,_),Send(role1, label1,_)) -> 
    role = role1 && label = label1
  |(Recv(role, label,_),Recv(role1, label1,_)) -> 
    role = role1 && label = label1
  | _ -> false

let rec run (fsm:CFSM) (s:State) (processor:TransitionProcessor)  = 
  // get the map of next transitions
  printfn "Current state in run %i" s
  let transitions = getNext s fsm
  // get the first transition from the map of next transitions
  // This is required as to do a pattern matching on the type of the transition 
  let tr_0 = transitions |> Map.toSeq |> Seq.map fst |> Seq.item 0 
  // check if the next transition is a singleton (send or receive)
  let singleton = if (transitions.Count = 1) then true else false
  let selected_tr = 
    match tr_0 with 
    |Send(role, label, payload) when singleton-> 
      printfn "tr_0: In send %s %s" role label
      processor.handleOutput(s, role, label, payload) |> ignore
      tr_0
    |Recv(role, label, payload) when singleton-> 
      printfn "tr_0: In recv %s %s" role label
      processor.handleInput(s, role, label, payload) |> ignore
      tr_0
    |Send(role, _, payloadFake) when (not singleton) -> // next transition is selection 
      // Also registeres handlers for the continuation 
      printfn "tr_0: In select %s" role
      let label = processor.handleSelectLabel(s)
      let (tr_real,_) = 
        transitions 
        |> Map.filter (fun tr _ -> eq_transitions(tr,Send(role, label, payloadFake)))
        |> Map.toList |> List.head 
      let payloadNames = match tr_real with Send(_, _, payload) -> payload
      printfn "tr_0: In select %s %s" role label
      processor.handleOutput(s, role, label, payloadNames) |> ignore
      Send(role, label, payloadNames)

    |Recv(role, _, payloadFake) when (not singleton)-> 
      // Also registeres handlers for the continuation 
      printfn "tr_0: In Branch %s" role
      let label = processor.handleBranchLabel(role)
      let (tr_real,_) = 
        transitions 
        |> Map.filter (fun tr _ -> eq_transitions(tr,Recv(role, label, payloadFake)))
        |> Map.toList |> List.head 
      let payloadNames = match tr_real with Recv(_, _, payload) -> payload
      printfn "tr_0: In Branch %s %s" role label
      processor.handleInput(s, role, label, payloadNames) |> ignore
      Recv(role, label, payloadNames)
    | End -> 
      printf "End of protocol" 
      End
  match selected_tr with 
  |End -> () 
  | _ -> 
    let (_, next_state) = 
      transitions |> Map.filter (fun tr _ -> tr = selected_tr) 
                |> Map.toList |> List.head
    run fsm next_state processor      

// Teh rest are runtime entities for the STP, tehy should no be in the model 
let mutable CFSMcache = Map.empty<string, CFSM>
let init name cfsm = 
   CFSMcache <- CFSMcache.Add(name, cfsm)  
let getCFSM name = CFSMcache.Item(name)

let runFromInit cfsm endpoint = 
  let runtime = TCPRuntime(true, endpoint)
  let processor = TransitionProcessor runtime 
  run cfsm (cfsm.InitState) processor  

(*
let f = initFsm 1 
        |> addTransition 1 (Transition.Send ("Hello","A")) 2
        |> addTransition 2 (Transition.Recv ("Hello","A")) 3 
        |> addTransition 3 (Transition.Send ("Hello","A")) 4
        |> addTransition 4 (Transition.End ("Hello","A")) -4 

run f (f.InitState)   
*)