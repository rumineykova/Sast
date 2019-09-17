module ScribbleGenerativeTypeProvider.CFSMModel

open ITransport

(*Decalre the basic types for handling Scribble CFSM *)
type Role = string 
type State = int
type Label = string

type Transition = 
  | Send of Role*Label
  | Recv of Role*Label
  | End 

type CFSM = {
  Transitions: Map<State, Map<Transition, State>>; 
  CurrentState: State;
  InitState: State} 

(*Basic CFSM operations*)
let addTransition state transition next (fsm: CFSM) = 
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


type TransitionProcessor (transport: LRuntime) = 
  member x.transport = transport
  member x.handleInput(state, role, label)  = 
    let input = x.transport.recv role label 
    Runtime.addToContext (state.ToString()) input
    let handler = Runtime.getFromRecvHandlers state 
    handler input
    
  member x.handleOutput(state, role, label)  = 
    let handler = Runtime.getFromSendHandlers state
    handler (Runtime.IContext()) |> ignore
    let payload = Runtime.getFromContext(state.ToString()) 
    x.transport.send role label payload

  member x.handleSelectLabel(state)  = 
    // get the selector function from the list of selectors
    // the selector function returns the label that has to be sent
    let selector = Runtime.getFromSelectorHandlers state
    // Execute the selector! 
    // Executing the selector populates selectedLables with the chosen label               
    selector (Runtime.StateType()) |> ignore
    // Extract the selected label
    let label = Runtime.selectedLabels.[0]
    // execute send
    label

  member x.handleBranchLabel(role)  = 
    // read the label that is received  
    let label = transport.lookUpLabel role
    // get a function that will register the continuation (CFSM states) for for this branch 
    let register_cont = Runtime.getFromBranchHandlers label 
    // TODO: we need a proper way to give the state here
    // similar to the selector case, this call will only register handlers for the continuation (CFSM states)
    register_cont (Runtime.StateType()) |> ignore 
    label

let rec run (fsm:CFSM) (s:State) (processor:TransitionProcessor)  = 
  // get the map of next transitions 
  let transitions = getNext s fsm
  // get the first transition from the map of next transitions
  // This is required as to do a pattern matching on the type of the transition 
  let tr_0 = transitions |> Map.toSeq |> Seq.map fst |> Seq.item 0 
  // check if the next transition is a singleton (send or receive)
  let singleton = if (transitions.Count = 1) then true else false
  let selected_tr = 
    match tr_0 with 
    |Send(role, label) when singleton-> 
      processor.handleInput(s, role, label)
      tr_0
    |Recv(role, label) when singleton-> 
      processor.handleOutput(s, role, label)
      tr_0
    |Send(role, _) when (not singleton) -> // next transition is selection 
      // Also registeres handlers for the continuation 
      let label = processor.handleSelectLabel(s)
      processor.handleOutput(s, role, label)
      Send(role, label)
    |Recv(role, _) when (not singleton)-> 
      // Also registeres handlers for the continuation 
      let label = processor.handleBranchLabel(role)
      processor.handleInput(s, role, label)
      Recv(role, label)
    | End -> 
      printf "End of protocol" 
      End 
  let (_, next_state) = 
    transitions |> Map.filter (fun tr _ -> tr = selected_tr) 
                |> Map.toList |> List.head
  run fsm next_state processor      

// Teh rest are runtime entities for the STP, tehy should no be in the model 
let mutable CFSMcache = Map.empty<string, CFSM>
let init name cfsm = 
   CFSMcache <- CFSMcache.Add(name, cfsm)  
let getCFSM name = CFSMcache.Item(name)
let runtime = LRuntime(true)
let processor = TransitionProcessor runtime 
let runFromInit cfsm = run cfsm (cfsm.InitState) processor  

(*
let f = initFsm 1 
        |> addTransition 1 (Transition.Send ("Hello","A")) 2
        |> addTransition 2 (Transition.Recv ("Hello","A")) 3 
        |> addTransition 3 (Transition.Send ("Hello","A")) 4
        |> addTransition 4 (Transition.End ("Hello","A")) -4 

run f (f.InitState)   
*)