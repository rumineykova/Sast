module ScribbleGenerativeTypeProvider.CFSMop

type Role = string 
type State = int
type Label = string

type Transition = 
    | Send of Label*Role
    | Recv of Label*Role
    | End of Label*Role

type CFSM = {
    Transitions: Map<State, Map<Transition, State>>; 
    CurrentState: State;
    InitState: State} 

let addTransition current event next (fsm: CFSM) = 
      match (Map.tryFind current fsm.Transitions) with 
      | None -> 
        {fsm with Transitions = fsm.Transitions 
                |> Map.add current (Map.add event next Map.empty)
        }
      | Some m -> 
        {fsm with Transitions = fsm.Transitions 
                |> Map.add current (Map.add event next m)
        } 

// Get one of the next states
let getNext current (fsm: CFSM) = 
    match fsm.Transitions|> Map.tryFind current with 
    | None -> failwith "The current state is not defined"
    | Some m -> let e = m |> Map.toSeq |> Seq.map fst |> Seq.item 0 
                let next = m.Item e
                (e, next)


let initFsm initState =
        {
            InitState = initState
            CurrentState = initState
            Transitions = Map.empty
        }

let mutable cfsmCache = Map.empty<string, CFSM>

let initCFSMCache name  cfsm = 
    //let  cfsm = initFsm firstState 
    cfsmCache <- cfsmCache.Add(name, cfsm)
    
let getCFSM name = cfsmCache.Item(name)

let rec run (fsm:CFSM) s = 
     let (t, next) = getNext s fsm
     match t with 
     |Send (label,role) ->
        let x = 1 // get the funtion from the handlers!!! 
        //Array.append labelSerialized payloadSerialized      
        let func = Runtime.getFromSendHandlers s
        let x = func (Runtime.IContext())
        //printfn "The result of the handler is %i" x 
        let x = 1
        let buf = System.BitConverter.GetBytes(x)
        Runtime.sendMessage "agent" (buf:byte[]) role 
        printfn "I am sending and moving to next state: %i %s and %s " s label role
        run fsm next   

     |Recv (label,role)-> 
        printfn "I am receiving and moving to next state: %i %s and %s " s label role
        printfn "the map for recv is is %A" (Runtime.recvHandlers.Item("recv"))
        printfn "%A" (Runtime.sendHandlers.Item("send"))
        let result = Runtime.receiveMessage "agent" [] role []      
        let decode = new System.Text.UTF8Encoding() 
        let labelRead = decode.GetString(result.[0]) 
        let func = Runtime.getFromRecvHandlers s 
        let applyFunc = func 1 
        printfn "The function should have been executed now"
        run fsm next

     |End (label,role)-> 
        Runtime.stopMessage "agent"
        printf "It is over."  

let runFromInit cfsm = run cfsm (cfsm.InitState) 

(*
let f = initFsm 1 
        |> addTransition 1 (Transition.Send ("Hello","A")) 2
        |> addTransition 2 (Transition.Recv ("Hello","A")) 3 
        |> addTransition 3 (Transition.Send ("Hello","A")) 4
        |> addTransition 4 (Transition.End ("Hello","A")) -4 

run f (f.InitState)   
*)