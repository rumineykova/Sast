module ScribbleGenerativeTypeProvider.CFSMop

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
    | None -> let s = sprintf "The current state is not defined %A %A" current fsm 
              failwith s
    | Some m -> printf "The next (transition, state) is %A" m
                m//let e = m |> Map.toSeq |> Seq.map fst //|> Seq.item 0 
                //e
                //let next = m.Item e
                //(e, next)


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

type LRuntime(isMock) = 
    member x.isMock = isMock 

    member x.processRecv s role label = 
        if x.isMock then 
            printfn "I am receiving and moving to next state: %i %s and %s " s label role
            printfn "the map for recv is is %A" (Runtime.recvHandlers.Item("recv"))
            printfn "%A" (Runtime.recvHandlers.Item("recv"))
            //let result = Runtime.receiveMessage "agent" [] role []      
            //let decode = new System.Text.UTF8Encoding() 
            //let labelRead = "hello" //decode.GetString(result.[0]) 
            let func = Runtime.getFromRecvHandlers s 
            let applyFunc = func 1 // Assuming we have received 1 
            printfn "The function should have been executed now"
        else
            printfn "I am receiving and moving to next state: %i %s and %s " s label role
            printfn "the map for recv is is %A" (Runtime.recvHandlers.Item("recv"))
            printfn "%A" (Runtime.recvHandlers.Item("recv"))
            let result = Runtime.receiveMessage "agent" [] role []      
            let decode = new System.Text.UTF8Encoding() 
            let labelRead = decode.GetString(result.[0]) 
            let func = Runtime.getFromRecvHandlers s 
            // we have to decode the actual int 
            let applyFunc = func 1
            printfn "The function should have been executed now"

    member x.processSend s role label  = 
        if x.isMock then 
            let x = 1 // get the funtion from the handlers!!! 
            //Array.append labelSerialized payloadSerialized      
            let func = Runtime.getFromSendHandlers s
            let x = func (Runtime.IContext())
            //printfn "The result of the handler is %i" x 
            let x = 1
            let buf = System.BitConverter.GetBytes(x)
            //Runtime.sendMessage "agent" (buf:byte[]) role 
            printfn "I am sending and moving to next state: %i %s and %s " s label role
        else 
            let x = 1 // get the funtion from the handlers!!! 
            //Array.append labelSerialized payloadSerialized      
            let func = Runtime.getFromSendHandlers s
            let x = func (Runtime.IContext())
            //printfn "The result of the handler is %i" x 
            let x = 1
            let buf = System.BitConverter.GetBytes(x)
            Runtime.sendMessage "agent" (buf:byte[]) role 
            printfn "I am sending and moving to next state: %i %s and %s " s label role

    member x.recvLabel role = 
        if x.isMock then 
            //let result = Runtime.receiveMessage "agent" [] role []      
            //let decode = new System.Text.UTF8Encoding() 
            let labelRead = "BYE"
            labelRead
        else 
            let result = Runtime.receiveMessage "agent" [] role []      
            let decode = new System.Text.UTF8Encoding() 
            let labelRead = decode.GetString(result.[0]) 
            labelRead

let rec run (fsm:CFSM) s (runtime:LRuntime) = 
     let nextTr = getNext s fsm
     // check if we next transition is a singleton (send or receive)
     if (nextTr.Count = 1) then 
        let t = nextTr |> Map.toSeq |> Seq.map fst |> Seq.item 0 
        let next = nextTr.Item t 
        match t with 
         |Send (label,role) -> // next transition is select 
            runtime.processSend s role label 
            run fsm next runtime  
         |Recv (label,role)-> // next transition is select 
            runtime.processRecv s role label
            run fsm next runtime 
         |End _ -> 
            //Runtime.stopMessage "agent" |> ignore
            printf "It is over."  
        else // next transition is selection or branching  
            let t = nextTr |> Map.toSeq |> Seq.map fst |> Seq.item 0 
            match t with 
            |Send (_, role) ->
                let x = 1 // get the funtion from the handlers!!! 
                //Array.append labelSerialized payloadSerialized      
                let func = Runtime.getFromSelectorHandlers s
                let x = func (Runtime.StateType())
                let handler = Runtime.getFromSelectHandlers x
                
                // this will configure (register) the selected handlers (expect a Label type as argument, e.g Fib.ADD but can be earsed to obj)
                handler (Runtime.StateType()) |> ignore
                // execute the given handler (that was just registered above) 
                let selected = 
                    nextTr |> Map.toList |> Seq.map fst |> Seq.toList 
                    |> List.filter (fun tr -> match tr with |Send(lbl, _) when lbl=x -> true | _ -> false) 
                    |> List.head 

                runtime.processSend s role x  
                let next = nextTr.Item selected
                run fsm next runtime 
            |Recv (_, role) ->
                printfn "FSM...%A" fsm 
                printfn "Branching..." 
                printfn "Branching...%A"  Runtime.branchHandlers
                let labelRead = runtime.recvLabel role
                let func = Runtime.getFromBranchHandlers labelRead 
                // we need a proper way to give the state here
                func (Runtime.StateType()) |> ignore // similar to the selector case, this call will only register the handlers
                
                printfn "After executing a label %s" labelRead

                printfn "recv handlers are registered: %A" (Runtime.recvHandlers.Item("recv"))
                printfn "send handlers are registered: %A" (Runtime.sendHandlers.Item("send"))

                printfn " handlers are registered: %A" (nextTr)

                
                let selected = 
                    nextTr |> Map.toList |> Seq.map fst |> Seq.toList 
                    |> List.filter (fun tr -> match tr with |Recv(_, lbl) when lbl=labelRead -> true | _ -> false) 
                    |> List.head         
                
                printfn " handlers are registered: %A" (selected)

                //runtime.processRecv s role labelRead
                let next = nextTr.Item selected
                printfn "Next state is: %A" (next)
                run fsm next runtime 

let runtime = LRuntime(true)
let runFromInit cfsm = run cfsm (cfsm.InitState)  runtime

(*
let f = initFsm 1 
        |> addTransition 1 (Transition.Send ("Hello","A")) 2
        |> addTransition 2 (Transition.Recv ("Hello","A")) 3 
        |> addTransition 3 (Transition.Send ("Hello","A")) 4
        |> addTransition 4 (Transition.End ("Hello","A")) -4 

run f (f.InitState)   
*)