module ScribbleGenerativeTypeProvider.CFSMAdapter

open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.TypeGeneration

let getChoiceTransitions index (fsmInstance:ScribbleProtocole.Root []) = 
  let methodName = fsmInstance.[index].Type
  let role = fsmInstance.[index].Partner
  let label = fsmInstance.[index].Label
  let payloadNames = (payloadsToVarNames fsmInstance.[index].Payload)
  let next = fsmInstance.[index].NextState
  let transition = 
      match methodName with 
        | "choice_send" ->  CFSMModel.Transition.Send (role, label, payloadNames)
        | "choice_receive" ->  CFSMModel.Transition.Recv (role, label, payloadNames)
        | "finish" ->  CFSMModel.Transition.End 
        | _ -> failwith "such event is not exepected"
  (transition, next)

let rec convertCFSM cfsm (states) (fsmInstance:ScribbleProtocole.Root []) isIndex = 
  //let lstates = Set.toList states 
  match states with
  | [] -> cfsm
  | h::t -> 
      //cfsm |>CFSM.addTransition h cfsm
      // convertCFSM  h fsmInstance
      let index = 
          if isIndex then h
          else findCurrentIndex  h fsmInstance 
      if index = -1 then 
          convertCFSM cfsm t fsmInstance false 
      else 
          let methodName = fsmInstance.[index].Type
          let role = fsmInstance.[index].Partner
          let label = fsmInstance.[index].Label
          let payloadNames = payloadsToVarNames fsmInstance.[index].Payload
          let next = fsmInstance.[index].NextState

          match methodName  with 
          | "send" | "receive" | "finish" ->
              let transition = 
                  match methodName with 
                  | "send" ->  CFSMModel.Transition.Send (role, label, payloadNames)
                  | "receive" ->  CFSMModel.Transition.Recv (role, label, payloadNames)
                  | "finish" ->  CFSMModel.Transition.End 
              let newcfsm = cfsm |> CFSMModel.addTransition h transition next
              let nextIndex = (findCurrentIndex next fsmInstance)
              let newcfsm1 = 
                  if nextIndex = -1
                  then newcfsm |> CFSMModel.addTransition next CFSMModel.Transition.End -1  
                  else newcfsm
              convertCFSM newcfsm1 t fsmInstance false
          | "choice_receive" | "choice_send" -> 
              let listIndexChoice = findSameCurrent fsmInstance.[index].CurrentState fsmInstance
              //let choiceStates = List.map (fun idx -> fsmInstance.[idx].CurrentState) listIndexChoice
              let newcfsm = 
                  List.fold  (fun ncfsm n->  
                                  let (tr, nxt) = getChoiceTransitions n fsmInstance
                                  CFSMModel.addTransition h tr nxt ncfsm) 
                            cfsm listIndexChoice 

              //let newcfsm = convertCFSM cfsm listIndexChoice fsmInstance true
              //let newcfsm = cfsm |> CFSMop.addTransition h transition next 
              convertCFSM newcfsm t fsmInstance false 

          | _ -> failwith "CFSM transition not supported"