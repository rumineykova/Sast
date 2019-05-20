module ScribbleGenerativeTypeProvider.ScribbleParser

//#r "./packages/FSharp.Data/lib/net40/FSharp.Data.dll"
//#r "./packages/FParsec/lib/net40-client/FParsecCS.dll"
//#r "./packages/FParsec/lib/net40-client/FParsec.dll"
open FSharp.Data
open System
open System.Text.RegularExpressions
open FParsec

let printListJson (aList:string list) =
    let length = aList.Length
    List.fold
        (fun (state,index) (elem:string) ->
            (   if index < length then
                    sprintf """%s"%s",""" state elem
                else
                    sprintf """%s"%s" """ state elem
             ,index+1)
        ) ("[",1) aList
    |> fun (state,_) -> state + "]"



type Current = Current of int
type Role = Role of string
type Partner = Partner of string
type Label = Label of string
type Payload = Payload of string List
type EventType = EventType of string
type Next = Next of int

type Transition =
    {
        Current     : Current
        Role        : Role
        Partner     : Partner
        Label       : Label
        Payload     : Payload     
        EventType   : EventType
        Next        : Next
    }
    member this.Stringify() =
        let (Current current)       = this.Current
        let (Role role)             = this.Role
        let (Partner partner)       = this.Partner
        let (Label label)           = this.Label
        let (Payload payload)       = this.Payload     
        let (EventType eventType)   = this.EventType
        let (Next next)             = this.Next

        sprintf
            """{ "currentState": %i , "localRole":"%s" , "partner":"%s" , "label":"%s" , "payload": %s , "type":"%s" , "nextState":%i  } """        
            current
            role
            partner
            label
            (if payload.Length = 1 && payload.[0] = "" then
                printListJson []
             else
                printListJson payload
            )
            eventType
            next         

type StateMachine =
    | Transitions of Transition list
    member this.Stringify() =
        let (Transitions transitionList) = this
        let length = transitionList.Length
        List.fold
            (fun (state,index) (transition:Transition) ->
                (if index < length then
                    state + (transition.Stringify()) + ",\n"
                 else
                    state + (transition.Stringify())                    
                , index + 1)
            ) ("[",1) transitionList
        |> fun (state,_) -> state + "]"

module parserHelper =
    let brackets = ('{','}')
    let squareBrackets = ('[',']')
    let quotes = ('\"','\"')
    let str_ws s = spaces >>. pstring s .>> spaces
    let char_ws c = pchar c .>> spaces
    let anyCharsTill pEnd = manyCharsTill anyChar pEnd
    let anyCharsTillApply pEnd f = manyCharsTillApply anyChar pEnd f
    let quoted quote parser = 
        pchar (quote |> fst) 
        .>> spaces >>. parser .>> spaces 
        .>> pchar (quote |> snd) 
    let line:Parser<string,unit> = anyCharsTill newline
    let restOfLineAfter str = str_ws str >>. line
    let startUpUseless:Parser<_,unit> = 
        pstring "compound = true;" 
        |> anyCharsTill
        // >>. skipNewline 
         
    let current:Parser<_,unit> = 
        spaces 
        >>. quoted quotes pint32 .>> spaces 
        |>> Current
    let next:Parser<_,unit> = 
        spaces 
        >>. quoted quotes pint32 .>> spaces 
        |>> Next
    
    let partnerEvent:Parser<_,unit> =
        str_ws "label"
        >>. pstring "=" >>. spaces
        >>. pchar '\"'
        >>. (anyCharsTillApply (pchar '!' <|> pchar '?') (fun str event -> (str,event)))
        |>> fun (str,event) -> 
                match event with
                | '!' -> 
                    Partner(str),EventType("send")
                | '?' ->
                    Partner(str),EventType("receive")                
                | _ ->
                    failwith "This case can never happen, if these two weren't here the flow would
                    have been broken earlier!!"

    let label:Parser<_,unit> = 
        spaces
        >>. (anyCharsTill (pchar '('))
        |>> Label
    let payload:Parser<_,unit> =
        let singlePayload =
            spaces
            >>. manyChars (noneOf [',';')'])
        spaces
        >>. between 
                spaces 
                (pstring ")\"" >>. spaces >>. pstring "];" >>. spaces) 
                (sepBy singlePayload (pstring ",")) 
        |>> Payload        
   
    let transition role currentState =
        parse{
            let! _ = pstring "->"
            let! nextState = next
            let! _ = pstring "["
            let! partner,eventType = partnerEvent
            let! label = label
            let! payload = payload
            return 
                {
                    Current     = currentState
                    Role        = Role role
                    Partner     = partner
                    Label       = label
                    Payload     = payload     
                    EventType   = eventType
                    Next        = nextState
                } |> Some
        }
    let skipLabelInfoLine:Parser<Transition option,unit> =
         parse{
            let! _ = pstring "[" .>> spaces
            let! _ = manyCharsTill anyChar (pstring "];")
            let! _ = spaces
            return None
        }
    let transitionOrSkipping role =
        parse{
            let! _ = spaces
            let! currentState = current .>> spaces
            return! transition role currentState <|> skipLabelInfoLine
        }
    let transitions role = 
        parse{
            let! _ = startUpUseless
            do! spaces
            let! list = (many (transitionOrSkipping role)) 
            printfn "%A" list
            return 
                list 
                |> List.filter Option.isSome 
                |> List.map Option.get
                |> Transitions
        }
module Parsing = 
    open parserHelper
    type ScribbleAPI = FSharp.Data.JsonProvider<""" { "code":"Code", "proto":"global protocol", "role":"local role" } """>
    type DiGraph = FSharp.Data.JsonProvider<""" {"result":"value"} """>
    type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":0 , "localRole":"StringLocalRole" , "partner":"StringPartner" , "label":"StringLabel" , "payload":["StringTypes"] , "type":"EventType" , "nextState":0  } ] """>
                        

    let isCurrentChoice (fsm:ScribbleProtocole.Root []) (index:int) =
        let current = fsm.[index].CurrentState
        let mutable size = 0 
        for elem in fsm do
            if elem.CurrentState = current then
                size <- size + 1
        (size>1)

    let modifyAllChoice (fsm:ScribbleProtocole.Root []) =
        let mutable newArray = [||] 
        for i in 0..(fsm.Length-1) do
            let elem = fsm.[i]
            if elem.Type = "receive" && (isCurrentChoice fsm i) then
                let choice_type = if elem.Type = "receive" then "choice_receive" else "choice_send"
                let newElem = ScribbleProtocole.Root(elem.CurrentState,elem.LocalRole,elem.Partner,elem.Label,elem.Payload, choice_type ,elem.NextState)
                newArray <- Array.append newArray [|newElem|]            
            else
            newArray <- Array.append newArray [|elem|]
        newArray


    let getArrayJson (response:string) json =
        //let s = DiGraph.Parse(response)
        //let s0 = s.Result
        let s0 = response
        match Regex.IsMatch(s0,"java\\.lang\\.NullPointerException") with
        |true ->  None
        |false ->
            let role = ScribbleAPI.Parse(json)
            let test = run (transitions (role.Role) ) s0
            match test with
            | Failure (error,_,_) -> 
                printfn "%s" error
                None
            | Success (res,_,_) ->
                printfn "%s" (res.Stringify())
                let res = ScribbleProtocole.Parse(res.Stringify())
                let newRes = modifyAllChoice res
                let finalRes =
                    [ for tr in newRes do
                        yield
                            {
                                Current     = tr.CurrentState |> Current
                                Role        = tr.LocalRole |> Role
                                Partner     = tr.Partner |> Partner
                                Label       = tr.Label |> Label
                                Payload     = tr.Payload |> List.ofArray |> Payload
                                EventType   = tr.Type |> EventType
                                Next        = tr.NextState |> Next
                            }  
                    ] |> Transitions
                Some (finalRes.Stringify())
                
    let getFSMJson (json:string) = getArrayJson json

