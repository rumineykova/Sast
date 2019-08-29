module ScribbleGenerativeTypeProvider.AsstScribbleParser


open System.Text.RegularExpressions
open FParsec
open AssertionParsing
open AssertionParsing.FuncGenerator


let nextNumber = 

    let counter = ref 0
    fun () -> 
        counter.Value <- !counter + 1
        !counter

let printListJson (aList:list<string*string>) =
    let length = aList.Length
    List.fold
        (fun (state,index) ((varName, varType):string*string) ->
            (   if index < length then
                    sprintf """%s{"varName":"%s", "varType":"%s" },""" state varName varType
                else
                    sprintf """%s{"varName":"%s", "varType":"%s"}""" state varName varType
             ,index+1)
        ) ("[",1) aList
    |> fun (state,_) -> state + "]"

type Current = Current of int
type Role = Role of string
type Partner = Partner of string
type Label = Label of string
type VarName = VarName of string
type VarType = VarType of string
type TrPayload = TrPayload of List<string*string>
type EventType = EventType of string
type Next = Next of int
type Assertion = Assertion of string
type Inferred = Inferred of string

type Transition =
    {
        Current     : Current
        Role        : Role
        Partner     : Partner
        Label       : Label
        TrPayload   : TrPayload    
        Assertion   : Assertion
        Inferred    : Inferred
        EventType   : EventType
        Next        : Next
    }
    member this.Stringify() =
        let (Current current)       = this.Current
        let (Role role)             = this.Role
        let (Partner partner)       = this.Partner
        let (Label label)           = this.Label
        let (TrPayload payload)     = this.TrPayload     
        let (EventType eventType)   = this.EventType
        let (Next next)             = this.Next
        let (Assertion assertion)   = this.Assertion
        let (Inferred inferred)     = this.Inferred

        sprintf
            """{ "currentState": %i , "localRole":"%s" , "partner":"%s" , "label":"%s" , "payload": %s , "assertion": "%s", "inferred": "%s", "type":"%s" , "nextState":%i  } """        
            current
            role
            partner
            label
            (if payload.Length = 1 && payload.[0] = ("", "") then
                printListJson []
             else
                printListJson payload
            )
            assertion
            inferred
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
        >>. skipNewline 
    let isUnderscore c = c='_'                                          
    let isVar c = isLetter c || isDigit c || c='_'
    //let dummy = many1Satisfy2L isUnderscore isVar "identifier" .>> spaces          
    let varParser:Parser<_, unit> = 
        many1Satisfy isVar .>> spaces
    let expr:Parser<_, unit> = 
        let normalChar 
            = satisfy (fun c -> c <> ';' && c<>'\\' && c<>'\"' && c<>'}')
        (manyChars normalChar) 

    let assrtExpr: Parser<_, unit> = 
        //let assrt  = 
        manyCharsTill anyChar (pstring "<>")
        //(attempt (pstring "True" <|> pstring "False" <|> assrt ))

    // pstring "\"" .>> spaces .>> pstring "];" >>. spaces

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
        >>. (anyCharsTillApply (attempt (pstring "!!" <|> pstring "!" <|> pstring "??" <|> pstring "?")) (fun str event -> (str,event)))
        |>> fun (str,event) -> 
                match event with
                | "!!" -> 
                    Partner(str),EventType("request")
                | "??" -> 
                    Partner(str),EventType("accept")
                | "!" -> 
                    Partner(str),EventType("send")
                | "?" ->
                    Partner(str),EventType("receive")                
                | _ ->
                    failwith "This case can never happen, if these two weren't here the flow would
                    have been broken earlier!!"

    let label:Parser<_,unit> = 
        spaces
        >>. (anyCharsTill (pchar '('))
        |>> Label
    
    let payload:Parser<_,unit> =
        //let varPayload = spaces >>. manyChars (noneOf [',';')';':'])
        //let typePayload = spaces >>. manyChars (noneOf [',';')'])
        //let singlePayload = pipe3 varPayload (pstring ":") (spaces >>. varPayload) (fun id _ str ->  (id, str))
        let varName = manyChars (noneOf [',';')'; ':']) 
        let unitType = ((pstring "_" >>. varName ) <|> (pstring "Unit")) |>> (fun x -> "")
        let dummyVars = (pstring "_" >>. varName ) //|>> (fun x -> "")
        let singlePayload = 
             attempt
                (pipe4 (spaces >>.  varName) (pstring ":") spaces (unitType <|> varName)
                        (fun name _ _ varType -> 
                                if (varType<>"") 
                                        then  (sprintf "%s" name, sprintf "%s" varType) 
                                else sprintf "", ""))
                <|> ((spaces >>. (unitType <|> varName)) |>> 
                     (fun varType -> 
                        if (varType<>"") then  (sprintf "_dummy%i" (nextNumber()), sprintf "%s" varType) 
                        else sprintf "", ""))

        spaces  >>. (sepBy singlePayload (pstring ",")) .>> (pstring ")")
        |>> TrPayload           
    //(pstring ")\"" >>. spaces >>. pstring "];" >>. spaces)    
    //type AssertionInferred = AssertionInferred of Assertion*Inferred

    let assertionPlus:Parser<_,unit> =     
        let varName = manyChars (noneOf [',';')'; ':']) 
        
        (*let singleRecord = pipe3 (spaces >>.varName) (pstring ":") (spaces >>. expr) 
                                 (fun name _ varExpr -> (sprintf "%s" name, sprintf "%s" varExpr))
        let inferredAll = spaces >>. (sepBy singleRecord (pstring ",")) .>> (pstring "}")
        //|>> Inferred*)
        let infFragment = spaces >>. (between (pstring "(")  (pstring ")") expr)
        let statevar = pstring "<>" 
        let endOfPayload = pstring "\"" >>. spaces >>. pstring "];" >>. spaces
        //let assertion = AssertionParser.xparser p //|> Visitors.getStringRepr
        (*
        let assrt = (endOfPayload |>> (fun x -> Assertion(""), Inferred("")))
                    // <|> ((((spaces >>. (between (pstring "{")  (pstring "}") expr))  .>> endOfPayload)) |>> Inferred)
                    <|> ((infFragment  .>> endOfPayload) |>> (fun x -> Assertion(""), Inferred(x)))
                    <|> pipe2  expr //(pstring "@" >>. (between (pstring "\\\"")  (pstring "\\\"") expr))  
                               ((endOfPayload |>> fun x -> "") <|> ((infFragment .>> endOfPayload) |> (fun x -> x))) 
                               (fun ass maybeInferred -> 
                                    if (maybeInferred <> "") 
                                    then Assertion(ass), Inferred(maybeInferred) 
                                    else Assertion(ass), Inferred("")
                                )
        *)
        let assrt = (assrtExpr .>> endOfPayload) 
                    |>> (fun ass -> Assertion(ass), Inferred(""))
        assrt
      // assrt |>> AssertionInferred

   (* let inferred:Parser<_, unit> = 
        let varName = manyChars (noneOf [',';')'; ':']) 
        let singleRecord = pipe3 (spaces >>.varName) (pstring ":") (spaces >>. expr) 
                                 (fun name _ varExpr -> (sprintf "%s" name, sprintf "%s" varExpr))
        spaces >>. (sepBy singleRecord (pstring ",")) .>> (pstring "}")
        |>> Inferred
        // between (pstring "{") (pstring "}")  

    endOfPayload |>> 
    inferred <|> assertion <|> assertion inferred *)
    (*let inferred: Parser<_,unit> = 
        //let endOfPayload = pstring "\"" >>. spaces >>. pstring "];" >>. spaces
        endOfPayload |>> fun x -> [sprintf "", "" ]
        |>> Inferred *)

    let transition role currentState =
        parse {
            let! _ = pstring "->"
            let! nextState = next
            let! _ = pstring "["
            let! partner,eventType = partnerEvent
            let! label = label
            let! payload = payload
            let! (assertion, inferred) = assertionPlus
            return 
                {
                    Current     = currentState
                    Role        = Role role
                    Partner     = partner
                    Label       = label
                    TrPayload   = payload
                    Assertion   = assertion
                    Inferred    = inferred      
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
    type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":0 , "localRole":"StringLocalRole" , "partner":"StringPartner" , "label":"StringLabel" , "payload":[{"varName":"someZ", "varType":"someType"}] , "assertion":"expression", "inferred":"bla", "type":"EventType" , "nextState":0  } ] """>
                        

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
            if (elem.Type = "receive" || elem.Type = "send") && (isCurrentChoice fsm i) then
                let choice_type = if elem.Type = "receive" then "choice_receive" else "choice_send"
                let newElem = ScribbleProtocole.Root(elem.CurrentState,elem.LocalRole,elem.Partner,elem.Label,elem.Payload, elem.Assertion, elem.Inferred, choice_type,elem.NextState)
                newArray <- Array.append newArray [|newElem|]            
            else
            newArray <- Array.append newArray [|elem|]
        newArray

    let transformPayloadToJson(payload: List<string*string>) = 
        [for elem in payload do
            yield ScribbleProtocole.Payload((fst elem), (snd elem))
        ]
    
    let transformJsonToPayload (typesMap:Map<string, string>) (payload: List<ScribbleProtocole.Payload>) = 
        [for elem in payload do
            yield (elem.VarName, typesMap.Item elem.VarType)
        ]

    let UnitType = "Unit" 
    let adjustTypesMap (typesMap: Map<string, string>) = 
        if not (typesMap.ContainsKey UnitType) then 
            typesMap.Add(UnitType, UnitType)
        else typesMap

    let getArrayJson (response:string) (config:string) (initTypesMap: Map<string, string>) =
        //let s = DiGraph.Parse(response)
        //let s0 = s.Result
        let typesMap =  adjustTypesMap initTypesMap 
        let s0 = response
        match Regex.IsMatch(s0,"java\\.lang\\.NullPointerException") with
        |true ->  None
        |false ->
            let role = ScribbleAPI.Parse(config)
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
                                TrPayload   = tr.Payload |> List.ofArray |> transformJsonToPayload typesMap |> TrPayload
                                Assertion   = tr.Assertion |> genLambdaFromStr |> Assertion
                                Inferred    = tr.Inferred |> Inferred
                                EventType   = tr.Type |> EventType
                                Next        = tr.NextState |> Next
                            }  
                    ] |> Transitions
                Some (finalRes.Stringify())
    // (typesMap: Map<string, string>)
    let getFSMJson (parsedScribble:string) = getArrayJson parsedScribble 