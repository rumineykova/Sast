module ScribbleGenerativeTypeProvider.IO

open System // needed for Exception
open System.IO
open System.Text
open System.Net.Sockets
open System.Threading.Tasks
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open ScribbleGenerativeTypeProvider.Util
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.CommunicationAgents


/// this way of defining failures can be used in an exception raising manner
/// or in a RoP manner if at then of the day we can raise IFailure or RoP on IFailure
type IOFailures =
    | Encoding of string*Exception
    | SerializationPayload of obj*string
    | DeserializationConvertion of (byte [])*string
    interface IFailure with
        member this.Description =
            match this with
            | Encoding (encode,exn) -> 
                sprintf "IOFailures[Encoding] : Impossible to encode( %s ) \-> exception( %s )" encode (exn.Message)
            | SerializationPayload (arg,typing) -> 
                sprintf "IOFailures[SerializationPayload] : Cannot serialize payloads( %A ) to type( %A )" arg typing
            | DeserializationConvertion (arg,typing) -> 
                sprintf "IOFailures[DeserializationConvertion] : Cannot deserialze payloads( %A ) to type( %A )" arg typing

// Helpers to write and read bytes with the help of delims
let toBytes (str : string) =
    try
        let encoder = new UTF8Encoding()
        encoder.GetBytes(str)
    with
    | e -> 
        Encoding (str,e) |> createFailure

// Serialization + Deserialization + DesAsync + DesChoice
// Currently only working for basic types.
// Need to find a better way to handle IO properly!!
let serLabel (label:string) (delim:string) =
    let labelBytes = label |> toBytes
    let delimBytes = delim |> toBytes
    delimBytes |> Array.append labelBytes

let getIntValues (args: Expr list) = 
    args 
    |> List.map (fun arg -> Expr.Coerce(arg,typeof<int>) |> unbox<int []>)
    
(*let runAssertion foo argName argVal= 
    // No Assertion provided
    let runAssrt = 
        if (foo <> "" &&  argName <> "") then 
            Runtime.addArgValueToAssertionDict "agent" argName argVal                            
            Runtime.runFooFunction "agent" foo
        else
            Some true
    match runAssrt with
    | None -> failwith ErrorMsg.incorrectType
    | Some false -> failwith ErrorMsg.assertionInvalid 
    | Some true -> ()
*)

let runAssertion assrtFun argNames argVals = 
    let evalAssrt = 
        if assrtFun <> "" then 
            (argNames, argVals)
            ||> List.map2(fun argName rcv -> 
                Runtime.addArgValueToAssertionDict "agent" argName rcv )
            |> ignore
            Runtime.runFooFunction "agent" assrtFun
        else
            Some true
    match evalAssrt with
    | None -> failwith ErrorMsg.incorrectType
    | Some false -> failwith ErrorMsg.assertionInvalid 
    | Some true -> ()

let serPayloads (args:Expr list) (listTypes:string list) 
                (payloadDelim:string) (endDelim:string) 
                (argsNames:string list) assrtFun (payloadNames: string list) =
    let mutable assArgIndex = 0
    let listPayloads =  
        args 
        |> List.mapi (fun i arg -> 
            let currentType = listTypes.[i]
            let argName =  
                if (assrtFun <> "" 
                    && assArgIndex < argsNames.Length 
                    && argsNames.[assArgIndex].Equals(payloadNames.[i])) 
                then
                    assArgIndex <- assArgIndex + 1
                    argsNames.[assArgIndex-1]
                else
                   ""
            match currentType with
            |"System.String" | "System.Char" -> 
                <@  
                    let spliced = %%(Expr.Coerce(arg,typeof<obj>))
                    let names = if argName = "" then [] else [argName]
                    runAssertion assrtFun [argName] [spliced] 
                    try
                        Type.GetType("System.Text.UTF8Encoding")
                            .GetMethod("GetBytes", [|Type.GetType(currentType)|])
                            .Invoke(new UTF8Encoding(), [|spliced|]) |> unbox<byte []> 
                    with
                    | _ -> 
                        Debug.print "Failed to serialize 1" ""
                        SerializationPayload (spliced,currentType) |> createFailure        
                @>
             |  _ -> 
                <@ 
                    let spliced = %%(Expr.Coerce(arg,typeof<obj>))
                    let names = if argName = "" then [] else [argName]
                    runAssertion assrtFun [argName] [spliced] 

                    try
                        Type.GetType("System.BitConverter")
                            .GetMethod("GetBytes",[|Type.GetType(currentType)|])
                            .Invoke(null,[|spliced|] ) |> unbox<byte []> 
                    with
                    | _ -> 
                        Debug.print "Failed to serialize 2" ""
                        SerializationPayload (spliced,currentType) |> createFailure   
                @> 
           )  

    let payloadsNum = listPayloads.Length
    let listDelims = 
        [ for i in 0..(payloadsNum-1) do
            if ( i = (payloadsNum-1) ) 
            then yield <@ (endDelim |> toBytes) @>
            else yield <@ (payloadDelim |> toBytes) @> ]

    listPayloads 
    |> List.fold2 (fun acc f1 f2 -> 
        <@  let f1 = %f1
            let f2 = %f2
            let acc = %acc
            Array.append (Array.append acc f1) f2 
        @>) 
        <@ [||] @> <| listDelims



let serialize (label:string) (args:Expr list) 
        (listTypes:string list) (payloadDelim:string) 
        (endDelim:string) (labelDelim:string) 
        argsNames foo payloadNames =

    let labelSerialized = <@ serLabel label labelDelim @>                    
    <@  
        let payloadSerialized = 
            %(serPayloads args listTypes payloadDelim 
                endDelim argsNames foo payloadNames)
        let labelSerialized  = %(labelSerialized) 
        Array.append labelSerialized payloadSerialized 
    @>        

let convert (arrayList:byte[] list) (elemTypelist:string list) =
    let rec aux (arrList:byte[] list) (elemList:string list) (acc:obj list) =
        match arrList with
        |[] -> List.rev acc
        |hd::tl ->  
            let sub = elemList.Head.Split('.')
            let typing = sub.[sub.Length-1]
            try
                let mymethod = 
                    Type.GetType("System.BitConverter")
                        .GetMethod("To"+typing,[|typeof<byte []>;typeof<int>|])
                
                let invoke = mymethod.Invoke(null,[|box hd;box 0|])
                aux tl (elemList.Tail) (invoke::acc)
            with
            | e -> 
                DeserializationConvertion (hd,typing) |> createFailure                        
    aux arrayList elemTypelist []
  

let deserialize (messages: _ list) (role:string) (args: Expr list) 
        (listTypes:string list) (argsNames:string list) foo  =
    let buffer = [for elem in args do yield Expr.Coerce(elem,typeof<ISetResult>)]
    <@  
        let result = Runtime.receiveMessage "agent" messages role [listTypes]
        //printfn " deserialize Normal : 
        // %A || Role : %A || listTypes : %A" messages role listTypes
        let received = convert (result.Tail) listTypes 
        runAssertion foo argsNames received
        let received = List.toSeq received  
        Runtime.setResults received 
            (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult [])     
    @>

let deserializeNew (messages: _ list) (role:string) (args: Expr list) 
        (listTypes:string list) (argsNames:string list) foo  =
    let buffer = [for elem in args do yield Expr.Coerce(elem,typeof<ISetResult>)]
    let x = 
        <@  
            let result = Runtime.receiveMessage "agent" messages role [listTypes]
            //printfn " deserialize Normal : 
            // %A || Role : %A || listTypes : %A" messages role listTypes
            let received = convert (result.Tail) listTypes 
            runAssertion foo argsNames received
            //let received = List.toSeq received  
            //Runtime.setResults received 
            //(%%(Expr.NewArray(typeof<System.Int32>, received))) 
            received.Head
        @>
    Expr.Coerce(x, typeof<System.Int32>)

let deserializeAsync (messages: _ list) (role:string) 
        (args: Expr list) (listTypes:string list)  argsNames foo =  
    
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]              
    <@ 
        let work = 
            async {            
                let! res = 
                    Runtime.receiveMessageAsync "agent" 
                        messages role listTypes 
                let received = (res.Tail |> convert <| listTypes )
                runAssertion foo argsNames received
                let received = received |> List.toSeq          
                Runtime.setResults received 
                    (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
            }
        Async.Start(work)
     @>


let deserializeChoice (args: Expr list) (listTypes:string list) argsNames foo =
    let buffer = [for elem in args do
                    yield Expr.Coerce(elem,typeof<ISetResult>) ]
    <@ 
        let result = Runtime.receiveChoice "agent" 
        Debug.print  "List types" listTypes
        let received = (result |> convert <| listTypes ) 
        runAssertion foo argsNames received
        let received = received |> List.toSeq
        let test = (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
        let completed = test |> Array.map(fun t -> t.GetTask())
        Debug.print  "Receive a choice" (received,test,completed)
        Runtime.setResults received 
            (%%(Expr.NewArray(typeof<ISetResult>, buffer)):ISetResult []) 
    @>