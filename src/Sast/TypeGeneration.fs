module ScribbleGenerativeTypeProvider.TypeGeneration

// Outside namespaces and modules
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
open System.Threading.Tasks
open System.Text
open System.Collections
open FSharp.Quotations.Evaluator
open AssertionParsing.InferredVarsParser

// ScribbleProvider specific namespaces and modules
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.CommunicationAgents
open ScribbleGenerativeTypeProvider.IO
open ScribbleGenerativeTypeProvider.RefinementTypes
open ScribbleGenerativeTypeProvider.Util.ListHelpers
open ScribbleGenerativeTypeProvider.Util

(******************* TYPE PROVIDER'S HELPERS *******************)

// CREATING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS
let internal createProvidedType assembly name = 
    ProvidedTypeDefinition(assembly, ns, name, Some typeof<obj>, IsErased=true)

let internal createProvidedIncludedType name = 
    ProvidedTypeDefinition(name,Some baseType, IsErased=true)

let internal createProvidedIncludedTypeChoice typing name =
    ProvidedTypeDefinition(name, Some typing , IsErased=true)

let internal createMethodType name param typing expression =
    ProvidedMethod( name, param, typing, InvokeCode = (fun args -> expression ))

let internal createPropertyType name typing expression =
    ProvidedProperty(name , typing , 
        IsStatic = true, 
        GetterCode = (fun args -> expression))

let internal createCstor param expression = 
    ProvidedConstructor( parameters = param, InvokeCode = (fun args -> expression ))


// ADDING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS TO THE ASSEMBLY AND AS MEMBERS OF THE TYPE PROVIDER
(*let internal addProvidedTypeToAssembly (providedType:ProvidedTypeDefinition)=
    asm.AddTypes([providedType])
    providedType*)

let internal addIncludedTypeToProvidedType nestedTypeToAdd (providedType:ProvidedTypeDefinition) =
    providedType.AddMembers(nestedTypeToAdd)
    providedType

let internal addMethod methodType (providedType:ProvidedTypeDefinition) = 
    providedType.AddMember methodType
    providedType    

let internal addProperty propertyToAdd (providedType:ProvidedTypeDefinition) =
    providedType.AddMember(propertyToAdd)
    providedType

let internal addCstor cstorToAdd (providedType:ProvidedTypeDefinition) =
    providedType.AddMember(cstorToAdd)
    providedType

let internal addMember (memberInfo:#MemberInfo) (providedType:ProvidedTypeDefinition) = 
    providedType.AddMember(memberInfo)
    providedType

let internal addMembers (membersInfo:#MemberInfo list) (providedType:ProvidedTypeDefinition) = 
    providedType.AddMembers(membersInfo)
    providedType

let ChoiceType = "ScribbleGenerativeTypeProvider.TypeChoices+Choice"

(******************* TYPE PROVIDER'S FUNCTIONS *******************)
let internal findCurrentIndex current (fsmInstance:ScribbleProtocole.Root []) = // gerer les cas
    let fsm = Array.toList fsmInstance
    let rec aux (acc:ScribbleProtocole.Root list) count =
        match acc with
            |[] -> -1
            |hd::tl -> 
                if hd.CurrentState = current then count
                else aux tl (count+1) 
    aux fsm 0

let internal findSameCurrent currentState 
        (fsmInstance:ScribbleProtocole.Root []) =
    let mutable list = []
    let mutable inc = 0
    for event in fsmInstance do
        if event.CurrentState = currentState then
            list <- inc::list
        inc <- inc+1
    list

/// Test this function by changing t with t+1 and see the mistakes happen  
/// -> generate the useless ProvidedTypeDefinition and throw exception cause it
/// is not added to the assembly.
let rec findProvidedType 
        (providedList:ProvidedTypeDefinition list) stateValue =
    match providedList with
    |[] -> // Useless case, t is useless but we need this case due to pattern matching exhaustiveness.
            "CodingMistake" |> createProvidedIncludedType 
    |[a] -> 
        let t = ref 0
        if System.Int32.TryParse(a.Name.Replace("State",""), t) 
            && (!t)=stateValue 
            then a
            else findProvidedType [] stateValue    
    |hd::tl -> 
        let t = ref 0
        if System.Int32.TryParse(hd.Name.Replace("State",""), t) 
            && (!t)=stateValue 
            then hd
            else findProvidedType tl stateValue      

let isVarInferred varName 
        (inferredDict:Generic.IDictionary<string, string> option) = 
    //let myDict = parseInfVars inferred
    match inferredDict with 
    | Some vars -> (vars.ContainsKey(varName))
    | None -> false

let internal createProvidedParameters (event : ScribbleProtocole.Root)  =
    let generic = typeof<Buf<_>>.GetGenericTypeDefinition() 
    let payload = event.Payload
    let mutable n = 0
    //let myDict = parseInfVars event.Inferred 
    [for param in payload do
        n <- n+1
        if param.VarType.Contains("[]") then
            let nameParam = param.VarType.Replace("[]","")
            let typing = System.Type.GetType(nameParam)
            let arrType = typing.MakeArrayType()
            let genType = generic.MakeGenericType(arrType)
            yield ProvidedParameter((param.VarName), genType) 
        else
            // Currently this Case is throwing an error due to the fact that 
            // The type returned by the scribble API is not an F# type
            // This case should be handled properly
            let genType = generic.MakeGenericType(
                            System.Type.GetType(param.VarType))
            yield ProvidedParameter((param.VarName),genType) 
    ]

(******************* START CONVERT FUNCTIONS FOR PAYLOADS*******************)

let internal payloadsToTypes 
        (payloads: System.Collections.Generic.IEnumerable<ScribbleProtocole.Payload>) =
    [for elem in payloads do
        yield elem.VarType ]


let internal payloadsToVarNames (payloads:ScribbleProtocole.Payload []) =
    [for i in 0..(payloads.Length-1) do
        yield payloads.[i].VarName
    ]

let internal payloadsToProvidedList 
        (payloads:ScribbleProtocole.Payload []) inferred = 
    let infDict = parseInfVars inferred
    [for i in 0 ..(payloads.Length-1) do 
            if (not (isVarInferred payloads.[i].VarName infDict)) then 
                yield ProvidedParameter(
                    (payloads.[i].VarName),
                    System.Type.GetType(payloads.[i].VarType))
    ]
(******************* END CONVERT FUNCTIONS FOR PAYLOADS*******************)

(******************* START AUX FUNCTIONS*******************)
let internal contains (aSet:Set<'a>) x = 
    Set.exists ((=) x) aSet

let internal stateSet (fsmInstance:ScribbleProtocole.Root []) =
    let firstState = fsmInstance.[0].CurrentState
    let mutable setSeen = Set.empty
    for event in fsmInstance do
        if (not(contains setSeen event.CurrentState) 
            || not(contains setSeen event.NextState)) 
            then
                setSeen <- setSeen.Add(event.CurrentState)
                setSeen <- setSeen.Add(event.NextState)
    (setSeen.Count,setSeen,firstState)

let internal getAllChoiceLabels (indexList : int list) 
    (fsmInstance:ScribbleProtocole.Root []) =
        let rec aux list acc =
            match list with
            |[] -> acc
            |hd::tl -> 
                Debug.print  "getAllChoiceLabels : I run" ""
                let label = fsmInstance.[hd].Label
                let labelDelim,_,_ = getDelims label
                let labelBytes = label |> serLabel <| (labelDelim.Head) 
                let typing = fsmInstance.[hd].Payload |> List.ofArray
                aux tl ((labelBytes,typing)::acc) 
        in aux indexList []

let internal getAllChoiceLabelString (indexList : int list) 
        (fsmInstance:ScribbleProtocole.Root []) =
    let rec aux list acc =
        match list with
            |[] -> acc
            |hd::tl -> let labelBytes = fsmInstance.[hd].Label 
                       aux tl (labelBytes::acc) 
    in aux indexList []
(******************* END AUX FUNCTIONS*******************)

(******************* START DOC GENERATION FUNCTIONS*******************)
let getDocForChoice indexList fsmInstance=  
    let sb = new System.Text.StringBuilder()
    sb.Append("<summary> When branching here, you will have to type pattern match on the following types :")
    |> ignore 
    
    (indexList |> getAllChoiceLabelString <| fsmInstance)
    |> List.iter(fun message -> 
        sb.Append ("<para> - " + message + "</para>" ) |> ignore) 
    |> ignore
    sb.Append("</summary>") |> ignore
    sb.ToString()

let getAssertionDoc assertion inferred = 
    if (assertion <> "" || inferred <> "") then 
        let sb = new System.Text.StringBuilder()
        sb.Append("<summary> Method arguments should satisfy the following constraint:") |> ignore
        if (assertion <> "") then 
            sb.Append ("<para>" + assertion.Replace(">", "&gt;").Replace("<","&lt;") + "</para>" ) |>ignore
        if (inferred <> "") then sb.Append(("<para>" + inferred + "</para>" )) |> ignore
        sb.Append("</summary>") |>ignore
        sb.ToString()
    else ""    

(******************* END DOC GENERATION FUNCTIONS*******************)

(******************* START GENERATE SIMPLE TYPES*******************)
let internal makeRoleList (fsmInstance:ScribbleProtocole.Root []) =
    let mutable setSeen = Set.empty
    [yield fsmInstance.[0].LocalRole
     for event in fsmInstance do
        if not(setSeen |> contains <| event.Partner) then
            setSeen <- setSeen.Add(event.Partner)
            yield event.Partner] 

let internal makeRoleTypes (fsmInstance:ScribbleProtocole.Root []) = 
    let mutable localRoles = [fsmInstance.[0].LocalRole]
    let mutable listeType = []
    let ctor = <@@ () @@> |> createCstor []
    let t = 
        fsmInstance.[0].LocalRole 
        |> createProvidedIncludedType 
        |> addCstor ctor
    let t = t |> addProperty (Expr.NewObject(ctor,[]) 
              |> createPropertyType "instance" t)

    t.HideObjectMethods <- true
    listeType <- t::listeType
    let mutable mapping = 
        Map.empty<_,ProvidedTypeDefinition>.Add(fsmInstance.[0].LocalRole, t)

    for event in fsmInstance do
        if not(containsRole localRoles event.Partner) then
            let ctor = ( <@@ () @@> |> createCstor [])
            let t = event.Partner 
                    |> createProvidedIncludedType
                    |> addCstor ctor
                        
            let t = t |> addProperty (Expr.NewObject(ctor, []) 
                      |> createPropertyType "instance" t)
            t.HideObjectMethods <- true                                                                     
            mapping <- mapping.Add(event.Partner,t)
            localRoles <- event.Partner::localRoles
            listeType <- t::listeType
    (mapping,listeType)

let internal makeStateTypeBase (n:int) (s:string) = 
    let ty = 
        (s + string n) 
        |> createProvidedIncludedType
        |> addCstor (<@@ s+ string n @@> |> createCstor [])
    ty.HideObjectMethods <- true
    ty

let internal makeStateType (n:int) = makeStateTypeBase n "State"

(*******************END GENERATE SIMPLE TYPES*******************)

(******************* START GENERATE STATE TYPES*******************)

let inlineAssertion assertion  = 
        if ((assertion <> "fun expression -> expression") 
            && (assertion <> ""))  
        then
            let index = Runtime.getAssertionIndex "agent" 
            let assertion = RefinementTypes.createFnRule index assertion
            let elem = assertion |> fst 
            Runtime.addToAssertionDict "agent" elem
            snd assertion 
        else 
            "",[]

let rec assertionExprToCachingExpr (node:AssertionParsing.Expr) = 
    match node with
    | AssertionParsing.Expr.Ident(identifier) -> 
        <@@ Runtime.getFromCache "cache" identifier @@>
    | AssertionParsing.Expr.Literal(AssertionParsing.Bool(value)) -> 
        Quotations.Expr.Value(value)
    | AssertionParsing.Expr.Literal(AssertionParsing.IntC(value)) -> 
        Quotations.Expr.Value(value)
    | AssertionParsing.Expr.Arithmetic(left, op, right) -> 
        let leftExpr = assertionExprToCachingExpr left
        let rightExpr = assertionExprToCachingExpr right 
        Quotations.Expr.Applications(op.ToExpr(), [[leftExpr]; [rightExpr]]) 

let mergeGivenAndInferredVarsOnSend (payloads:string list) 
        (inferred:Generic.IDictionary<string, string>) 
        (buffers:Expr list) = 
    if (inferred.Count = 0) then buffers
    else 
        let mutable index = 0
        payloads |> List.map (fun elem -> 
            if (inferred.ContainsKey(elem)) 
            then 
                let inferredExpr = inferred.Item elem
                let assertionExpr = 
                    AssertionParsing.FuncGenerator.parseAssertionExpr inferredExpr
                assertionExprToCachingExpr assertionExpr

            else 
                let givenExpr = buffers.Item index
                index <- index + 1
                givenExpr)

let getExprCachingOnReceive payloadTypes payload buffers exprDes exprState = 
    let cachingSupported = 
        payloadTypes 
        |> List.filter (fun x -> x <> "System.Int32") 
        |> List.length |> (fun x -> x=0) 
    
    if (cachingSupported=true) then 
        let payloadNames = (payloadsToVarNames payload)
        let addToCacheExpr = 
            <@@ let myValues:Buf<int> [] = 
                    (%%(Expr.NewArray(typeof<Buf<int>>, buffers)):Buf<int> []) 
                Runtime.addVarsBufsToCache "cache" payloadNames myValues @@>

        let exprDes = Expr.Sequential(exprDes, addToCacheExpr)                                                            
        let cachePrintExpr = <@@ Runtime.printCount "cache" @@>
        let exprDes = Expr.Sequential(exprDes, cachePrintExpr)
        Expr.Sequential(exprDes,exprState) 

    else Expr.Sequential(exprDes,exprState)

let getExprCachingOnSend payloadTypes payloadNames buffers exprAction = 
    let cachingSupported = payloadTypes 
                           |> List.filter (fun x -> x <> "System.Int32") 
                           |> List.length |> (fun x -> x=0) 
    
    if (cachingSupported=true) then 
         let addToCacheExpr = 
             <@@ let myValues:int [] = 
                    (%%(Expr.NewArray(typeof<int>, buffers)):int []) 
                 Runtime.addVarsToCache "cache" payloadNames myValues  @@>

         let exprAction = Expr.Sequential(addToCacheExpr, exprAction)
         let printCacheExpr = <@@ Runtime.printCount "cache" @@>
         Expr.Sequential(printCacheExpr,exprAction)
     else
        exprAction 

let invokeCodeOnSend (args:Expr list) (payload: ScribbleProtocole.Payload [])
    exprState role fullName assertion inferredVars = 
    
    let labelDelim, payloadDelim, endDelim = getDelims fullName
    let allBuffers = args.Tail.Tail                                                         
    let payloadNames = (payloadsToVarNames payload)

    let sendBuffers = 
        match inferredVars with 
            |Some inferred -> 
                mergeGivenAndInferredVarsOnSend payloadNames inferred allBuffers
            |None -> allBuffers

    let types = payloadsToTypes payload 
    let assertionFunc, assertionArgs = inlineAssertion assertion
    let sendExpr = 
        <@@ 
            let buf = 
                %(serialize 
                    fullName sendBuffers types (payloadDelim.Head) 
                    (endDelim.Head) (labelDelim.Head) 
                    assertionArgs assertionFunc payloadNames)
            Runtime.sendMessage "agent" (buf:byte[]) role @@>

    let exprCaching = 
        getExprCachingOnSend types payloadNames sendBuffers sendExpr
    Expr.Sequential(exprCaching,exprState) 

let invokeCodeOnRequest role exprState =     
    let exprNext = 
        <@@ Debug.print  "in request" role  @@>
    let exprState = 
        Expr.Sequential(exprNext,exprState)
    let exprNext = 
        <@@ Runtime.requestConnection "agent" role @@>
    Expr.Sequential(exprNext,exprState)

let invokeCodeOnAccept role exprState = 
    let exprNext = 
        <@@ Debug.print  "in accept" role  @@>
    let exprState = 
        Expr.Sequential(exprNext,exprState)
    let exprNext = 
        <@@ Runtime.acceptConnection "agent" role @@>
    Expr.Sequential(exprNext,exprState)

/// return the types of the values to be received 
//. This is calculates by removing the inferred types from the list of all types
let getReceiveTypes (payloadNames: string list) (types: string list)
        (inferred: Generic.IDictionary<string, string> option) = 
    
    match inferred with 
    | None -> types
    | Some inferredVars -> 
        List.zip payloadNames types 
            |> List.filter (fun (varName, _) -> 
                not (inferredVars.ContainsKey(varName)))
            |> List.map (fun (_, varType) -> 
                varType)

/// return those variable names  (and their names represented as Expr) 
/// that have to be assigned by the received values
let getReceiveBuffers (payloadNames: string list) (buffers: Expr list)
                        (inferred: Generic.IDictionary<string, string> option) = 
    match inferred with 
    | None -> buffers
    | Some inferredVars -> 
        List.zip payloadNames buffers 
            |> List.filter (fun (varName, _) -> not (inferredVars.ContainsKey(varName)))
            |> List.map (fun (_, bufferExpr) -> bufferExpr)

// return those variable names  (and their names represented as Expr) 
// that have to be assigned from the InferredValues dictionary
let getInferredBuffers (payloadNames: string list) (buffers: Expr list)
                       (inferredVars: Generic.IDictionary<string, string>) = 

        List.zip payloadNames buffers 
        |> List.filter (fun (varName, _) -> (inferredVars.ContainsKey(varName))) 
        |> List.map (fun (varName, bufferExpr) -> (varName, bufferExpr))

let getExprFromInferred (inferred: Generic.IDictionary<string, string>) = 
        inferred 
        |> Seq.map (|KeyValue|)  
        |> Map.ofSeq
        |> Map.map (fun _ expr -> 
            let parsedExpr = AssertionParsing.FuncGenerator.parseAssertionExpr expr
            let expr = assertionExprToCachingExpr parsedExpr
            expr)

let getExprInferredVars payloadNames buffers parsedInferred newExpr  = 
    match parsedInferred with 
    | None -> newExpr
    | Some inferred -> 
        let inferredBufs = getInferredBuffers payloadNames buffers inferred 
        let buffersToAssign = inferredBufs |> List.map (fun (_, iter) -> 
            Expr.Coerce(iter,typeof<ISetResult>))
        let inferredBuffsMapping = getExprFromInferred inferred 
        let coercedExpr = inferredBufs |> List.map (fun (name, _) -> 
            Expr.Coerce(inferredBuffsMapping.Item name,typeof<System.Int32>))
        let inferredExpr =  
            <@@ Runtime.setResults 
                    (%%(Expr.NewArray(typeof<System.Int32>, coercedExpr)):System.Int32 [])
                    (%%(Expr.NewArray(typeof<ISetResult>, buffersToAssign)):ISetResult []) 
            @@>
        Expr.Sequential(inferredExpr, newExpr)

let invokeCodeOnReceive (args:Expr list) 
        (payload: ScribbleProtocole.Payload []) exprCurrent 
        assertionString  inferredVars deserializeFunc = 

    let payloadTypes = (payloadsToTypes payload)
    let payloadNames = (payloadsToVarNames payload)
    let newPayloadTypes = 
        getReceiveTypes payloadNames payloadTypes inferredVars
    let allBuffers = args.Tail.Tail
    let receiveBuffers = 
        getReceiveBuffers payloadNames allBuffers inferredVars 
    let assertionFunc, assertionArgs = inlineAssertion assertionString
    let exprDeserialize = 
        deserializeFunc receiveBuffers 
            newPayloadTypes assertionArgs assertionFunc                                                            
    let exprCaching = 
        getExprCachingOnReceive payloadTypes payload 
            allBuffers exprDeserialize exprCurrent
    let exprInferredVars = 
        getExprInferredVars payloadNames allBuffers 
            inferredVars exprCaching
    exprInferredVars

let invokeCodeOnChoice (payload: ScribbleProtocole.Payload []) indexList fsmInstance role 
        (args: Expr list) (labelNames: string List) = 
         
    let listPayload = (payloadsToTypes payload) 
    let listExpectedMessagesAndTypes  = getAllChoiceLabels indexList fsmInstance
    let listExpectedMessages = listExpectedMessagesAndTypes |> List.map fst
    let listExpectedTypes = 
        listExpectedMessagesAndTypes 
        |> List.map snd 
        |> List.map (fun p -> payloadsToTypes p)
    
    // 1. get all label names 
    // 2. Map them to the approprate handler 
    // 3. get the handler : h = findByName
    // 4. invoke h with the received value 
    // Should invorporate whatever is in the receive... 

    // Maybe we can't implement only receive handlers! 
    let elem = <@ 1 @> 
    let expr = 
        <@@  
            Debug.print "Before Branching :" 
                (listExpectedMessages,listExpectedTypes,listPayload)
        
            let result = 
                Runtime.receiveMessage "agent" listExpectedMessages 
                    role listExpectedTypes 
            let decode = new UTF8Encoding() 
            let labelRead = decode.GetString(result.[0])
            Debug.print "After receive :" labelRead
            // let handlerIndex = labelNames |> List.findIndex (fun x -> x = labelRead)
            // let handler = args.[handlerIndex]
            // here have invokeCodeOnreceive
        
            let labelIndex = 
                labelNames 
                |> List.findIndex (fun x -> x.Equals(labelRead))         

            if labelIndex = 1 then 
                %%Expr.Applications(args.[1], [[elem]; [elem]])
            else if labelIndex = 2 then 
                %%Expr.Applications(args.[2], [[elem]; [elem]])
    @@>
    expr
    //let index = Expr.Coerce(expr, typeof<int>) :? System.Int32 
    //let elem = <@ 1 @>
    //let index = Expr.Coerce(expr, typeof<int>)
    //Expr.Applications( <@@ args.[%%index] @@>, [[elem]; [elem]])
    //let elem = <@ 1 @>
    

       (*
        let methodInfo =
            handler.GetMethods()
            |> Array.filter (
                fun x -> x.Name = "Invoke" 
                            && x.GetParameters().Length = 1)
            |> Array.head
        let nextPartialFn = 
            methodInfo.Invoke(partialFn, [| args.Head |])
        helper nextPartialFn args.Tail
        handler 1 *)
        //typeof<TypeChoices.Choice1> |> ignore
        // get the current channel 
        // TODO: Get contructor for the generated type 
        //let instanceOfChoice = Expr.Call getTheContructorOfThisType
        //let instance = %%(args.[0])
        //instance
        //Expr.Application(handler,  instance) |> ignore
        (*let assembly = System.Reflection.Assembly.GetExecutingAssembly() 
        let label = Runtime.getLabelType labelRead 
        let ctor = label.GetConstructors().[0] 
        let typing = assembly.GetType(label.FullName) 
        System.Activator.CreateInstance(typing,[||])*)

let rec makeFunctionType (types: System.Type list) =
            match types with
            | [x;y] -> FSharp.Reflection.FSharpType.MakeFunctionType(x,y)
            | x::[] -> x
            | x::xs -> FSharp.Reflection.FSharpType.MakeFunctionType(x,makeFunctionType xs)
            //FSharp.Reflection.FSharpType.MakeFunctionType(typeof<()>,System.Int32) //failwith "shouldn't happen"
            | _ ->  typeof<unit> 

let generateHandlers (aType:ProvidedTypeDefinition)
        (labels:Map<string,ProvidedTypeDefinition>)  
        (fsmInstance: ScribbleProtocole.Root []) 
        currentState indexList indexOfState 
        (mRole:Map<string,ProvidedTypeDefinition>) 
        (providedList: ProvidedTypeDefinition list) = 
    let mutable mapping = Map.empty<_,ProvidedParameter>
    let event = fsmInstance.[indexOfState]
    let listIndexChoice = findSameCurrent event.CurrentState fsmInstance

    let rec aux (outTranition:int) =
        let currEvent = fsmInstance.[outTranition]
        
        let name = currEvent.Label.Replace("(","").Replace(")","")                                                                                 
        let listTypes = createProvidedParameters currEvent 
        let listParam = 
            List.append 
                [ProvidedParameter(
                    "Role_State_" + currEvent.NextState.ToString(),
                    mRole.[currEvent.Partner]
                    )] 
                listTypes

        let nextType = findProvidedType providedList (currEvent.NextState)
        let inferredVars = parseInfVars currEvent.Inferred
        (*
        let paramTypes = 
            listTypes 
            |>  List.map (fun (x:ProvidedParameter) -> x.ParameterType)
        *)

        let assembly = Assembly.GetExecutingAssembly()
        let labelType = labels.[currEvent.Label+"_1"].DeclaringType
        //let ctor = label.GetConstructors().[0] 
        //let labelType = assembly.GetType(nextType.FullName).DeclaringType
        // (labelType::paramTypes)
        //let T = makeFunctionType(List.append [labelType] [typeof<End>])        
        //let T = makeFunctionType(List.append [labelType] [typeof<unit>])        
        let T = makeFunctionType(List.append [typeof<int>; typeof<int>] [typeof<unit>])        
        let param = ProvidedParameter(currEvent.Label, T)
        param
             
    in listIndexChoice |> List.map aux 

let generateChoice (aType:ProvidedTypeDefinition)  
        (fsmInstance: ScribbleProtocole.Root []) 
        currentState indexList indexOfState 
        (mRole:Map<string,ProvidedTypeDefinition>) 
        (providedList: ProvidedTypeDefinition list) 
        (labels: Map<string, ProvidedTypeDefinition>) = 
    //aType |> addMethod myMethod |> ignore

    aType.AddMemberDelayed(fun () -> 
        let assem = typeof<TypeChoices.Choice1>.Assembly
        let labelType = assem.GetType(ChoiceType + string currentState)
        let event = fsmInstance.[indexOfState]
        let role = event.Partner
    
        let handlers = 
            generateHandlers aType labels fsmInstance currentState
                indexList indexOfState mRole providedList

        let myMethod = 
            //let paramTypes = listParam |>  List.map (fun (x:ProvidedParameter) -> x.ParameterType)
            //let T = makeFunctionType(paramTypes)
            //let param = [ProvidedParameter("handler", T)]
            let labelNames = handlers |> List.map (fun x -> x.Name) 
            ProvidedMethod("branch", 
                handlers, typeof<End>, 
                IsStaticMethod = false, 
                InvokeCode = (fun args  ->  
                    invokeCodeOnChoice event.Payload 
                        indexList fsmInstance role args labelNames)) 
    
        let doc = getDocForChoice indexList fsmInstance
        myMethod.AddXmlDocDelayed(fun () -> doc)
        myMethod
    )

let internal makeChoiceLabelTypes (fsmInstance:ScribbleProtocole.Root []) 
        (providedList: ProvidedTypeDefinition list) 
        (mRole:Map<string,ProvidedTypeDefinition>)
        : Map<string,ProvidedTypeDefinition> * ProvidedTypeDefinition list = 

    let mutable listeLabelSeen = []
    let mutable listeType = []
    let mutable choiceIter = 1
    let mutable mapping = Map.empty<_,ProvidedTypeDefinition>
    for event in fsmInstance do
        if (event.Type.Contains("choice") 
            && not(alreadySeenLabel listeLabelSeen (event.Label,event.CurrentState))) then
            match choiceIter with
            |i when i <= TypeChoices.NUMBER_OF_CHOICES ->   
                let assem = typeof<TypeChoices.Choice1>.Assembly
                let typeCtor = assem.GetType(ChoiceType + i.ToString())
                choiceIter <- choiceIter + 1
                let listIndexChoice = findSameCurrent event.CurrentState fsmInstance
                let rec aux (liste:int list) =
                    if (List.length liste) = 0 then ()
                    else
                        let currEvent = fsmInstance.[liste.Head]
                        let name = currEvent.Label.Replace("(","").Replace(")","") 
                        let mutable holderType = 
                            name + "_1" 
                            |> createProvidedIncludedType
                        
                        let mutable aType = 
                            name 
                            |> createProvidedIncludedType
                            |> addCstor (<@@ name @@> |> createCstor [])

                        if (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                            aType <- mapping.[currEvent.Label] //:?> ProvidedTypeDefinition
                                                                                        
                        let listTypes = createProvidedParameters currEvent 
                        let listParam = 
                            List.append 
                                [ProvidedParameter("Role_State_" + currEvent.NextState.ToString(), 
                                    mRole.[currEvent.Partner])] 
                                listTypes

                        let nextType = findProvidedType providedList (currEvent.NextState)
                        let ctor = nextType.GetConstructors().[0]
                        let exprState = Expr.NewObject(ctor, [])
                        let inferredVars = parseInfVars currEvent.Inferred
                        
                        let myMethod = 
                            ProvidedMethod("receive",listParam,nextType,
                                IsStaticMethod = false,
                                InvokeCode = fun args-> 
                                    invokeCodeOnReceive args currEvent.Payload   
                                        exprState event.Assertion inferredVars
                                        deserializeChoice)
                        
                        let doc = getAssertionDoc currEvent.Assertion currEvent.Inferred
                        if doc <> "" then  myMethod.AddXmlDocDelayed(fun() -> doc)                                                                                                                                        
                        aType <- aType |> addMethod (myMethod)

                        //aType.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
                        //aType.HideObjectMethods <- true
                        //aType.AddInterfaceImplementation typeCtor
                        aType.AddMember holderType


                        if not (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then 
                            mapping <- mapping.Add(currEvent.Label,aType)
                            mapping <- mapping.Add(currEvent.Label + "_1", holderType)
                            listeType <- (aType)::listeType       
                        listeLabelSeen <- (currEvent.Label,currEvent.CurrentState)::listeLabelSeen
                        
                        

                        if (List.length liste) > 1 then aux liste.Tail

                in aux listIndexChoice 
            | _ -> failwith ("number of choices > " + TypeChoices.NUMBER_OF_CHOICES.ToString() 
                                + " : This protocol won't be taken in account by this TP. ") 

    (mapping,listeType)

let generateMethod (aType:ProvidedTypeDefinition) 
        (methodName:string) listParam nextType 
        (errorMessage:string) (event: ScribbleProtocole.Root) 
        exprState role = 
    aType.AddMembersDelayed( fun () -> 
        let fullName = event.Label
        let nameLabel = fullName.Replace("(","").Replace(")","")
        let inferredVars = parseInfVars event.Inferred
        let methods = 
            match methodName with
            |"send" -> 
                let m = ProvidedMethod(methodName+nameLabel, listParam, nextType,
                            IsStaticMethod = false,
                            InvokeCode = fun args-> 
                                invokeCodeOnSend args event.Payload 
                                    exprState role fullName 
                                    event.Assertion inferredVars)
                
                //let dummy = ProvidedMethod(methodName+nameLabel, [ ], nextType, IsStaticMethod = false, InvokeCode = fun args -> <@@ 3 @@>)              
                // all arguments can be given as static parameters, then they are imemdiately inserted in the cache. 
                // otherwise, we can use the static parameters to program the assertions on send (in the choice)!
                let staticParams = [ProvidedStaticParameter("Count", typeof<int>)]
                
                
                let add (a:System.Int32) (b:System.Int32) = a + b
                let T = add.GetType()
               
                //let T1 = typeof<int -> float> 
                //T1 = T2
                (*m.DefineStaticParameters(staticParams, (fun nm args ->
                                            let arg = args.[0] :?> int
                                            if arg < 3 then  
                                                let m2 = ProvidedMethod(nm, param, nextType,
                                                            IsStaticMethod = false,
                                                            InvokeCode = fun args-> 
                                                                invokeCodeOnSend args event.Payload 
                                                                    exprState role fullName 
                                                                    event.Assertion inferredVars)
                                                aType.AddMember m2
                                                m2
                                            else failwith "Assertion not satisfied"
                                            ))*)
                [m]
            |"receive" ->  
                let labelDelim, _, _ = getDelims fullName
                let decode = new System.Text.UTF8Encoding()
                let message = 
                    Array.append (decode.GetBytes(fullName)) 
                        (decode.GetBytes(labelDelim.Head))

                let recvMethod = 
                    ProvidedMethod(
                        methodName+nameLabel,listParam,nextType,
                        IsStaticMethod = false,
                        InvokeCode = fun args -> 
                            invokeCodeOnReceive args event.Payload  
                                exprState   event.Assertion inferredVars
                                (deserialize [message] role)
                        )
                let recvMethodAsync = 
                    ProvidedMethod(
                        (methodName+nameLabel+"Async"),listParam,nextType, 
                        IsStaticMethod = false,
                        InvokeCode = fun args -> 
                            invokeCodeOnReceive args event.Payload 
                                exprState event.Assertion inferredVars
                                (deserializeAsync [message] role))
                [recvMethod; recvMethodAsync]
            |"request" ->
                [ProvidedMethod(
                    methodName+nameLabel, 
                    listParam, nextType, 
                    IsStaticMethod = false,
                    InvokeCode = fun _-> invokeCodeOnRequest role exprState)]
            |"accept" ->  
                    [ProvidedMethod(
                        methodName+nameLabel, 
                        listParam, 
                        nextType, 
                        IsStaticMethod = false,
                        InvokeCode = fun _-> invokeCodeOnAccept role exprState)] 
            | _ -> failwith errorMessage    

        methods) 
        (*|> List.iter (fun genMethod -> 
            let doc = getAssertionDoc event.Assertion event.Inferred
            if doc <> "" then genMethod.AddXmlDocDelayed(fun () -> doc);
            aType |> addMethod genMethod |> ignore)*)

let generateMethodParams (fsmInstance:ScribbleProtocole.Root []) idx 
        (providedList:ProvidedTypeDefinition list) roleValue  = 

    let nextType = 
        findProvidedType providedList fsmInstance.[idx].NextState
    let methodName = fsmInstance.[idx].Type
    let event = fsmInstance.[idx]
    let c = nextType.GetConstructors().[0]
    let exprState = Expr.NewObject(c, [])
                
    let listTypes = 
        match methodName with
            |"send" -> payloadsToProvidedList event.Payload event.Inferred
            |"receive" -> createProvidedParameters event 
            | _ -> []
                
    let listParam = 
        match methodName with
            |"send" | "receive" | "accept" | "request" -> 
                List.append [ProvidedParameter("Role", roleValue)] listTypes
            | _  -> []

    let makeReturnTuple = (methodName, listParam, nextType, exprState)
    makeReturnTuple

let rec goingThrough (methodNaming:string) 
        (providedList:ProvidedTypeDefinition list) 
        (aType:ProvidedTypeDefinition) (indexList:int list) 
        (mLabel:Map<string,ProvidedTypeDefinition>) 
        (mRole:Map<string,ProvidedTypeDefinition>) 
        (fsmInstance:ScribbleProtocole.Root []) =
    
    if (List.length indexList = 0) then 
        let expr = <@@ Runtime.stopMessage "agent" @@> 
        let finishExpr =  Expr.Sequential(expr, <@@ printfn "finish" @@>)
        aType 
        |> addMethod (finishExpr |> createMethodType methodNaming [] typeof<End>) 
        |> ignore
    else  
        let event = fsmInstance.[indexList.Head]
        let role = event.Partner
        let methodName, listParam, nextType, exprState = 
            generateMethodParams fsmInstance 
                indexList.Head providedList (mRole.[role]) 

        let errorMessage = ErrorMsg.unexpectedMethod methodName
        generateMethod aType methodName listParam 
            nextType errorMessage event exprState role 
        if (List.length indexList > 1) then 
            goingThrough methodName providedList aType 
                indexList.Tail mLabel mRole fsmInstance 


let rec addProperties (providedListStatic:ProvidedTypeDefinition list) 
        (providedList:ProvidedTypeDefinition list) 
        (stateList: int list) 
        (mLabel:Map<string,ProvidedTypeDefinition>) 
        (mRole:Map<string,ProvidedTypeDefinition>) 
        (fsmInstance:ScribbleProtocole.Root []) =

    let currentState = stateList.Head
    let indexOfState = findCurrentIndex currentState fsmInstance
    let indexList = findSameCurrent currentState fsmInstance 
    let mutable methodName = "finish"
    if indexOfState <> -1 then
        methodName <- fsmInstance.[indexOfState].Type
    match providedList with
    |[] -> ()
    |[aType] -> 
        match methodName with
            |"send" |"receive" |"request" |"accept" -> 
                goingThrough methodName providedListStatic 
                    aType indexList mLabel mRole fsmInstance 
            |"choice" -> 
                generateChoice aType fsmInstance currentState 
                    indexList indexOfState  mRole providedList mLabel
            |"finish" ->  
                goingThrough methodName providedListStatic aType 
                    indexList mLabel mRole fsmInstance 
            | _ -> failwith ErrorMsg.methodNameNotFound
    |hd::tl ->  
        match methodName with
            |"send" |"receive" |"request" |"accept" -> 
                goingThrough methodName providedListStatic 
                    hd indexList mLabel mRole fsmInstance 
            |"choice" -> 
                generateChoice hd fsmInstance currentState 
                    indexList indexOfState mRole providedList mLabel
            |"finish" -> 
                goingThrough methodName providedListStatic hd 
                    indexList mLabel mRole fsmInstance 
            | _ -> failwith ErrorMsg.methodNameNotFound

        addProperties providedListStatic tl (stateList.Tail) 
            mLabel mRole fsmInstance

(******************* END GENERATE STATE TYPES*******************)



