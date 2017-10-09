module ScribbleGenerativeTypeProvider.TypeGeneration

// Outside namespaces and modules
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
open System.Threading.Tasks
open System.Text
open FSharp.Quotations.Evaluator

// ScribbleProvider specific namespaces and modules
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.CommunicationAgents
open ScribbleGenerativeTypeProvider.IO
open ScribbleGenerativeTypeProvider.RefinementTypes
(******************* TYPE PROVIDER'S HELPERS *******************)

// CREATING TYPES, NESTED TYPES, METHODS, PROPERTIES, CONSTRUCTORS
let internal createProvidedType assembly name = 
    ProvidedTypeDefinition(assembly, ns, name, Some baseType, IsErased=false)

let internal createProvidedIncludedType name = 
    ProvidedTypeDefinition(name,Some baseType, IsErased=false)

let internal createProvidedIncludedTypeChoice typing name =
    ProvidedTypeDefinition(name, Some typing , IsErased=false)

let internal createMethodType name param typing expression =
    ProvidedMethod( name, param, typing, InvokeCode = (fun args -> expression ))

let internal createPropertyType name typing expression =
    ProvidedProperty( name , typing , IsStatic = true, GetterCode = (fun args -> expression ))

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


(******************* TYPE PROVIDER'S FUNCTIONS *******************)

let internal findCurrentIndex current (fsmInstance:ScribbleProtocole.Root []) = // gerer les cas
    let mutable inc = 0
    let mutable index = -1 
    let fsm = Array.toList fsmInstance
    let rec aux (acc:ScribbleProtocole.Root list) count =
        match acc with
            |[] -> -1
            |hd::tl -> if hd.CurrentState = current then   
                          count
                       else
                          aux tl (count+1) 
    aux fsm 0

let internal findNext index (fsmInstance:ScribbleProtocole.Root []) =
    (fsmInstance.[index].NextState)

let internal findNextIndex currentState (fsmInstance:ScribbleProtocole.Root []) =
    let index = findCurrentIndex currentState fsmInstance in
    let next = findNext index fsmInstance in
    findCurrentIndex next fsmInstance

let internal findSameNext nextState  (fsmInstance:ScribbleProtocole.Root [])  =
    let mutable list = []
    let mutable inc = 0
    for event in fsmInstance do
        if event.NextState = nextState then
            list <- inc::list
        inc <- inc+1
    list

let rec alreadySeenLabel (liste:(string*int) list) (elem:string*int) =
    match liste with
        | [] -> false
        | (hdS,hdI)::tl ->  if hdS.Equals(elem |> fst) && hdI.Equals(elem |> snd) then
                                true
                            else
                                alreadySeenLabel tl elem

let rec alreadySeenOnlyLabel (liste:(string*int) list) (elem:string) =
    match liste with
        | [] -> false
        | (hdS,hdI)::tl ->  if hdS.Equals(elem) then
                                true
                            else
                                alreadySeenOnlyLabel tl elem


let rec alreadySeenRole (liste:string list) (elem:string) =
    match liste with
        | [] -> false
        | hd::tl -> if hd.Equals(elem) then
                        true
                    else
                        alreadySeenRole tl elem


let internal findSameCurrent currentState  (fsmInstance:ScribbleProtocole.Root [])  =
    let mutable list = []
    let mutable inc = 0
    for event in fsmInstance do
        if event.CurrentState = currentState then
            list <- inc::list
        inc <- inc+1
    list


// Test this function by changing t with t+1 and see the mistakes happen  -> generate the useless ProvidedTypeDefinition and throw exception cause it
// is not added to the assembly.
let rec findProvidedType (providedList:ProvidedTypeDefinition list) stateValue =
    match providedList with
        |[] -> // Useless case, t is useless but we need this case due to pattern matching exhaustiveness.
                "CodingMistake" |> createProvidedIncludedType 
        |[a] -> let t = ref 0
                if System.Int32.TryParse(a.Name.Replace("State",""),t) && (!t)=stateValue then
                    a
                else 
                    findProvidedType [] stateValue    
        |hd::tl -> let t = ref 0
                   if System.Int32.TryParse(hd.Name.Replace("State",""),t) && (!t)=stateValue then
                       hd
                   else
                       findProvidedType tl stateValue      

let internal createProvidedParameters (event : ScribbleProtocole.Root) =
    let generic = typeof<Buf<_>>.GetGenericTypeDefinition() 
    let payload = event.Payload
    let mutable n = 0

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
            let genType = generic.MakeGenericType(System.Type.GetType(param.VarType))
            yield ProvidedParameter((param.VarName),genType) 
    ]

(*let internal toList (array:_ []) =
    [for elem in array do
        yield elem ]*)

let internal payloadsToList (payloads: System.Collections.Generic.IEnumerable<ScribbleProtocole.Payload>) =
    [for elem in payloads do
        yield elem.VarType ]


let internal payloadsToListStr (payloads:ScribbleProtocole.Payload []) =
    [for i in 0..(payloads.Length-1) do
        yield payloads.[i].VarName
    
    ]

let internal payloadsToProvidedList (payloads:ScribbleProtocole.Payload []) =
    [for i in 0 ..(payloads.Length-1) do 
            yield ProvidedParameter((payloads.[i].VarName),System.Type.GetType(payloads.[i].VarType))
    ]

                       
let internal makeRoleTypes (fsmInstance:ScribbleProtocole.Root []) = 
    let mutable liste = [fsmInstance.[0].LocalRole]
    let mutable listeType = []
    let ctor = <@@ () @@> |> createCstor []
    let t = fsmInstance.[0].LocalRole 
                                        |> createProvidedIncludedType 
                                        |> addCstor ctor
    let t = t |> addProperty (Expr.NewObject(ctor,[]) |> createPropertyType "instance" t)
    t.HideObjectMethods <- true
    listeType <- t::listeType
    let mutable mapping = Map.empty<_,ProvidedTypeDefinition>.Add(fsmInstance.[0].LocalRole,t)
    for event in fsmInstance do
        if not(alreadySeenRole liste event.Partner) then
            let ctor = ( <@@ () @@> |> createCstor [])
            let t = event.Partner 
                                    |> createProvidedIncludedType
                                    |> addCstor ctor    
            let t = t |> addProperty (Expr.NewObject(ctor, []) |> createPropertyType "instance" t)
            t.HideObjectMethods <- true                                                                     
            mapping <- mapping.Add(event.Partner,t)
            liste <- event.Partner::liste
            listeType <- t::listeType
    (mapping,listeType)


let getAssertionDoc assertion = 
    if (assertion <> "") then 
        let sb = new System.Text.StringBuilder()
        sb.Append("<summary> Method arguments should satisfy the following constraint:") |> ignore
        sb.Append ("<para>" + assertion.Replace(">", "&gt;").Replace("<","&lt;") + "</para>" ) |>ignore
        sb.Append("</summary>") |>ignore
        sb.ToString()
    else ""    

let internal makeLabelTypes (fsmInstance:ScribbleProtocole.Root []) (providedList: ProvidedTypeDefinition list) (mRole:Map<string,ProvidedTypeDefinition>) : Map<string,ProvidedTypeDefinition> * ProvidedTypeDefinition list = 
    let mutable listeLabelSeen = []
    let mutable listeType = []
    let mutable choiceIter = 1
    let mutable mapping = Map.empty<_,ProvidedTypeDefinition>
    for event in fsmInstance do
        if (event.Type.Contains("choice") && not(alreadySeenLabel listeLabelSeen (event.Label,event.CurrentState))) then
            match choiceIter with
            |i when i <= TypeChoices.NUMBER_OF_CHOICES ->   
                let assem = typeof<TypeChoices.Choice1>.Assembly
                let typeCtor = assem.GetType("ScribbleGenerativeTypeProvider.TypeChoices+Choice" + i.ToString())
                //mapping <- mapping.Add("Choice"+ string event.CurrentState,typeCtor)
                //listeType <- typeCtor::listeType 
                choiceIter <- choiceIter + 1
                let listIndexChoice = findSameCurrent event.CurrentState fsmInstance
                let rec aux (liste:int list) =
                    match liste with
                    |[] -> ()
                    |[aChoice] -> 
                        let currEvent = fsmInstance.[aChoice]
                        let name = currEvent.Label.Replace("(","").Replace(")","") 
                        printing "Add types + Ctor = " name
                        let mutable t = name |> createProvidedIncludedType
                                            |> addCstor (<@@ name @@> |> createCstor [])

                        if (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                            t <- mapping.[currEvent.Label] //:?> ProvidedTypeDefinition
                                                                                        
                        let listTypes = createProvidedParameters currEvent
                        let listParam = List.append [ProvidedParameter("Role_State_" + currEvent.NextState.ToString(),mRole.[currEvent.Partner])] listTypes
                        //let listPayload = (toList event.Payload)   
                        let nextType = findProvidedType providedList (currEvent.NextState)
                        let ctor = nextType.GetConstructors().[0]
                        let exprState = Expr.NewObject(ctor, [])
                        let myMethod = 
                            ProvidedMethod("receive",listParam,nextType,
                                           IsStaticMethod = false,
                                           InvokeCode = 
                                            fun args-> 
                                                let buffers = args.Tail.Tail
                                                (*let listPayload = (payloadsToList event.Payload)
                                                let exprDes = deserializeChoice buffers listPayload
                                                Expr.Sequential(exprDes,exprState)//*)                                             

                                                let listPayload = (payloadsToList event.Payload)

                                                let assertionString = event.Assertion



                                                let fooName,argsName = 
                                                    if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
                                                        let index = Regarder.getIndex "agent" 
                                                        let assertion = RefinementTypes.createFnRule index assertionString
                                                        let elem = assertion |> fst 
                                                        Regarder.addToDict "agent" elem
                                                        snd assertion 
                                                    else 
                                                        "",[]

                                                let exprDes = deserializeChoice buffers listPayload argsName fooName
                                                Expr.Sequential(exprDes,exprState)
                                          )
                        let doc = getAssertionDoc event.Assertion
                        if doc <> "" then  myMethod.AddXmlDocDelayed(fun() -> doc)                                                                                                                                        
                        
                        t <- t |> addMethod (myMethod)

                        t.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
                        t.HideObjectMethods <- true
                        t.AddInterfaceImplementation typeCtor
                        
                        if not (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then 
                            mapping <- mapping.Add(currEvent.Label,t)
                            listeType <- (t)::listeType       
                        listeLabelSeen <- (currEvent.Label,currEvent.CurrentState)::listeLabelSeen
                                                                                                     
                    |hd::tl ->  
                        let currEvent = fsmInstance.[hd] 
                        let name = currEvent.Label.Replace("(","").Replace(")","")
                        printing "Add types + Ctor = " name
                        let mutable t = name |> createProvidedIncludedType
                                                |> addCstor (<@@ name @@> |> createCstor [])
//                                                                                                         |> addCstor ([] |> createCstor <|  <@@ () @@>) 
                                                                                    
                        if (alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                            t <- mapping.[currEvent.Label] //:?> ProvidedTypeDefinition    
                        //if not(alreadySeenLabel listeLabelSeen (currEvent.Label,currEvent.CurrentState)) then
                            //let name = currEvent.Label.Replace("(","").Replace(")","") 
                                                                                        
                        let listTypes = createProvidedParameters currEvent
                        //let listPayload = (toList currEvent.Payload)   
                        let listParam = List.append [ProvidedParameter("Role_State_" + currEvent.NextState.ToString(),mRole.[currEvent.Partner])] listTypes
                        let nextType = findProvidedType providedList (currEvent.NextState)                                                                                 
                        let ctor = nextType.GetConstructors().[0]
                        let exprState = Expr.NewObject(ctor, [])
                        let myMethod = 
                            ProvidedMethod("receive",listParam,nextType,
                                            IsStaticMethod = false,
                                            InvokeCode = 
                                                fun args-> 
                                                    let buffers = args.Tail.Tail
                                                    let listPayload = (payloadsToList event.Payload)
                                                    let assertionString = event.Assertion

                                                    let fooName,argsName = 
                                                        if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
                                                            let index = Regarder.getIndex "agent" 
                                                            let assertion = RefinementTypes.createFnRule index assertionString
                                                            let elem = assertion |> fst 
                                                            Regarder.addToDict "agent" elem
                                                            snd assertion 
                                                        else 
                                                            "",[]

                                                    let exprDes = deserializeChoice buffers listPayload argsName fooName
                                                    Expr.Sequential(exprDes,exprState)
                                          )
                        let doc = getAssertionDoc event.Assertion
                        if doc <> "" then myMethod.AddXmlDocDelayed(fun() -> doc)     

                        t <- t |> addMethod (myMethod)

                        t.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
                        t.HideObjectMethods <- true
                        t.AddInterfaceImplementation typeCtor
                        if not(alreadySeenOnlyLabel listeLabelSeen currEvent.Label) then
                            mapping <- mapping.Add(currEvent.Label,t)
                            listeType <- (t)::listeType
                        listeLabelSeen <- (currEvent.Label,currEvent.CurrentState)::listeLabelSeen
                                                                                        
                        aux tl 
                in aux listIndexChoice 
            | _ -> failwith ("number of choices > " + TypeChoices.NUMBER_OF_CHOICES.ToString() + " : This protocol won't be taken in account by this TP. ") 

    (mapping,listeType)


let internal makeStateTypeBase (n:int) (s:string) = 
    let ty = (s + string n) |> createProvidedIncludedType
                            |> addCstor (<@@ s+ string n @@> |> createCstor [])
    ty.HideObjectMethods <- true
    ty

let internal makeStateType (n:int) = makeStateTypeBase n "State"

let internal getAllChoiceLabels (indexList : int list) (fsmInstance:ScribbleProtocole.Root []) =
        let rec aux list acc =
            match list with
                |[] -> acc
                |hd::tl -> 
                    printing "getAllChoiceLabels : I run" ""
                    let label = fsmInstance.[hd].Label
                    let labelDelim,_,_ = getDelims label
                    let labelBytes = label |> serLabel <| (labelDelim.Head) 
                    let typing = fsmInstance.[hd].Payload |> List.ofArray
                    aux tl ((labelBytes,typing)::acc) 
        in aux indexList []

let internal getAllChoiceLabelString (indexList : int list) (fsmInstance:ScribbleProtocole.Root []) =
    let rec aux list acc =
        match list with
            |[] -> acc
            |hd::tl -> let labelBytes = fsmInstance.[hd].Label 
                       aux tl (labelBytes::acc) 
    in aux indexList []

let invokeCodeOnSend (args:Expr list) (payload: ScribbleProtocole.Payload [])  (payloadDelim: string List) 
                    (labelDelim : string List)  (endDelim: string List)  (nameLabel:string) exprState role fullName (event:ScribbleProtocole.Root) = 
    let buffers = args.Tail.Tail         
                                                    
    let payloadNames = (payloadsToListStr payload )
    let types = payloadsToList payload
    let assertionString = event.Assertion
    
    let fooName,argsName = 
        if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
            let index = Regarder.getIndex "agent" 
            let assertion = RefinementTypes.createFnRule index assertionString
            let elem = assertion |> fst 
            Regarder.addToDict "agent" elem
            snd assertion 
        else 
            "",[]

    //let buf = ser buffers
    let exprAction = 
        <@@ let buf = %(serialize fullName buffers types (payloadDelim.Head) (endDelim.Head) (labelDelim.Head) argsName fooName)
            Regarder.sendMessage "agent" (buf:byte[]) role @@>

    let fn eq =
        <@ if eq then failwith (sprintf "METHOD USED : Send + Label = %A" nameLabel )
            else printing "METHOD USED : Send + Label = " nameLabel @>
    
    let exprAction = 
        Expr.Sequential(<@@ %(fn false) @@>,exprAction)
                                                     
    let cachingSupported = types |> List.filter (fun x -> x <> "Syste,.Int32") 
                                        |> List.length |> (fun x -> x=0) 
    if (cachingSupported=true) then 
        let addToCacheExpr = 
            <@@ let myValues:int [] = (%%(Expr.NewArray(typeof<int>, buffers)):int []) 
                Regarder.addVars "cache" payloadNames myValues  
                @@>
        let exprAction = Expr.Sequential(addToCacheExpr, exprAction)
        let printCacheExpr = <@@ Regarder.printCount "cache" @@>
        let exprAction = Expr.Sequential(printCacheExpr,exprAction)
        Expr.Sequential(exprAction,exprState) 
    else
        Expr.Sequential(exprAction,exprState) 


let invokeCodeOnRequest role exprState= 
    let hello = "hello"
    let exprNext = 
        <@@ printing "in request" role  @@>
    let exprState = 
        Expr.Sequential(exprNext,exprState)
    let exprNext = 
        <@@ Regarder.requestConnection "agent" role @@>
    Expr.Sequential(exprNext,exprState)

let invokeCodeOnAccept role exprState= 
    let hello = "hello"
    let exprNext = 
        <@@ printing "in accept" role  @@>
    let exprState = 
        Expr.Sequential(exprNext,exprState)
    let exprNext = 
        <@@ Regarder.acceptConnection "agent" role @@>
    Expr.Sequential(exprNext,exprState)

let invokeCodeOnReceive (args:Expr list) (payload: ScribbleProtocole.Payload [])  (payloadDelim: string List) 
                       (labelDelim : string List)  (endDelim: string List)  (nameLabel:string) (message: byte[]) 
                        exprState role fullName assertionString = 
   
    let buffers = args.Tail.Tail
    let listPayload = (payloadsToList payload)

    let fooName,argsName = 
        if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
            let index = Regarder.getIndex "agent" 
            let assertion = RefinementTypes.createFnRule index assertionString
            let elem = assertion |> fst 
            Regarder.addToDict "agent" elem
            snd assertion 
        else 
            "",[]

    let exprDes = deserialize buffers listPayload [message] role argsName fooName
   
    let exprDes = 
        Expr.Sequential(<@@ printing "METHOD USED : Receive + Label = " nameLabel @@>,exprDes)
                                                            
    let cachingSupported = listPayload |> List.filter (fun x -> x <> "Syste,.Int32") 
                                |> List.length |> (fun x -> x=0) 
    if (cachingSupported=true) then 
        let payloadNames = (payloadsToListStr payload)
        
        let addToCacheExpr = 
            <@@ let myValues:Buf<int> [] = (%%(Expr.NewArray(typeof<Buf<int>>, buffers)):Buf<int> []) 
                Regarder.addVarsBufs "cache" payloadNames myValues @@>

        let exprDes = Expr.Sequential(exprDes, addToCacheExpr)                                                            
        let cachePrintExpr = <@@ Regarder.printCount "cache" @@>
        
        let exprDes = Expr.Sequential(exprDes, cachePrintExpr)
        Expr.Sequential(exprDes,exprState) 
    else 
        Expr.Sequential(exprDes,exprState) 

let invokeCodeOnChoice (payload: ScribbleProtocole.Payload []) indexList fsmInstance role = 
    let listPayload = (payloadsToList payload) 
    let listExpectedMessagesAndTypes  = getAllChoiceLabels indexList fsmInstance
    let listExpectedMessages = listExpectedMessagesAndTypes |> List.map fst
    let listExpectedTypes = listExpectedMessagesAndTypes |> List.map snd |> List.map (fun p -> payloadsToList p)
    
    <@@ 
        printing "Before Branching : " (listExpectedMessages,listExpectedTypes,listPayload)
        let result = Regarder.receiveMessage "agent" listExpectedMessages role listExpectedTypes 
        let decode = new UTF8Encoding() 
        let labelRead = decode.GetString(result.[0]) 
        let assembly = System.Reflection.Assembly.GetExecutingAssembly() 
        let label = Regarder.getLabelType labelRead 
        let ctor = label.GetConstructors().[0] 
        let typing = assembly.GetType(label.FullName) 
        System.Activator.CreateInstance(typing,[||]) 
    @@>

let getDocForChoice indexList fsmInstance=  
    let sb = new System.Text.StringBuilder()
    sb.Append("<summary> When branching here, you will have to type pattern match on the following types :") |> ignore
    
    (indexList |> getAllChoiceLabelString <| fsmInstance)
    |> List.iter(fun message -> sb.Append ("<para> - " + message + "</para>" ) |> ignore ) 
    |> ignore

    sb.Append("</summary>") |> ignore
    sb.ToString()


let generateMethod aType (methodName:string) listParam nextType (errorMessage:string) 
                   (event: ScribbleProtocole.Root) exprState role = 
    
    let fullName = event.Label
    let nameLabel = fullName.Replace("(","").Replace(")","")

    match methodName with
        |"send" -> 
            let labelDelim, payloadDelim, endDelim = getDelims fullName
            let decode = new System.Text.UTF8Encoding()
            let message = Array.append (decode.GetBytes(fullName)) (decode.GetBytes(labelDelim.Head))
            
            let myMethod = 
                ProvidedMethod(methodName+nameLabel, listParam, nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun args-> 
                            invokeCodeOnSend args event.Payload payloadDelim 
                                labelDelim endDelim nameLabel 
                                exprState role fullName event)
                        
            let doc = getAssertionDoc event.Assertion
            if doc <> "" then myMethod.AddXmlDocDelayed(fun () ->  doc)
                        
            aType 
                |> addMethod myMethod
                |> ignore
            let assertion = event.Assertion
            let aName = sprintf "%s" assertion
            if assertion<> "" then 
                aType |> addProperty (<@@ assertion @@> |> createPropertyType aName typeof<string> ) |> ignore
            //aType.AddMembers (createProperty "assertion" System.String event.Assertion)
        |"receive" ->  
            let labelDelim, payloadDelim, endDelim = getDelims fullName
            let decode = new System.Text.UTF8Encoding()
            let message = Array.append (decode.GetBytes(fullName)) (decode.GetBytes(labelDelim.Head))

            let myMethod = 
                ProvidedMethod(methodName+nameLabel,listParam,nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun args-> 
                            invokeCodeOnReceive args event.Payload payloadDelim 
                                labelDelim endDelim nameLabel message 
                                exprState role fullName event.Assertion)
            
            let myMethodAsync = 
                ProvidedMethod((methodName+nameLabel+"Async"),listParam,nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun args -> 
                            let buffers = args.Tail.Tail
                            let listPayload = (payloadsToList event.Payload)
                            let assertionString = event.Assertion
                           
                            let fooName,argsName = 
                                if ((assertionString <> "fun expression -> expression") && (assertionString <> ""))  then
                                    let index = Regarder.getIndex "agent" 
                                    let assertion = RefinementTypes.createFnRule index assertionString
                                    let elem = assertion |> fst 
                                    Regarder.addToDict "agent" elem
                                    snd assertion 
                                else 
                                    "",[]

                            let exprDes = deserializeAsync buffers listPayload [message] role argsName fooName
                            Expr.Sequential(exprDes,exprState))
                                       
            let doc = getAssertionDoc event.Assertion
            if doc <> "" then myMethod.AddXmlDocDelayed(fun () -> doc); myMethodAsync.AddXmlDoc(doc)
            //let property = createPropertyType "test" (Constraint.Numbers.ConstraintInt32<Rule = assertionString>) 
            aType 
            |> addMethod myMethod
            |> addMethod myMethodAsync
            
            |> ignore       
        |"request" ->
            let myMethod = 
                ProvidedMethod(methodName+nameLabel, listParam, nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun args-> 
                            invokeCodeOnRequest role exprState) 
            aType 
                |> addMethod myMethod
                |> ignore
          
        |"accept" ->  
            let myMethod = 
                ProvidedMethod(methodName+nameLabel, listParam, nextType,
                    IsStaticMethod = false,
                    InvokeCode = 
                        fun args-> 
                            invokeCodeOnAccept role exprState) 
            aType 
                |> addMethod myMethod
                |> ignore

        | _ -> failwith errorMessage                    

let generateChoice (aType:ProvidedTypeDefinition) (fsmInstance: ScribbleProtocole.Root []) currentState indexList indexOfState  = 
    let assem = typeof<TypeChoices.Choice1>.Assembly
    let labelType = assem.GetType("ScribbleGenerativeTypeProvider.TypeChoices+Choice" + string currentState)
    let event = fsmInstance.[indexOfState]
    let role = event.Partner

    let myMethod = 
        ProvidedMethod("branch", [],labelType, IsStaticMethod = false, 
            InvokeCode = 
                (fun args  ->  
                    invokeCodeOnChoice event.Payload indexList fsmInstance role))                                                                                         
    let doc = getDocForChoice indexList fsmInstance
    myMethod.AddXmlDocDelayed(fun () -> doc)
    aType |> addMethod myMethod |> ignore

let generateMethodParams (fsmInstance:ScribbleProtocole.Root []) idx (providedList:ProvidedTypeDefinition list) roleValue= 
    let nextType = findProvidedType providedList fsmInstance.[idx].NextState
    let methodName = fsmInstance.[idx].Type
    let event = fsmInstance.[idx]
    let c = nextType.GetConstructors().[0]
    let exprState = Expr.NewObject(c, [])
    let role = event.Partner
                
    let listTypes = 
        match methodName with
            |"send" -> payloadsToProvidedList event.Payload
            |"receive" -> createProvidedParameters event
            | _ -> []
                
    let listParam = 
        match methodName with
            |"send" | "receive" | "accept" | "request" -> List.append [ProvidedParameter("Role", roleValue)] listTypes
            | _  -> []


    let makeReturnTuple = (methodName, listParam, nextType, exprState)
    makeReturnTuple

let rec goingThrough (methodNaming:string) (providedList:ProvidedTypeDefinition list) (aType:ProvidedTypeDefinition) (indexList:int list) 
                     (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
        match indexList with
        |[] -> // Last state: no next state possible
                aType |> addMethod (<@@ printfn "finish" @@> |> createMethodType methodNaming [] typeof<End> ) |> ignore
        |[b] ->  let event = fsmInstance.[b]
                 let role = event.Partner

                 let methodName, listParam, nextType, exprState = generateMethodParams fsmInstance b providedList (mRole.[role])
                 let errorMessage = (sprintf " Mistake you have a method named : %s, that is not expected !" methodName )

                 generateMethod aType methodName listParam nextType errorMessage
                                event exprState role 

        |hd::tl -> let nextState = fsmInstance.[hd].NextState
                   let currentState = fsmInstance.[hd].CurrentState
                   let event = fsmInstance.[hd]
                   let role = event.Partner                 
                   
                   let methodName, listParam, nextType, exprState = generateMethodParams fsmInstance hd providedList (mRole.[role])
                   let errorMessage = (sprintf " Mistake you have a method named : %s  %d  %d !" methodName nextState currentState)
                   
                   generateMethod aType methodName listParam nextType errorMessage
                                  event exprState role 

                   goingThrough methodName providedList aType tl mLabel mRole fsmInstance 


let rec addProperties (providedListStatic:ProvidedTypeDefinition list) (providedList:ProvidedTypeDefinition list) (stateList: int list) 
                      (mLabel:Map<string,ProvidedTypeDefinition>) (mRole:Map<string,ProvidedTypeDefinition>) (fsmInstance:ScribbleProtocole.Root []) =
    let currentState = stateList.Head
    let indexOfState = findCurrentIndex currentState fsmInstance
    let indexList = findSameCurrent currentState fsmInstance 
    let mutable choiceIter = 1
    let mutable methodName = "finish"
    if indexOfState <> -1 then
        methodName <- fsmInstance.[indexOfState].Type
    match providedList with
        |[] -> ()
        |[aType] -> match methodName with
                        |"send" |"receive" |"request" |"accept" 
                            -> goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance 
                        |"choice" -> generateChoice aType fsmInstance currentState indexList indexOfState
                        |"finish" ->  goingThrough methodName providedListStatic aType indexList mLabel mRole fsmInstance 
                        | _ -> failwith "The only method name that should be available should be send/receive/choice/finish"
        |hd::tl ->  match methodName with
                        |"send" |"receive" |"request" |"accept" 
                            -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance 
                        |"choice" -> generateChoice hd fsmInstance currentState indexList indexOfState
                        |"finish" -> goingThrough methodName providedListStatic hd indexList mLabel mRole fsmInstance 
                        | _ -> failwith "The only type of event from the CFSM that should be available is one of the 
                                         following : send/receive/choice"
                    hd |> addProperty (<@@ "Test" @@> |> createPropertyType "MyProperty" typeof<string> ) |> ignore

                    addProperties providedListStatic tl (stateList.Tail) mLabel mRole fsmInstance 


let internal contains (aSet:Set<'a>) x = 
    Set.exists ((=) x) aSet

let internal stateSet (fsmInstance:ScribbleProtocole.Root []) =
    let firstState = fsmInstance.[0].CurrentState
    let mutable setSeen = Set.empty
    let mutable counter = 0
    for event in fsmInstance do
        if (not(contains setSeen event.CurrentState) || not(contains setSeen event.NextState)) then
            setSeen <- setSeen.Add(event.CurrentState)
            setSeen <- setSeen.Add(event.NextState)
    (setSeen.Count,setSeen,firstState)

let internal makeRoleList (fsmInstance:ScribbleProtocole.Root []) =
    let mutable setSeen = Set.empty
    [yield fsmInstance.[0].LocalRole
     for event in fsmInstance do
        if not(setSeen |> contains <| event.Partner) then
            setSeen <- setSeen.Add(event.Partner)
            yield event.Partner] 



