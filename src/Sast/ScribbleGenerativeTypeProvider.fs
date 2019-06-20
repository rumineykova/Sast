namespace ScribbleGenerativeTypeProvider

// Outside namespaces and modules
open System
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
open System.Diagnostics
open System.IO
open System.Collections.Generic
open FSharp.Data
// ScribbleProvider specific namespaces and modules
open ScribbleGenerativeTypeProvider.TypeGeneration
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.CommunicationAgents
open ScribbleGenerativeTypeProvider.RefinementTypesDict
open ScribbleGenerativeTypeProvider.AsstScribbleParser
open ScribbleGenerativeTypeProvider.Util


type ScribbleSource = 
    | WebAPI = 0 
    | File = 1
    | LocalExecutable = 2

type internal WatchSpec = 
    private 
    | WatchSpec of string * (string -> bool)
    static member File path = WatchSpec (path, fun _ -> true)
    static member FileAndFilter (path, filter) = WatchSpec (path, filter)
    member x.Path = let (WatchSpec(p, _)) = x in p
    member x.Filter = let (WatchSpec(_, f)) = x in f

type State() =
   let _ = ()

[<TypeProvider>]
type GenerativeTypeProvider(config) as this =
    inherit TypeProviderForNamespaces (config)      
    //let tmpAsm = Assembly.LoadFrom(config.RuntimeAssembly)
    let thisAssembly = Assembly.GetExecutingAssembly()

    //let s = TimeMeasure.start()
    //TimeMeasure.measureTime "Starting"   

    // ==== cachng ============
    (*
    let cachedTypes = Dictionary<string, ProvidedTypeDefinition>()
    let mutable disposals = ResizeArray<(unit -> unit)>()
    let invalidation = new Event<System.EventHandler, _>()
    let mutable invalidationTriggered = 0
    let invalidate() = 
        // FSW can run callbacks in multiple threads - actual event should be raised at most once
        if System.Threading.Interlocked.CompareExchange(&invalidationTriggered, 1, 0) = 0 then
            invalidation.Trigger(null, EventArgs())
    
    let watchPath (spec : WatchSpec) =

        let watcher = 
            let folder = Path.GetDirectoryName spec.Path
            let file = Path.GetFileName spec.Path
            new FileSystemWatcher (folder, file)
        
        watcher.Changed.Add (fun f -> 
            if spec.Filter(f.FullPath) then 
                cachedTypes.Clear()        
                invalidate()
            )
        watcher.Deleted.Add(fun _ -> cachedTypes.Clear(); invalidate())
        watcher.Renamed.Add(fun _ -> cachedTypes.Clear(); invalidate())

        watcher.EnableRaisingEvents <- true
        disposals.Add(fun () -> watcher.Dispose())
     *)
    let invokeScribble pathToFile protocol localRole tempFileName assertionsOn =         
        // Configure command line
        // Add -batch (to speed up Z3 by passing one logical formulae for checking the protocol, 
        // hence the check is fast when the protocol is correct, but slow when it is not. 
            
        (* let batFile = if assertionsOn then """%scribble%""" else """%scribbleno%"""
        
        let scribbleArgs = sprintf """/C %s %s -ass %s -ass-fsm %s -Z3  >> %s 2>&1 """ 
                                    batFile pathToFile protocol localRole tempFileName *)

        // Uncomment below for Scribble without assertions 
        let scribbleArgs = match assertionsOn with 
                           | false -> 
                                let batFile = """%scribbleno%""" 
                                sprintf """/C %s %s -fsm %s %s >> %s 2>&1 """ batFile pathToFile protocol localRole tempFileName
                           | true -> 
                                let batFile = """%scribble%""" 
                                sprintf """/C %s %s -ass %s -ass-fsm %s -Z3  >> %s 2>&1 """ 
                                                batFile pathToFile protocol localRole tempFileName

        let psi = ProcessStartInfo("cmd.exe", scribbleArgs)
        psi.UseShellExecute <- false; psi.CreateNoWindow <- true; 
                                                            
        // Run the cmd process and wait for its completion
        let p = new Process()
        p.StartInfo<- psi;                             
        let res = p.Start(); 
        p.WaitForExit()

        // Read the result from the executed script
        let parsedFile = File.ReadAllText(tempFileName) 
        // TODO:  Fix the parser not to care about starting/trailing spaces!
        let parsedScribble = parsedFile.ToString().Replace("\r\n\r\n", "\r\n")
        parsedScribble            

    let parseCFSM parsedScribble protocol localRole typeAliasing = 
        let str = sprintf """{"code":"%s","proto":"%s","role":"%s"}""" "code" protocol localRole
        match Parsing.getFSMJson parsedScribble str typeAliasing with 
            | Some parsed -> 
                parsed
            | None -> failwith "The file given does not contain a valid fsm"
    
    let rec convertCFSM cfsm (states) (fsmInstance:ScribbleProtocole.Root []) = 
        //let lstates = Set.toList states 
        match states with
        | [] -> cfsm
        | h::t -> 
            //cfsm |>CFSM.addTransition h cfsm
            // convertCFSM  h fsmInstance
            let index = findCurrentIndex  h fsmInstance 
            if index = -1 then 
                convertCFSM cfsm t fsmInstance 
            else 
                let methodName = fsmInstance.[index].Type
                let role = fsmInstance.[index].Partner
                let label = fsmInstance.[index].Label
                let next = fsmInstance.[index].NextState
                match methodName  with 
                | "send" | "receive" | "finish" ->
                    let transition = 
                        match methodName with 
                        | "send" ->  CFSMop.Transition.Send (role, label)
                        | "receive" ->  CFSMop.Transition.Recv (role, label)
                        | "finish" ->  CFSMop.Transition.End (role, label) 
                    let newcfsm = cfsm |> CFSMop.addTransition h transition next 
                    convertCFSM newcfsm t fsmInstance 
                | "choice_receive" | "choice_send" -> 
                    //let listIndexChoice = findSameCurrent fsmInstance.[index].CurrentState fsmInstance
                    convertCFSM cfsm t fsmInstance 
                | _ -> failwith "CFSM transition not supported"
    
    let generateTypes (fsm:string) (variablesMap : Map<string, AssertionParsing.Expr>) (name:string) (parameters:obj[]) = 
    
        let configFilePath = parameters.[0]  :?> string
        let delimitaters = parameters.[1]  :?> string
        let explicitConnection = parameters.[4] :?> bool

        let protocol = ScribbleProtocole.Parse(fsm)
        
        
        let triple = stateSet protocol
        let n, stateSet, firstState = triple
        
        let listTypes = (Set.toList stateSet) |> List.map (fun x -> makeStateType x )
        let ctxInTypes = (Set.toList stateSet) |> List.map (fun x -> makeInContextType x )
        let ctxOutTypes = (Set.toList stateSet) |> List.map (fun x -> makeOutContextType x )
        //let ctxTypes = List.map fst ctxTypesList
        //let ctxDeclTypes = List.map snd ctxTypesList
        let firstStateType = findProvidedType listTypes firstState
        let tupleRole = makeRoleTypes protocol
        let tupleLabel = makeChoiceLabelTypes protocol listTypes (tupleRole |> fst) ctxInTypes ctxOutTypes
        let listOfRoles = makeRoleList protocol
        let labelList = snd(tupleLabel)
        let roleList = snd(tupleRole)
        
        let mutable mapping = Map.empty<string,string list* string list * string list>

        let instance = MappingDelimiters.Parse(delimitaters)
        for elem in instance do
            let label = elem.Label
            let delims = elem.Delims
            let delim1 = delims.Delim1 |> Array.toList
            let delim2 = delims.Delim2 |> Array.toList
            let delim3 = delims.Delim3 |> Array.toList
            mapping <- mapping.Add(label,(delim1,delim2,delim3)) 

        mapping |> DomainModel.modifyMap 

        let naming = Path.Combine(config.ResolutionFolder, configFilePath)
        DomainModel.config.Load(naming)


        //(tupleLabel |> fst) |> Runtime.addLabel
        let agentRouter = createRouter (DomainModel.config)  listOfRoles explicitConnection
        Runtime.addAgent "agent" agentRouter 
        let cache = createCache
        let assertionLookUp = createlookUp
        Runtime.initAssertionDict "agent" assertionLookUp
        Runtime.initCache "cache" cache
        
        let ctor = firstStateType.GetConstructors().[0]                                                               
        let ctorExpr = <@@ obj() @@> //Expr.NewObject(ctor, [])
        let exprCtor = ctorExpr
        let exprStart = <@@ Runtime.startAgentRouter "agent"  @@>
        let exprStart0 = Expr.Sequential(exprStart, <@@ printfn "starting"  @@>)
        let exprStart1 = Expr.Sequential(exprStart0, <@@ Runtime.initRecvHandlers "recv" Runtime.handlersRecvMap @@>)
        let exprStart2 = Expr.Sequential(exprStart1, <@@ Runtime.initSendHandlers "send" Runtime.handlersSendMap @@>)
        let expression = Expr.Sequential(exprStart2,exprCtor)
        
        
        let ty = 
            name 
            |> createProvidedType thisAssembly
            |> addCstor ( <@@ "hey" + string n :> obj @@> |> createCstor [])
            |> addMethod ( expression |> createMethodType "Init" [] firstStateType)
            |> addIncludedTypeToProvidedType ctxInTypes
            |> addIncludedTypeToProvidedType ctxOutTypes
            //|> addIncludedTypeToProvidedType ctxDeclTypes
            |> addIncludedTypeToProvidedType roleList
            |> addIncludedTypeToProvidedType labelList    
            |> addIncludedTypeToProvidedType listTypes
            
            //|> addEndType
        
        //ty.AddMemberDelayed ( fun () -> ProvidedMethid()

        addProperties listTypes listTypes ctxInTypes ctxOutTypes
                      (Set.toList stateSet) 
                      (fst tupleLabel)
                      //Map.empty
                      (fst tupleRole) protocol
        let mutable cfsm = CFSMop.initFsm firstState
        let newCFSM = convertCFSM cfsm (Set.toList stateSet) protocol 
        CFSMop.initCFSMCache "cfsm" newCFSM

        //let assemblyPath = Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
        //let assembly = ProvidedAssembly asse/mblyPath
        //ty.SetAttributes(Type
        //Public ||| TypeAttributes.Class)
        //ty.HideObjectMethods <- truescribble
        //assembly.AddTypes [ty]
        ty

    let createOrUseProvidedTypeDefinition (name:string) (parameters:obj[]) =
       (* match cachedTypes.TryGetValue name with 
        | true, typeDef -> 
            TimeMeasure.measureTime "From the cache"     
            typeDef
        | _ -> 
        *)
        let file = parameters.[0] :?> string
        let protocol = parameters.[1] :?> string
        let localRole = parameters.[2] :?> string
        let configFilePath = parameters.[3] :?> string
        let typeAliasingParam = parameters.[5] :?> string

        // TODO: Fix as to replace only types
        let typeAliasing: Map<string, string> = 
            DotNetTypesMapping.Parse(typeAliasingParam) 
            |>  Array.map (fun s -> (s.Alias, s.Type)) 
            |> Map.ofArray


        let naming = Path.Combine(config.ResolutionFolder, configFilePath)

        match File.Exists(naming) with 
            | true -> DomainModel.config.Load(naming)
            | false -> failwith ("The path to the config folder is not correct: " + config.ResolutionFolder + " " + naming)

        

        let relativePath = Path.Combine(config.ResolutionFolder, file)

        let pathToFile = 
            match File.Exists(file) with 
                | true -> file 
                | false -> 
                     match File.Exists(relativePath) with 
                        | true -> relativePath
                        | false -> failwith "The given file does not exist"
            
        //watchPath (WatchSpec.File pathToFile) 
        //watchPath (WatchSpec.File naming) 

        let scribbleSource = parameters.[6] :?> ScribbleSource
        let assertionsOn = parameters.[8] :?> bool


        let fsm = 
            match scribbleSource with 
                | ScribbleSource.WebAPI ->  
                    let str =  File.ReadAllText(pathToFile) // parse the Scribble code
                    let replace0 = System.Text.RegularExpressions.Regex.Replace(str,"(\s{2,}|\t+)"," ") 
                    let replace2 = System.Text.RegularExpressions.Regex.Replace(replace0,"\"","\\\"")
                    let str = sprintf """{"code":"%s","proto":"%s","role":"%s"}""" replace2 protocol localRole
                    FSharp.Data.Http.RequestString("http://localhost:8083/graph.json", 
                        query = ["json",str] ,
                        headers = [ FSharp.Data.HttpRequestHeaders.Accept HttpContentTypes.Json ],
                        httpMethod = "GET" )
                |ScribbleSource.File -> 
                    //TimeMeasure.measureTime "Before Scribble"
                    //let parsedScribble = code.ToString()
                    let scribbleCode = File.ReadAllText(pathToFile)
                    parseCFSM scribbleCode protocol localRole typeAliasing
                |ScribbleSource.LocalExecutable ->  
                    // let batFile = DomainModel.config.ScribblePath.FileName 
                    let tempFileName = Path.GetTempFileName()       
                    try  
                        let parsedScribble = invokeScribble pathToFile protocol localRole tempFileName assertionsOn
                        //TimeMeasure.measureTime "After Scribble Compiler"  
                        let p = parseCFSM parsedScribble protocol localRole typeAliasing
                        //TimeMeasure.measureTime "After Parsing "
                        p
                    finally 
                        if File.Exists(tempFileName) then File.Delete(tempFileName)
        let variablesMap : Map<string, AssertionParsing.Expr> = Map.empty
        let size = parameters.Length
        //TimeMeasure.measureTime "Before Type generation"
        let genType = generateTypes fsm  variablesMap name parameters.[3..(size-1)]
        //cachedTypes.Add(name, genType)
        //TimeMeasure.measureTime "After Type generation"
        genType

    //let providedType = TypeGeneration.createProvidedType thisAssembly "TypeProviderFile"       
    let parametersTP =  [ProvidedStaticParameter("FileUri",typeof<string>);
                         ProvidedStaticParameter("GlobalProtocol",typeof<string>);
                         ProvidedStaticParameter("Role",typeof<string>);
                         ProvidedStaticParameter("Config",typeof<string>);
                         ProvidedStaticParameter("Delimiter",typeof<string>);
                         ProvidedStaticParameter("TypeAliasing",typeof<string>); 
                         ProvidedStaticParameter("ScribbleSource",typeof<ScribbleSource>, ScribbleSource.File);
                         ProvidedStaticParameter("ExplicitConnection",typeof<bool>, false);
                         ProvidedStaticParameter("AssertionsOn",typeof<bool>, false);
                         ProvidedStaticParameter("AssertionOptimisationsOn",typeof<bool>, false);]
    do 
        (*this.Disposing.Add((fun _ ->
            let disposers = disposals |> Seq.toList
            disposals.Clear()
            for disposef in disposers do try disposef() with _ -> ()
        ))*)

        //providedType.DefineStaticParameters(parametersTP, createOrUseProvidedTypeDefinition)
        let stpTy  = ProvidedTypeDefinition(thisAssembly, ns, "STP", Some typeof<obj>, isErased = true)
        stpTy.DefineStaticParameters(parametersTP, createOrUseProvidedTypeDefinition)
        this.AddNamespace(ns, [stpTy])
        //this.AddNamespace(ns, [providedType])     
        //TimeMeasure.measureTime "Assembly"
    
    //[<CLIEvent>]
    member x.Invalidate = invalidation.Publish

[<assembly:TypeProviderAssembly>]
    do() 
