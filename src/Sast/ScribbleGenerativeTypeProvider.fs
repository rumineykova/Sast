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

    let invokeScribble pathToFile protocol localRole tempFileName assertionsOn =         
      let scribbleArgs = 
        match assertionsOn with 
          | false -> 
              let batFile = """%scribbleno%""" 
              sprintf """/C %s %s -fsm %s %s >> %s 2>&1 """ 
                batFile pathToFile protocol localRole tempFileName
          | true -> 
              let batFile = """%scribble%""" 
              sprintf """/C %s %s -assrt  -fsm %s %s -z3  >> %s 2>&1 """ 
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
          | Some parsed -> parsed
          | None -> failwith "The file given does not contain a valid fsm"

    let parseScribble scribbleSource scrFile protocol 
      localRole typeAliasing assertionsOn =  
      
      // TODO: Fix as to replace only types
      let typeAliasing: Map<string, string> = 
          DotNetTypesMapping.Parse(typeAliasing) 
          |>  Array.map (fun s -> (s.Alias, s.Type)) 
          |> Map.ofArray
     
      let relativePath = Path.Combine(config.ResolutionFolder, scrFile)
      let pathToFile = 
        match File.Exists(scrFile) with 
          | true -> scrFile 
          | false -> 
            match File.Exists(relativePath) with 
              | true -> relativePath
              | false -> failwith "The given file does not exist"

      match scribbleSource with 
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
    
    let parseDelimeters delimitaters = 
      // handle configFile delim parameter (used for serialisation)
      let mutable mapping = Map.empty<string,string list* string list * string list>
      let instance = MappingDelimiters.Parse(delimitaters)
      for elem in instance do
          let label = elem.Label
          let delims = elem.Delims
          let delim1 = delims.Delim1 |> Array.toList
          let delim2 = delims.Delim2 |> Array.toList
          let delim3 = delims.Delim3 |> Array.toList
          mapping <- mapping.Add(label,(delim1,delim2,delim3)) 
      DomainModel.modifyMap mapping
    
    let parseConfigFile configFilePath = 
      let configFile = Path.Combine(config.ResolutionFolder, configFilePath)
      match File.Exists(configFile) with 
          | true -> DomainModel.config.Load(configFile)
          | false -> failwith ("The path to the config folder is not correct: " + config.ResolutionFolder + " " + configFile)
      
    let generateTypes protocol (name:string) 
      (explicitConnections) states firstState= 
      let states_size = states |> Seq.length  
      // create initial types
      let listTypes = (Set.toList states) |> List.map (fun x -> makeStateType x )
      let ctxInTypes = (Set.toList states) |> List.map (fun x -> makeInContextType x )
      let ctxOutTypes = (Set.toList states) |> List.map (fun x -> makeOutContextType x )
      let firstStateType = findProvidedType listTypes firstState
      let tupleRole = makeRoleTypes protocol
      let roleList = snd(tupleRole)
      let tupleLabel = makeChoiceLabelTypes protocol listTypes (tupleRole |> fst) ctxInTypes ctxOutTypes
      let listOfRoles = makeRoleList protocol
      let labelList = snd(tupleLabel)
     
      // initialise persistent objects (teh Agent router and the cache for assertion values)
      Runtime.initialise(listOfRoles, explicitConnections) 
       
      // create the expression for Initialising the TP
      let exprInit = 
        <@@ 
          printfn "starting"  
          Runtime.initRecvHandlers "recv" Runtime.handlersRecvMap 
          Runtime.initSendHandlers "send" Runtime.handlersSendMap 
          obj() 
          @@> 
        
      // create the expression for ctarting the protocol
      let exprStart = 
        <@@ CFSMModel.runFromInit (CFSMModel.getCFSM "cfsm") 
        @@>
        
      let host_type = 
        name 
        |> createProvidedType thisAssembly
        |> addCstor ( <@@ string states_size :> obj @@> |> createCstor [])
        |> addMethod ( exprInit |> createMethodType "Init" [] firstStateType)
        |> addMethod ( exprStart |> createMethodType "Start" [] firstStateType)
        |> addIncludedTypeToProvidedType ctxInTypes
        |> addIncludedTypeToProvidedType ctxOutTypes
        |> addIncludedTypeToProvidedType roleList
        |> addIncludedTypeToProvidedType labelList    
        |> addIncludedTypeToProvidedType listTypes
        
      addProperties listTypes listTypes ctxInTypes ctxOutTypes
        (Set.toList states) (fst tupleLabel) (fst tupleRole) protocol

      host_type

    let createOrUseProvidedTypeDefinition (name:string) (parameters:obj[]) =
        // parse TP static parameters 
        let scrFile = parameters.[0] :?> string
        let protocol = parameters.[1] :?> string
        let localRole = parameters.[2] :?> string
        let configFilePath = parameters.[3] :?> string
        let delimeters = parameters.[4] :?> string
        let typeAliasing = parameters.[5] :?> string
        let scribbleSource = parameters.[6] :?> ScribbleSource
        let explicit_conn = parameters.[7] :?> bool
        let assertionsOn = parameters.[8] :?> bool

        parseConfigFile configFilePath
        parseDelimeters delimeters

        // Get a json_cfsm representation of the Scribble file 
        let fsm_as_straing = 
          parseScribble scribbleSource scrFile protocol 
            localRole typeAliasing assertionsOn
        let fsm_as_json = ScribbleProtocole.Parse(fsm_as_straing)   
        let _, states, firstState = stateSet fsm_as_json

        // Convert the parsed CFSM to our internal runtime CFSMModel
        let cfsm = 
          CFSMAdapter.convertCFSM (CFSMModel.initFsm firstState) 
            (Set.toList states) fsm_as_json false
        CFSMModel.init "cfsm" cfsm

        let genType = generateTypes fsm_as_json name explicit_conn states firstState
        genType

    let parametersTP =  
      [ProvidedStaticParameter("FileUri",typeof<string>);
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
      let stpTy  = ProvidedTypeDefinition(thisAssembly, ns, "STP", Some typeof<obj>, isErased = true)
      stpTy.DefineStaticParameters(parametersTP, createOrUseProvidedTypeDefinition)
      this.AddNamespace(ns, [stpTy])

[<assembly:TypeProviderAssembly>]
  do() 
