namespace ScribbleGenerativeTypeProvider

// Outside namespaces and modules
open FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes // open the providedtypes.fs file
open System.Reflection // necessary if we want to use the f# assembly
open System.Diagnostics
open System.IO
open FSharp.Data
open FSharp.Configuration
// ScribbleProvider specific namespaces and modules
open ScribbleGenerativeTypeProvider.TypeGeneration
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.CommunicationAgents
open ScribbleGenerativeTypeProvider.Regarder
open ScribbleGenerativeTypeProvider.RefinementTypes
open ScribbleGenerativeTypeProvider.RefinementTypesDict
open ScribbleGenerativeTypeProvider.AsstScribbleParser

open System.Text.RegularExpressions
open System.Text


type ScribbleSource = 
    | WebAPI = 0 
    | File = 1
    | LocalExecutable = 2

[<TypeProvider>]
type GenerativeTypeProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let tmpAsm = Assembly.LoadFrom(config.RuntimeAssembly)

    let generateTypes (fsm:string) (name:string) (parameters:obj[]) = 

        let configFilePath = parameters.[0]  :?> string
        let delimitaters = parameters.[1]  :?> string
        let typeAliasing = parameters.[2] :?> string
        let explicitConnection = parameters.[4] :?> bool
        
        let fsm = 
            let aliases = DotNetTypesMapping.Parse(typeAliasing)
            let mutable prot = fsm
            for alias in aliases do
                 prot <- Regex.Replace(prot,alias.Alias,alias.Type)
            prot

        let protocol = ScribbleProtocole.Parse(fsm)
        let triple= stateSet protocol
        let n,stateSet,firstState = triple
        let listTypes = (Set.toList stateSet) |> List.map (fun x -> makeStateType x )
        let firstStateType = findProvidedType listTypes firstState
        let tupleRole = makeRoleTypes protocol
        let tupleLabel = makeLabelTypes protocol listTypes (tupleRole |> fst)
        let listOfRoles = makeRoleList protocol
        let list1 = snd(tupleLabel)
        let list2 = snd(tupleRole)

        (*let local = parameters.[0]  :?> bool
        let partnersInfos = parameters.[1]  :?> Map<string,string*int>
        let localRoleInfos = parameters.[2]  :?> string*int *)

        
        let mutable mapping = Map.empty<string,string list* string list * string list>

        let instance = MappingDelimiters.Parse(delimitaters)
        for elem in instance do
            let label = elem.Label
            let delims = elem.Delims
            let delim1 = delims.Delim1 |> Array.toList
            let delim2 = delims.Delim2 |> Array.toList
            let delim3 = delims.Delim3 |> Array.toList
            mapping <- mapping.Add(label,(delim1,delim2,delim3)) 

        (*let serializePath = parameters.[1]  :?> string*string
        let deserializePath = parameters.[2]  :?> string*string
        let deserializeChoicePath = parameters.[3]  :?> string*string*)

        mapping |> DomainModel.modifyMap 

        let naming = __SOURCE_DIRECTORY__ + configFilePath
        DomainModel.config.Load(naming)


        (tupleLabel |> fst) |> Regarder.addLabel
        let agentRouter = createRouter (DomainModel.config)  listOfRoles explicitConnection
        Regarder.addAgent "agent" agentRouter 
        let cache = createCache
        let assertionLookUp = createlookUp
        Regarder.initAssertionDict "agent" assertionLookUp
        Regarder.initCache "cache" cache

        addProperties listTypes listTypes (Set.toList stateSet) (fst tupleLabel) (fst tupleRole) protocol

        let ctor = firstStateType.GetConstructors().[0]                                                               
        let ctorExpr = Expr.NewObject(ctor, [])
        let exprCtor = ctorExpr
        let exprStart = <@@ Regarder.startAgentRouter "agent"  @@>
        let expression = Expr.Sequential(exprStart,exprCtor)
            
        let ty = name 
                    |> createProvidedType tmpAsm
                    |> addCstor ( <@@ "hey" + string n @@> |> createCstor [])
                    |> addMethod ( expression |> createMethodType "Start" [] firstStateType)
                    |> addIncludedTypeToProvidedType list2
                    |> addIncludedTypeToProvidedType list1
                    |> addIncludedTypeToProvidedType listTypes
                    //|> addProvidedTypeToAssembly
        
        let assemblyPath = Path.ChangeExtension(System.IO.Path.GetTempFileName(), ".dll")
        printfn "Path = %A " assemblyPath
        printfn "%A" (System.Reflection.Assembly.GetCallingAssembly())
        printfn "%A" (System.Reflection.Assembly.GetEntryAssembly())
        printfn "%A" (System.Reflection.Assembly.GetExecutingAssembly())
//        printfn "%A" (System.Reflection.Assembly.LoadFrom())
        let assembly = ProvidedAssembly assemblyPath
        ty.SetAttributes(TypeAttributes.Public ||| TypeAttributes.Class)
        ty.HideObjectMethods <- true
        assembly.AddTypes [ty]
        ty

    let createTypeWithFSM (name:string) (parameters:obj[]) =
        let fsm = parameters.[0]  :?> string  (* this is used if we want to assure that the type of the parameter
        we are grabbing is a string : DOWNCASTING . Which also means type verification at runtime and not compile time *)
        let size = parameters.Length
        generateTypes fsm name parameters.[1..(size-1)]

    let createTypeWithFile (name:string) (parameters:obj[]) =
        
        let file = parameters.[0] :?> string
        let protocol = parameters.[1] :?> string
        let localRole = parameters.[2] :?> string
        let configFilePath = parameters.[3] :?> string

        let naming = __SOURCE_DIRECTORY__ + configFilePath
        DomainModel.config.Load(naming)

        let scribbleSource = parameters.[6] :?> ScribbleSource

        let relativePath = __SOURCE_DIRECTORY__ + file

        let pathToFile = match File.Exists(file) with 
                        | true -> file 
                        | false -> match File.Exists(relativePath) with 
                                    | true -> relativePath
                                    | false -> failwith "The given file does not exist"
        let code = File.ReadAllText(pathToFile)

        (*    match (File.Exists(file) , File.Exists(relativePath)) with
            | true , false -> File.ReadAllText(file)
            | false , true -> File.ReadAllText(relativePath)                               
            | true , true -> File.ReadAllText(relativePath)
            | false, false ->  
                File.ReadAllText(relativePath)
        *)

        let fsm = match scribbleSource with 
                    | ScribbleSource.WebAPI ->  
                        let str = code.ToString()
                        let replace0 = System.Text.RegularExpressions.Regex.Replace(str,"(\s{2,}|\t+)"," ") 
                        let replace2 = System.Text.RegularExpressions.Regex.Replace(replace0,"\"","\\\"")
                        let str = sprintf """{"code":"%s","proto":"%s","role":"%s"}""" replace2 protocol localRole
                        FSharp.Data.Http.RequestString("http://localhost:8083/graph.json", 
                            query = ["json",str] ,
                            headers = [ FSharp.Data.HttpRequestHeaders.Accept HttpContentTypes.Json ],
                            httpMethod = "GET" )
                    |ScribbleSource.File -> 
                        let parsedScribble = code.ToString()
                        let str = sprintf """{"code":"%s","proto":"%s","role":"%s"}""" "code" protocol localRole
                        match Parsing.getFSMJson parsedScribble str with 
                            | Some parsed -> parsed
                            | None -> failwith "The file given does not contain a valid fsm"
                    |ScribbleSource.LocalExecutable ->  
                        //redirect the output stream
                        let batFile = DomainModel.config.ScribblePath.FileName 
                        let tempFileName = Path.GetTempFileName()        
                        
                        try                                 
                            // Configure command line
                            let scribbleArgs = sprintf """/C %s %s -ass %s -ass-fsm %s -Z3 >> %s 2>&1 """ 
                                                        batFile pathToFile protocol localRole tempFileName

                            // Incomment below for Scribble without assertions 
                            //let scribbleArgs = sprintf """/C %s %s -fsm %s %s >> %s 2>&1 """ 
                            //                               batFile pathToFile protocol localRole tempFileName

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
                            let str = sprintf """{"code":"%s","proto":"%s","role":"%s"}""" "code" protocol localRole

                            match Parsing.getFSMJson parsedScribble str with 
                                | Some parsed -> parsed
                                | None -> failwith (sprintf "The file given does not contain a valid fsm: %s" parsedScribble)
                        finally 
                            if File.Exists(tempFileName) then File.Delete(tempFileName)

        let size = parameters.Length
        generateTypes fsm name parameters.[3..(size-1)]
    
    //let basePort = 5000       
            
    let providedTypeFSM = TypeGeneration.createProvidedType tmpAsm "TypeProviderFSM"
    let providedTypeFile = TypeGeneration.createProvidedType tmpAsm "TypeProviderFile"
    
    let parametersFSM = [ProvidedStaticParameter("Protocol",typeof<string>);
                         ProvidedStaticParameter("Config",typeof<string>);
                         ProvidedStaticParameter("Delimiter",typeof<string>);
                         ProvidedStaticParameter("TypeAliasing",typeof<string>)]
                        (* ProvidedStaticParameter("SerializeMessagePath",typeof<string*string>);
                         ProvidedStaticParameter("DeserializeMessagePath",typeof<string*string>);
                         ProvidedStaticParameter("DerializeChoicePath",typeof<string*string>)]*)
    
    let parametersFile=  [ProvidedStaticParameter("File Uri",typeof<string>);
                          ProvidedStaticParameter("Global Protocol",typeof<string>);
                          ProvidedStaticParameter("Role",typeof<string>);
                          ProvidedStaticParameter("Config",typeof<string>);
                          ProvidedStaticParameter("Delimiter",typeof<string>);
                          ProvidedStaticParameter("TypeAliasing",typeof<string>); 
                          ProvidedStaticParameter("ScribbleSource",typeof<ScribbleSource>);
                          ProvidedStaticParameter("ExplicitConnection",typeof<bool>); ]
                         (* ProvidedStaticParameter("SerializeMessagePath",typeof<string*string>);
                          ProvidedStaticParameter("DeserializeMessagePath",typeof<string*string>);
                          ProvidedStaticParameter("DerializeChoicePath",typeof<string*string>)]*)

    do 
        providedTypeFSM.DefineStaticParameters(parametersFSM,createTypeWithFSM)
        providedTypeFile.DefineStaticParameters(parametersFile,createTypeWithFile)
        
        this.AddNamespace(ns, [providedTypeFSM])
        this.AddNamespace(ns, [providedTypeFile])
[<assembly:TypeProviderAssembly>]
    do()