module ScribbleGenerativeTypeProvider.DomainModel

open System.Threading
open FSharp.Configuration

// Scribble Type 
type ScribbleProtocole = FSharp.Data.JsonProvider<""" [ { "currentState":0 , 
                            "localRole":"StringLocalRole", 
                            "partner":"StringPartner", 
                            "label":"StringLabel",
                            "payload":[{"varName":"someZ", "varType":"someType"}] ,
                            "assertion":"expression", 
                            "inferred":"expressionMap", 
                            "type":"EventType" , 
                            "nextState":0  } ] """>

type ScribbleAPI = FSharp.Data.JsonProvider<""" { "code":"Code", 
                        "proto":"global protocol", 
                        "role":"local role" } """>

type MappingDelimiters = FSharp.Data.JsonProvider<""" [ {"label" : "string", 
                            "delims": {"delim1": ["delim1"],
                            "delim2": ["delim2"],
                            "delim3": ["delim3"] } } ] """>

// workaround for duplication of convertion between type aliases and real type.
type DotNetTypesMapping = FSharp.Data.JsonProvider<""" [ {"alias" : "aliasType", 
                            "type": "RealType"} ] """>

type ISetResult =
    abstract member SetValue : obj -> unit
    abstract member GetTask : unit -> bool

type Buf<'T>() =
    inherit Tasks.TaskCompletionSource<'T>()
    member this.getValue() =
        this.Task.Result
    interface ISetResult with
        member this.SetValue(res) =
            this.SetResult(unbox<'T> res)
        member this.GetTask() = 
            this.Task.IsCompleted


// Agent Type + Messages Types = DU 
type Agent<'T> = MailboxProcessor<'T> 
 
type Message =
    // (serialized message to be put in the tcp Stream , role of the partner)
    |SendMessage of byte [] * string 
    // (serialized message to be put in the tcp Stream , the reply channel , role of the partner)
    |ReceiveMessage of 
        (byte[] * string list) list * string * AsyncReplyChannel<byte [] list> 
    // (serialized message to be put in the tcp Stream , the reply channel , role of the partner)
    |ReceiveMessageAsync of 
        byte[] list * string * string list * AsyncReplyChannel<byte [] list> 
    |Stop 

// TYPE PROVIDER'S ASSEMBLY (for generative type provider) + NAMESPACE + BASETYPE 
let internal ns = "Provided"
// let asm = ProvidedAssembly(Path.ChangeExtension(Path.GetTempFileName(), ".dll"))
let baseType = typeof<obj>

let mutable mappingDelimitateur = 
    Map.empty<string,string list * string list * string list>

let modifyMap delim = 
    mappingDelimitateur <- delim

let getDelims label =
    match (mappingDelimitateur.TryFind label) with
        | None -> 
            ErrorMsg.labelNotInDelim  label 
            |> failwith
        | Some delims -> delims

// Configuration File YAML TP
[<Literal>]
let metaYaml = "Partners:
    - Name: You
      IP: 127.0.0.1
      Port: 5001

LocalRole:
  Name: Me
  IP: 127.0.0.1
  Port: 5000 
"

type ConfigFile = YamlConfig<YamlText=metaYaml>
let config = ConfigFile()

// Result Monad + End type 
type End internal () = class end


type IFailure =
    abstract member Description: string

let createFailure (failure:#IFailure) = failwith failure.Description


