module ScribbleGenerativeTypeProvider.Runtime 

open ProviderImplementation.ProvidedTypes
open ScribbleGenerativeTypeProvider.CommunicationAgents
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.RefinementTypesDict

// The router map stores only one element (called agent), that acts as a router in the system. 
// It redirects the messages to the internal actors
let mutable routerMap = Map.empty<string,AgentRouter>
// TODO: This does not look safe!
let mutable changed = false
let mutable mLabel = Map.empty<string,ProvidedTypeDefinition>

let addAgent str agent =
    if not(changed) then
        routerMap <- routerMap.Add(str,agent)
        changed <- true

let addLabel mapping =
    mLabel <- mapping

let getLabelType (labelRead:string) =
    //printfn "getLabelType : %A" (mLabel,labelRead) 
     
    mLabel.[labelRead]

let startAgentRouter agent =
    printfn "Before adding agent"
    routerMap.Item(agent).Start() 
    printfn "After adding agent"
    ()

let acceptConnection agent role =
    routerMap.Item(agent).AcceptConnection(role)

let requestConnection agent role =
    routerMap.Item(agent).RequestConnection(role)

let stopMessage agent = 
    routerMap.Item(agent).Stop()

let sendMessage agent message role =
    routerMap.Item(agent).SendMessage(message,role)

let receiveMessageAsync agent message role listTypes =
    routerMap.Item(agent).ReceiveMessageAsync(message,role,listTypes) 

let receiveMessage agent message role listTypes =
    let messageAndTypes = List.zip message listTypes
    routerMap.Item(agent).ReceiveMessage(messageAndTypes,role) 

let receiveChoice agent =
    routerMap.Item(agent).ReceiveChoice()

let mutable cache = Map.empty<string,VarCache>
let initCache name (newCache:VarCache) = 
    cache <- cache.Add(name, newCache)

let printCount name= 
    cache.Item(name).Print()

let addVarsToCache name (keys: string list) (values:int []) = 
    keys 
    |> List.iteri (fun i key ->  cache.Item(name).Add(key, values.[i]))

let addVarsBufsToCache name (keys: string list) (values:Buf<int> []) = 
    keys 
    |> List.iteri (fun i key ->  
        cache.Item(name).Add(key, values.[i].getValue()))

let getFromCache name elem =
    cache.Item(name).Get(elem)
 

let mutable assertionLookUp = Map.empty<string, LoopUpDict>
let initAssertionDict name (assertLookip:LoopUpDict) = 
    assertionLookUp <- assertionLookUp.Add(name, assertLookip)
 
let getAssertionIndex name  = 
    assertionLookUp.Item(name).Index()

let addToAssertionDict name elem = 
    assertionLookUp.Item(name).addToDict(elem)

let runFooFunction name foo = 
    assertionLookUp.Item(name).runFooFunction(foo)

let addArgValueToAssertionDict name argName rcv = 
    assertionLookUp.Item(name).addArgValue argName rcv 

let setResults results (bufs:ISetResult []) = 
    Seq.zip results (Array.toSeq bufs) 
    |> Seq.iter (fun (res,buf:ISetResult) -> buf.SetValue(res))