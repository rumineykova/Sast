module ScribbleGenerativeTypeProvider.Regarder 

open ProviderImplementation.ProvidedTypes
open ScribbleGenerativeTypeProvider.CommunicationAgents
open ScribbleGenerativeTypeProvider.DomainModel
open System.Collections.Concurrent

// The router map stores only one element (called agent), that acts as a router in the system. 
// It redirects the messages to the internal actors
let mutable routerMap = Map.empty<string,AgentRouter>
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
    routerMap.Item(agent).Start()

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


open Microsoft.FSharp.Quotations
let mutable cache = Map.empty<string,VarCache>

let initCache name (newCache:VarCache) = 
    cache <- cache.Add(name, newCache)

let printCount name= 
    cache.Item(name).Print()

let addVars name (keys: string list) (values:int []) = 
    //List.zip keys values |> List.iter (fun (key, value)->  cache.Item(name).Add(key, value))
    keys |> List.iteri (fun i key ->  cache.Item(name).Add(key, values.[i]))

let addVarsBufs name (keys: string list) (values:Buf<int> []) = 
    //List.zip keys values |> List.iter (fun (key, value)->  cache.Item(name).Add(key, value))
    keys |> List.iteri (fun i key ->  cache.Item(name).Add(key, values.[i].getValue()))

open ScribbleGenerativeTypeProvider.RefinementTypesDict

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