module ScribbleGenerativeTypeProvider.CommunicationAgents

// Outside namespaces and modules
open System
open System.IO
open System.Net
open System.Text
open System.Net.Sockets
open System.Collections.Generic
open Microsoft.FSharp.Quotations
open System.Runtime.Serialization
open System.Runtime.Serialization.Formatters.Binary

// ScribbleProvider specific namespaces and modules
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.Util
open ScribbleGenerativeTypeProvider.Util.ListHelpers


let isDummyVar (x:string) = x.StartsWith("_")
type VarCache()=
    let data = Dictionary<string,_>()
    member x.RuntimeOperation() = data.Count  
      
    member x.Add(k:string, v:int) = 
      if not (isDummyVar k) then 
        match data.ContainsKey(k) with 
        | true -> data.Item(k) <- v
        | _ -> data.Add(k, v) 
    
    member x.Get(k:string) = 
        match data.TryGetValue(k) with 
        | true, value -> value
        | _ -> failwith (ErrorMsg.valueNotInCache k)

    member x.Print() = 
        [for key in data.Keys do 
            sprintf "%s -- %A " key (data.Item(key)) 
            |> ignore
        ]

let createCache = new VarCache()
exception TooManyTriesError of string

let serializeBinary<'a> (x :'a) =
    let binFormatter = new BinaryFormatter()
    use stream = new MemoryStream()
    binFormatter.Serialize(stream, x)
    stream.ToArray()

let deserializeBinary<'a> (arr : byte[])=
    let binFormatter = new BinaryFormatter()
    use stream = new MemoryStream(arr)
    binFormatter.Deserialize(stream) :?> 'a

let internal moreLists (labels:byte[] list) =
    let rec aux acc (list1 : byte[] list) =
        match list1 with
        |[] -> acc
        |hd::tl -> 
            let encode = new System.Text.UTF8Encoding()
            // TODO : FIX Num 1 TMP
            let str1 = encode.GetString(hd.[0..hd.Length-2])
            let str2 = encode.GetString(hd)
            let tryDelims1 = DomainModel.mappingDelimitateur.TryFind str1
            let tryDelims2 = DomainModel.mappingDelimitateur.TryFind str2
            let listDelim,_,_ =
                match tryDelims1, tryDelims2 with
                | None , Some delims -> delims
                | Some delims , None -> delims
                | _ , _ -> failwith (ErrorMsg.wrongDelim str1 str2)
            aux (listDelim::acc) tl
    Debug.print  "Labels :" labels
    aux [] labels


let internal readLab (s : NetworkStream) (labels : byte[] list) =
    let listsDelim = moreLists labels
    let decode = new UTF8Encoding()
    let dis = new BinaryReader(s)
    let rec aux acc = 
        let tmp = dis.ReadByte()
        let value = decode.GetString([|tmp|])
        // TODO : FIX Num 2 TMP
        if (isInOne listsDelim value) 
        then (acc,[|tmp|])
        else aux (Array.append acc [|tmp|]) 
    aux [||]

let readPay (s:Stream) (label:string) types = 
    // TODO : FIX Num 3 TMP
    let str1 = label.[0..(label.Length-2)] 
    let str2 = label
    let tryDelims1 = DomainModel.mappingDelimitateur.TryFind str1
    let tryDelims2 = DomainModel.mappingDelimitateur.TryFind str2
    let _,listDelPay,listDelEnd = 
        match tryDelims1, tryDelims2 with
        | None , Some delims -> delims
        | Some delims , None -> delims
        | _ , _ -> failwith (ErrorMsg.wrongDelim str1 str2)

    let dis = new BinaryReader(s)
    let decode = new UTF8Encoding()
    let rec aux accList accArray leftTypes =
        match leftTypes with
        | [] -> accList |> List.rev
        | hd::tl ->
            let tmp = dis.ReadByte()
            let value = decode.GetString([|tmp|])
            Debug.print value tmp
            
            if (List.exists (fun elem -> elem = value) listDelEnd) 
            then (accArray::accList) |> List.rev 
            elif (List.exists (fun elem -> elem = value) listDelPay) 
            then aux (accArray::accList) [||] tl
            else aux accList (Array.append accArray [|tmp|]) (hd::tl)
    in aux [] [||] types

type IRouter = 
    abstract member UpdateAgentSenders : string -> TcpClient -> unit
    abstract member UpdateAgentReceiver : string -> TcpClient -> unit

type AgentSender(ipAddress,port, localRole:string, role:string) =
    let mutable localRole = localRole
    let mutable role = role
    let mutable tcpClient:TcpClient = null
    [<DefaultValue>] val mutable router : IRouter

    let waitSynchronously timeout =
        async { do! Async.Sleep(timeout*1000)} 
    
    // 5 Tries of 3 seconds and then double the time at each try 
    let connect address p (tcpClient:TcpClient) (router:IRouter) =
        let rec aux timeout count =
            let tries = 5
            try
                match count with
                |n when n<tries ->  
                    tcpClient.Connect(IPAddress.Parse(address),p)
                |_ -> 
                    tcpClient.Connect(IPAddress.Parse(address),p)
                    if not(tcpClient.Connected) then
                        raise (TooManyTriesError(""))
            with
            | :? System.ArgumentException as ex -> 
                printfn "Argument Exception: %s"  ex.Message
            | :? System.Net.Sockets.SocketException as ex ->  
                printfn "Socket Exception error code: %d"  ex.ErrorCode
                timeout |> waitSynchronously |> Async.RunSynchronously
                aux (timeout*2) (count+1)
            | :? System.ObjectDisposedException as ex -> 
                printfn "Object Disposed Exception: %s"  ex.Message
            | TooManyTriesError(str) -> 
                printfn "Too Many Tries Error: %s" str
        
        in aux 3 0

    let send (stream:NetworkStream) (actor:Agent<Message>) =
        let rec loop () = async {
            let! msg = actor.Receive()
            match msg with
            |ReceiveMessageAsync _ ->
                ()
                return! loop()
            |ReceiveMessage _ -> 
                () 
                return! loop()      
            |SendMessage (message,role) -> 
                do! stream.AsyncWrite(message)
                return! loop()
            |Stop -> stream.Close()
            }
        in loop()
 
    let mutable agentSender = None 

    /// If None raises an exception Error due to using this method before 
    /// the Start method in the type provider 
    member this.SendMessage(message) =
        match (agentSender:Option<Agent<Message>>) with
            |None -> ()
            |Some sending -> 
                sending.Post(Message.SendMessage message)
    
    /// this.Start should be called when we have request!
    member this.SetRouter router = 
        this.router <- router
        
    member this.Stop () = 
        let stream = tcpClient.GetStream().Close()
        tcpClient.Close()

    /// Raise an exception due to trying to connect and parsing the IPAddress
    member this.Start() = 
        let tcpClientSend = new TcpClient()
        connect ipAddress port tcpClientSend this.router
        tcpClient <- tcpClientSend
        let stream = tcpClientSend.GetStream()

        let serializedRole = localRole + ";"
        let msg =  Encoding.ASCII.GetBytes(serializedRole)
        stream.Write(msg, 0, msg.Length)
        agentSender <- Some (Agent.Start(send stream))

    /// Raise an exception due to trying to connect and parsing the IPAddress
    member this.Accept(tcpClient:TcpClient) = 
        let stream = tcpClient.GetStream()        
        agentSender <- Some (Agent.Start(send stream))
    
type AgentReceiver(ipAddress,port, roles: string list) =

    let server = new TcpListener(IPAddress.Parse(ipAddress),port)
    let mutable clientMap = Map.empty
    let mutable roles = roles
    [<DefaultValue>] val mutable parentRouter : IRouter
 
    let rec waitForCancellation str count =
        match count with
        |n when n=0 -> ()
        |n when n>0 -> 
            if not(clientMap.ContainsKey str) then
                Async.RunSynchronously(Async.Sleep 1000)
                waitForCancellation str (count-1)
            else ()
        |_ -> ()     
 
    /// CHANGE BELOW BY READING THE ROLE IN ANOTHER Map<role:string,(IP,PORT)>
    /// Note that here we do not actually know which roles are connected. 
    /// We do the actual binding when we receive the first message, 
    /// that is role for which role
    let binding (tcpListenerReceive:TcpListener) 
            (router:IRouter) (actor:Agent<Message>) =
        let rec loop () = async {
            let client = tcpListenerReceive.AcceptTcpClient()
            let stream = client.GetStream()
            let endpointClient = client.Client.RemoteEndPoint.ToString()
            Debug.print "Add a stream for role" (roles.Length)
            let dis = new BinaryReader(stream)
            let decode = new UTF8Encoding()
            let mutable value = ""
            let sb = new StringBuilder()
            
            while not stream.DataAvailable do 
                Debug.print "waiting for data" value
            
            let res = 
                while stream.DataAvailable && value<>";" do 
                    let tmp = dis.ReadByte()
                    value <- decode.GetString([|tmp|])
                    if value<>";" 
                    then sb.Append(value) |> ignore
            
            let readRole = sb.ToString()
            clientMap <- clientMap.Add(readRole,stream)
            return! loop()
            }
        in loop()
 
    let receive (actor:Agent<Message>) =
        let rec loop () = async {
            let! msg = actor.Receive()
            match msg with
            |SendMessage (message,role)->
                ()  // Raise an exception Error due to bad coding in the type provider
                return! loop()      
            |ReceiveMessageAsync (message,role,listTypes,channel) -> 
                // The UnMarshalling is done outside the Agent Routing Architecture NOT HERE.
                let fakeRole = role
                Debug.print "Check ClientMap :" clientMap
                if not(clientMap.ContainsKey(fakeRole)) then
                    // Change th number
                    waitForCancellation fakeRole 50 |> ignore 
                let stream = clientMap.[fakeRole]
                // DESERIALIZER BIEN LA
                let decode = new System.Text.UTF8Encoding()
                let (label,delim) = readLab stream message
                match label with
                |msg when 
                    (message |> isInList <| (Array.append msg delim)) 
                    |> not 
                    -> failwith ErrorMsg.wrongLable
                | _ ->  
                    let payloads = 
                        readPay stream 
                            (decode.GetString(label)) 
                            listTypes
                    let list1 = label::payloads
                    channel.Reply(list1)
                return! loop()

            |ReceiveMessage (message,role,channel) ->
                Debug.print "Check ClientMap :" clientMap
                if not(clientMap.ContainsKey(role)) then
                    // Todo: Change the number
                    waitForCancellation role 50 |> ignore 
                Debug.print "Check ClientMap :" clientMap
                let stream = clientMap.[role]
                let decode = new System.Text.UTF8Encoding()
                Debug.print "Wait Read Label" stream.DataAvailable
                let mutable succeed = false
                let (label,delim) = 
                    try 
                        let res = 
                            readLab stream (message |> List.map fst)
                        succeed <- true
                        res
                    with
                    | e -> 
                        printfn "The error is:%s" (e.ToString())
                        succeed <- false
                        [||],[||]
                match label with
                |msg when ((message |> List.map fst) 
                            |> isInList <| (Array.append msg delim)) 
                            |> not 
                    -> 
                    Debug.print "wrong label read :" (label,message)
                    if stream.DataAvailable then 
                        failwith ErrorMsg.wrongLable
                | _ ->  
                    let listTypes = message |> List.map snd
                    let labelAssociatedTypes =
                        let label = Array.append label delim 
                        let types = 
                            message 
                            |> List.find (fun el -> (el |> fst) = label) 
                            |> snd
                        types
                    let payloads = 
                        readPay 
                            stream (decode.GetString(label)) 
                            labelAssociatedTypes
                    let list1 = label::payloads
                    channel.Reply(list1)
                return! loop()
                    
            }
        in loop()
 
    let mutable agentReceiver = None
    member this.SetRouter (router:IRouter) = 
        this.parentRouter <- router

    member this.Start()=
        server.Start()
        Debug.print "Start the router" this.parentRouter
        Agent.Start(binding server this.parentRouter) |> ignore
        agentReceiver <- Some (Agent.Start(receive))

    /// To Closes the listener
    /// To be done in the finish ProvidedMethod 
    ///that represent the Ending process in Session Types.
    member this.Stop() =
        for client in clientMap do
            client.Value.Close()
        server.Stop()

    member thid.UpdateClientMap(role:string, client:TcpClient)= 
        if not (clientMap.ContainsKey(role)) then
            Debug.print "Update client map " (role.ToString())
            clientMap <- clientMap.Add(role,client.GetStream())

    // Be carefull with this function: IF IT'S NONE RAISE AN EXCEPTION
    member this.ReceiveMessageAsync(message) =
        match agentReceiver with
        |Some receive -> 
            receive.Post(Message.ReceiveMessageAsync message )                            
        |None -> 
            failwith ErrorMsg.agentNotInstantiated
                     
    member this.ReceiveMessage(message) =
        let (msg,role,ch) = message
        match agentReceiver with
        |Some receive -> 
            receive.PostAndReply(fun ch -> 
                Message.ReceiveMessage (msg,role,ch))
        |None -> failwith ErrorMsg.agentNotInstantiated


                     
/// In case of receive: message is the serialized version of the Type
/// replyMessage is the message really received from the network 
/// In case of send: message is the serialized message that needs to be sent
type AgentRouter(explicitConnection:bool) =
        let explicitConnection = explicitConnection
        let mutable (payloadChoice:byte[] list) = []

        let sendAndReceive (agentMapping:Map<string,AgentSender>) 
                (agentReceiver: AgentReceiver) (agentRouter:Agent<Message>) =
            let rec loop () = async {
                let! msg = agentRouter.Receive()
                match msg with
                |SendMessage (message,role) ->
                    let agentSender = agentMapping.[role]
                    agentSender.SendMessage(message,role) 
                    return! loop()
                |ReceiveMessageAsync (message, role, listTypes, channel) -> 
                    agentReceiver.ReceiveMessageAsync(
                        message, role, listTypes, channel) 
                    return! loop()
                |ReceiveMessage (message,role,channel) -> 
                    let message = 
                        agentReceiver.ReceiveMessage(message,role,channel)  
                    channel.Reply(message)                                                                                  
                    return! loop()
                }
            in loop()
        
        
        [<DefaultValue>] val mutable agentRouter: MailboxProcessor<Message>
        [<DefaultValue>] val mutable agentMapping:Map<string,AgentSender>
        [<DefaultValue>] val mutable agentReceiver: AgentReceiver

        member this.StartAgentRouter(agentMapping:Map<string,AgentSender>,
                                     agentReceiver: AgentReceiver) = 
            
            this.agentMapping<- agentMapping
            this.agentReceiver <- agentReceiver
            this.agentRouter <- Agent.Start(sendAndReceive 
                agentMapping agentReceiver)    

        member this.RequestConnection (roleName :string) = 
            Debug.print "In request for role" roleName
            let role = this.agentMapping.[roleName]
            role.Start()

        member this.AcceptConnection (roleName :string) = 
            // This is wrong. It does request and not accept
            Debug.print "In waiting state to connect to role " roleName
            ()

        member this.Start() =
            printfn "before agentReceive.start()"
            this.agentReceiver.Start()
            printfn "After agentReceive.start()"
            if (not explicitConnection) then 
                for sender in this.agentMapping do
                    sender.Value.Start()
                    //connectedAgents.[sender.Key] = true |> ignore
            Debug.print "Starting the connection..."

        member this.Stop() =
            for sender in this.agentMapping do
                    sender.Value.Stop()
            this.agentReceiver.Stop()
            Debug.print "closing the connections"
                   
        member this.SendMessage(message) =
            Debug.print "SendMessage : Post to the write role = " message
            this.agentRouter.Post(Message.SendMessage message)
   
        member this.ReceiveMessage(messageAndType) =
            let (msg,role) = messageAndType
            let replyMessage = 
                this.agentRouter.PostAndReply(
                    fun ch -> Message.ReceiveMessage (msg,role,ch))
            payloadChoice <- replyMessage.Tail
            replyMessage

        member this.ReceiveMessageAsync(message) = 
            let (msg,role,listTypes) = message
            let replyMessage = 
                this.agentRouter.PostAndAsyncReply(fun ch -> 
                    Message.ReceiveMessageAsync (msg,role,listTypes,ch))
            replyMessage            

        member this.ReceiveChoice() =
            Debug.print "Go through A choice!!!" (payloadChoice)
            payloadChoice

        interface IRouter with 
            member this.UpdateAgentSenders role  tcpClient = 
                this.agentMapping.[role].Accept(tcpClient)

            member this.UpdateAgentReceiver role tcpClient = 
                this.agentReceiver.UpdateClientMap(role, tcpClient)

// Functions that generate the agents.

let isIn (list:string list) (localRole:string) =
    list |> List.exists (fun x -> x=localRole) 

let private createReceiver (ipAddress:string) (port:int) 
    (roles: string list) = new AgentReceiver(ipAddress,port, roles)

let createMapSender (partnersInfo: IList<ConfigFile.Partners_Item_Type>) 
        (listRoles:string list) (localRole:string) =
    let mutable mapping = Map.empty<string,AgentSender>
    for partner in partnersInfo do
        match (listRoles |> isIn <| partner.Name) with
            | false -> failwith (ErrorMsg.wrongRole (partner.Name))
            | true -> 
                mapping <- mapping.Add(
                    partner.Name, 
                    new AgentSender(partner.IP, partner.Port, 
                                    localRole, partner.Name)
                    )
    mapping

let createRouter (configInfos:ConfigFile) 
        (listRoles:string list) (explicitConnection: bool) =
    let lengthList = listRoles.Length
    let configSize = configInfos.Partners.Count + 1
    match (configSize = lengthList) with
    | false -> failwith ErrorMsg.wrongNumberOfRoles
    | true ->
        match (listRoles |> isIn <| configInfos.LocalRole.Name) with
        |false -> failwith (ErrorMsg.wrongRole (configInfos.LocalRole.Name))
        |true -> 
            let router = new AgentRouter(explicitConnection)
            let mapAgentSender = 
                createMapSender configInfos.Partners 
                    listRoles configInfos.LocalRole.Name
            
            for sender in mapAgentSender 
                do  sender.Value.SetRouter router

            let ip = configInfos.LocalRole.IP
            let port = configInfos.LocalRole.Port
            let partners = 
                listRoles 
                |> List.filter (fun x -> x<>configInfos.LocalRole.Name)        
            Debug.print "Infos For Agent Sender : %A" (mapAgentSender,
                configInfos.Partners,listRoles)
            Debug.print "Infos For Agent Receiver : %A" 
                (configInfos.LocalRole.IP,
                 configInfos.LocalRole.Port, configInfos.LocalRole.Name)
            let receiver = 
                createReceiver configInfos.LocalRole.IP 
                    configInfos.LocalRole.Port partners 
            receiver.SetRouter router
            router.StartAgentRouter(mapAgentSender, receiver)
            router
    