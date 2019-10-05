module ScribbleGenerativeTypeProvider.ITransport

type LRuntime(isMock, endpoint:Comms.CommunicationChannel) =
  member x.isMock = isMock 
  member x.endpoint = endpoint
  member x.startClient = 
    let ip = System.Net.IPAddress.Parse("127.0.0.1")
    let endpoint = Comms.makeTcpServerEndPoint "S" ip 4001
    x.endpoint = endpoint
  
  member x.startServer = 
     let ip = System.Net.IPAddress.Parse("127.0.0.1")
     let endpoint = Comms.makeTcpClientEndPoint "C" "127.0.0.1" 4001
     x.endpoint = endpoint

  member x.recv role label = 
    printfn "I am receiving and moving to next state: %s and %s " label role 
    if x.isMock then 1
    else
      let result = Runtime.receiveMessage "agent" [] role []      
      let decode = new System.Text.UTF8Encoding() 
      let labelRead = decode.GetString(result.[0]) 
      1
     
  member x.send role label (payload:int) = 
    if x.isMock then ()
    else 
      let buf = System.BitConverter.GetBytes(payload)
      Runtime.sendMessage "agent" (buf:byte[]) role 
    printfn "I am sending and moving to next state: %i %s and %s " payload label role 
  member x.lookUpLabel role = 
    if x.isMock then "BYE"
    else 
      let result = Runtime.receiveMessage "agent" [] role []      
      let decode = new System.Text.UTF8Encoding() 
      let labelRead = decode.GetString(result.[0]) 
      labelRead

let startClient () = 
  let endpoint = Comms.makeTcpClientEndPoint "C" "127.0.0.1" 4001
  endpoint

let startServer () = 
   let ip = System.Net.IPAddress.Parse("127.0.0.1")
   let endpoint = Comms.makeTcpServerEndPoint "S" ip 4001
   endpoint

type TCPRuntime(isMock, endpoint:Comms.CommunicationChannel) =
  member x.isMock = isMock
  member x.endpoint  = endpoint
    
  
  member x.recv role label = 
    async {
      let! result = x.endpoint.RecvInt ()
      return result
    } 
     
  member x.send role label (payload:int) = 
    x.endpoint.SendInt payload 

  member x.lookUpLabel role = 
     "BYE"
