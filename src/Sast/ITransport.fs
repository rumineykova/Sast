module ScribbleGenerativeTypeProvider.ITransport

type LRuntime(isMock) = 
  member x.isMock = isMock 
  member x.recv role label = 
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
  