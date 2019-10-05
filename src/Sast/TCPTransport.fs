module ScribbleGenerativeTypeProvider.TCPTransport

module Payload =
    type Obj =
        | Str of string
        | Int of int
        | Unit of unit

    let serialise payloadObj =
        let str =
            match payloadObj with
            | Str s -> sprintf "1%s" s
            | Int i -> sprintf "2%d" i
            | Unit() -> "3"

        let bytes = System.Text.Encoding.ASCII.GetBytes str
        Array.append bytes [| 0uy |]

    let deserialise (str : string) =
        assert (String.length str > 0)
        match str.[0] with
        | '1' -> Str(str.[1..])
        | '2' -> Int(int str.[1..])
        | '3' -> Unit()
        | _ -> failwith "Invalid"

type Queue = System.Collections.Concurrent.ConcurrentQueue<Payload.Obj>

type CommunicationChannel(name, inQueue, outQueue, handleSend, handleRecv, disposer) =

    interface System.IDisposable with
        member this.Dispose() = disposer()

    member this.InQueue : Queue = inQueue
    member this.OutQueue : Queue = outQueue
    member this.Name = name

    member this.SendInt i =
        async {
            this.OutQueue.Enqueue(Payload.Obj.Int i)
            printfn "%s sends Integer %d" this.Name i
            do! handleSend()
        }

    member this.SendString s =
        async {
            this.OutQueue.Enqueue(Payload.Obj.Str s)
            printfn "%s sends String %s" this.Name s
            do! handleSend()
        }

    member this.SendUnit u =
        async {
            this.OutQueue.Enqueue(Payload.Obj.Unit u)
            printfn "%s sends Unit" this.Name
            do! handleSend()
        }

    member this.RecvItem() =
        async {
            let success, value = this.InQueue.TryDequeue()
            if success then return value
            else
                do! handleRecv()
                return! this.RecvItem()
        }

    member this.RecvInt() =
        async {
            let! (Payload.Obj.Int i) = this.RecvItem()
            printfn "%s receives Integer %d" this.Name i
            return i
        }

    member this.RecvString() =
        async {
            let! (Payload.Obj.Str s) = this.RecvItem()
            printfn "%s receives String %s" this.Name s
            return s
        }

    member this.RecvUnit() =
        async {
            let! (Payload.Obj.Unit u) = this.RecvItem()
            printfn "%s receives Unit" this.Name
            return u
        }

let makeLocalEndPointPair (name1, name2) =
    let queue1 = Queue()
    let queue2 = Queue()
    let doNothing _ = async { () }
    let endpoint1 =
        new CommunicationChannel(name1, queue1, queue2, doNothing, doNothing,
                                 fun () -> ())
    let endpoint2 =
        new CommunicationChannel(name2, queue2, queue1, doNothing, doNothing,
                                 fun () -> ())
    endpoint1, endpoint2

let handleSend (outQueue : Queue) (stream : System.Net.Sockets.NetworkStream) () =
    async {
        let success, value = outQueue.TryDequeue()
        if success then
            let bytes = Payload.serialise value
            do! stream.AsyncWrite(bytes, 0, Array.length bytes)
        else ()
    }

let handleReceive (inQueue : Queue) (stream : System.Net.Sockets.NetworkStream)
    (buffer : System.Text.StringBuilder) () =
    async {
        try
            let buf = Array.zeroCreate 1
            let! bytesRead = stream.AsyncRead(buf, 0, 1)
            if bytesRead = 0 then ()
            else
                let byteFromStream = buf.[0] |> int
                if byteFromStream = -1 then ()
                else if byteFromStream = 0 then
                    let payloadObj = Payload.deserialise (buffer.ToString())
                    inQueue.Enqueue(payloadObj)
                    buffer.Clear() |> ignore
                else
                    let character = System.Char.ConvertFromUtf32 byteFromStream
                    buffer.Append(character) |> ignore
        with :? System.Net.Sockets.SocketException -> ()
    }

let makeNetworkEndPoint name (stream : System.Net.Sockets.NetworkStream)
    disposer =
    let buffer = System.Text.StringBuilder()
    let inQueue = Queue()
    let outQueue = Queue()
    let endpoint =
        new CommunicationChannel(name, inQueue, outQueue,
                                 handleSend outQueue stream,
                                 handleReceive inQueue stream buffer, disposer)
    endpoint

let makeTcpClientEndPoint name ip port =
    let tcpClient = new System.Net.Sockets.TcpClient(ip, port)
    let stream = tcpClient.GetStream()

    let disposer() =
        stream.Close()
        tcpClient.Dispose()
    makeNetworkEndPoint name stream disposer

let makeTcpServerEndPoint name ip port =
    let tcpListener = System.Net.Sockets.TcpListener(ip, port)
    tcpListener.Start()
    let tcpClient = tcpListener.AcceptTcpClient()
    let stream = tcpClient.GetStream()

    let disposer() =
        stream.Close()
        tcpListener.Stop()
        tcpClient.Close()
    makeNetworkEndPoint name stream disposer
