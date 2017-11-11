#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims = """ [ {"label" : "quote2", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "ok", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "title", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "quote", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "empty", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "empty1", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "empty2", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "empty3", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "quoteByTwo", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "quit", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

// 
// C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Examples/LargeTests/FSM/2BuyerFSMB.txt"

type Fib = 
    Provided.TypeProviderFile<"C:/Users/rn710/Repositories/scribble-java/scribble-demos/scrib/twobuyer/src/twobuyer/TwoBuyer.scr"
                                ,"TwoBuyer"
                               ,"B"
                               ,"../../../Examples/LargeTests/Config/configbuyer.yaml"
                               ,Delimiter=delims
                               ,TypeAliasing=typeAliasing
                               ,ScribbleSource = ScribbleSource.LocalExecutable
                               ,ExplicitConnection=true
                               ,AssertionsOn=false>



let session = new Fib()
session.Start()
let dummy = new DomainModel.Buf<int>()

let r = new DomainModel.Buf<int>()
let sessionCh = session.Start()
//let branch =  sessionCh.branch() 
fibServer(sessionCh)

(*  
let rec fibrec a b iter (c:Fib.State14) =
    let res = new DomainModel.Buf<int>()
    printfn"number of iter: %d" (numIter - iter)
    match iter with
        |0 -> c.sendBYE(S).receiveBYE(S).finish()
        |n -> let c1 = c.sendADD(S, a, b)
              let c2 = c1.receiveRES(S, res)
              printfn "Fibo : %d" (res.getValue())
              Async.RunSynchronously(Async.Sleep(1000))
              fibrec b (res.getValue()) (n-1) c2

let fibo = new Fib()
let first = fibo.Start()
first |> fibrec 1 1 numIter

*)