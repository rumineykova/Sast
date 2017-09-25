#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims = """ [ {"label" : "propose", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "accpt", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "confirm", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "reject", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "confirm", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """



type Fib = 
    Provided.TypeProviderFile<"C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Examples/LargeTests/FSM/NegoC.txt"
                               ,"Nego"
                               ,"C"
                               ,"../../../Examples/LargeTests/Config/configNego.yaml"
                               ,Delimiter=delims
                               ,TypeAliasing=typeAliasing
                               ,ScribbleSource = ScribbleSource.File, 
                               ExplicitConnection=true>



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