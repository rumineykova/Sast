#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims1 = """ [ {"label" : "ADD", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "RES", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BYE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""

[<Literal>]
let typeAliasing1 = """ [ {"alias" : "int", "type": "System.Int32"},
                          {"alias" : "string", "type": "System.String"}] """

type Fib = 
    Provided.TypeProviderFile<"../../../Examples/Fibonacci/FSM/FSMAsstS.txt"
                               ,"Adder"
                               ,"S"
                               ,"../../../Examples/Fibonacci/configServer.yaml"
                               ,Delimiter=delims1
                               ,TypeAliasing=typeAliasing1
                               ,ScribbleSource = ScribbleSource.File
                               ,ExplicitConnection=false 
                               ,AssertionsOn=true>

let C = Fib.C.instance

let rec fibServer (c0:Fib.State26) =
    let res1 = new DomainModel.Buf<int>()
    //let res2 = new DomainModel.Buf<int>()
    let res3 = new DomainModel.Buf<string>()
    //let res4 = new DomainModel.Buf<int>()
    //let res5 = new DomainModel.Buf<int>()
    //let c = c0.receiveHELLO(C, res1)
    //printfn"received Hello %i" (res1.getValue())
    let c = c0.receiveHELLO(C, res3)
    //printfn "After receive once"
    match c.branch() with 
        | :? Fib.BYE as bye-> 
            printfn"receive bye"
            bye.receive(C).sendBYE(C).finish()
        | :? Fib.ADD as add -> 
            printfn"receive add" 
            let c1 = add.receive(C, res1)
            let c2 = c1.sendRES(C, res1.getValue())
            fibServer c2

let session = new Fib()

let r = new DomainModel.Buf<int>()
let sessionCh = session.Start()//.accept(C)
let snd = sessionCh.sendHELLO(C, 3)
printfn "Just received"

let thr = snd.sendHELLO(C, 2)
printfn "Then send"
fibServer(thr)

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

(*
let e = if System.DateTime.Today.Day % 2 = 0 then 
            <@ + 1 @> 
        else 
            <@ - 1 @>
let resf = <@ fun x -> %e + x @>

open Microsoft.FSharp.Quotations
let inferred = dict["x", "5"; "y", "4"]
let elem = "x"
let myRes = Expr.Value(inferred.Item elem)
let x = 2
let s = <@@ x + 1 @@>
printf "Test" *)

//let myres = <@@ res @@>
