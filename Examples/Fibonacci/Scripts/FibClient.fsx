#r "../../../src/Sast/bin/Debug/Sast.dll"


open ScribbleGenerativeTypeProvider           
           
           
                        
[<Literal>]
let delims = """ [ {"label" : "ADD", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "RES", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BYE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing1 = """ [ {"alias" : "int", "type": "System.Int32"}, 
                          {"alias" : "string", "type": "System.String"}] """


type Fib = Provided.STP<"../../../Examples/Fibonacci/Protocols/Fib.scr"
                       , "Adder" 
                       , "C"
                       , "../../../Examples/Fibonacci/Config/config.yaml"
                       ,Delimiter=delims
                       ,TypeAliasing=typeAliasing1
                       ,ScribbleSource = ScribbleSource.LocalExecutable
                       ,ExplicitConnection=false 
                       ,AssertionsOn=true>

let C = Fib.C.instance
let S = Fib.S.instance

let s = new Fib()
let c = s.Init()
let p = new DomainModel.Buf<int>()
let p2 = new DomainModel.Buf<int>()
let finalS = c.sendHELLO<0>(S, 2).sendHELLO<1>(S, 1)

//let finalS = c.sendHELLO(S, 2).sendHELLO(S, 3)//receiveHELLO(S, p).sendHELLO(S, 2).receiveHELLO(S, p2).finish()
Async.RunSynchronously(Async.Sleep(2000))
finalS.finish()

printfn "Done too:%i!" (p.getValue())
(*let Fib = Provided.STP<"../../../Examples/Fibonacci/FSM/SimpleC.txt"
                               ,"Adder"
                               ,"C"
                               ,"../../../Examples/Fibonacci/Config/config.yaml"
                               ,Delimiter=delims
                               ,TypeAliasing=typeAliasing1
                               ,ScribbleSource = ScribbleSource.File
                               ,ExplicitConnection=false 
                               ,AssertionsOn=true>

module TimeMeasure =     
    open System.IO
    open System.Diagnostics

    let mutable stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let path = "C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Evaluation/"
    let file = "C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Evaluation/tempfib.txt"

    let start() = 
        stopWatch.Stop()
        stopWatch <- System.Diagnostics.Stopwatch.StartNew()

    let measureTime (step:string) = 
        stopWatch.Stop()
        let numSeconds = stopWatch.ElapsedTicks / Stopwatch.Frequency
        let curTime = sprintf "%s: %i \r\n" step stopWatch.ElapsedMilliseconds
        File.AppendAllText(file, curTime)
        stopWatch <- Stopwatch.StartNew()

let numIter = 1000
let S = Fib.S.instance

let rec fibrec a b iter (c0:Fib.State12) = 
    let res = new DomainModel.Buf<int>()
    //let res2 = new DomainModel.Buf<int>()
    //let res1 = new DomainModel.Buf<int>()
    //printfn "number of iter: %d" (numIter - iter)
    let c = c0.sendHELLO(S, "1")
    //printfn "Result received: %d" (res2.getValue())
    match iter with
        |0 -> 
            let c1 = c.sendBYE(S)
            let c2 = c1.receiveBYE(S)
            printfn "Fibo : %d" b
            TimeMeasure.measureTime  "done"
            let finalc = c2.finish()
            printfn "done fib"
            finalc
        |n -> 
            let c1 = c.sendADD(S, b)
            printfn "Send ADD"   
            (*let genRandomNumbers count =
                let rnd = System.Random()
                List.init count (fun _ -> rnd.Next ())

            let l = genRandomNumbers 10000 |> List.sort *)
            let c2 = c1.receiveRES(S, res)
            let foo s = if s > 0 then true else false 
            let result = foo (res.getValue())
            //printfn "GetValue: %d" (res.getValue())
            //if res.getValue() >  0 then 
            //    printfn "Fibo : %d" (res.getValue())
            //else 
            //    printfn "Fibo : %d" (res.getValue())
            //Async.RunSynchronously(Async.Sleep(1000))
            fibrec b (res.getValue()) (n-1) c2



let fibo = new Fib()
let r = new DomainModel.Buf<int>()
let newS = new DomainModel.Buf<int>()
let f = new DomainModel.Buf<int>()
let first = fibo.Start()//.request(S)
let snd = first.receiveHELLO(S, newS)
printfn "Just sent"

let thr = snd.sendHELLO(S, 2)

//printfn "The received values are %i and %i" (r.getValue()) (f.getValue())

//TimeMeasure.start()
//let s = sprintf "TP measure with assertions in the code for: %i" numIter
//TimeMeasure.measureTime s
thr |> fibrec 1 1 3

*)