#r "../../../src/Sast/bin/Debug/Sast.dll"


open ScribbleGenerativeTypeProvider           
           
           
                        
[<Literal>]
let delims = """ [ {"label" : "SUM", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "ADD", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "RES", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BYE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing1 = """ [ {"alias" : "int", "type": "System.Int32"}, 
                          {"alias" : "string", "type": "System.String"}] """

(*
type Fib = Provided.STP<"../../../Examples/Fibonacci/Protocols/Fib.scr"
                       , "Adder" 
                       , "C"
                       , "../../../Examples/Fibonacci/Config/config.yaml"
                       ,Delimiter=delims
                       ,TypeAliasing=typeAliasing1
                       ,ScribbleSource = ScribbleSource.LocalExecutable
                       ,ExplicitConnection=false 
                       ,AssertionsOn=true>
*)

type Fib = 
   Provided.STP<"..\FSM\FSMAsstC_new.txt", "Adder", "C"
       ,"../Config/configC.yaml", Delimiter=delims
       ,TypeAliasing=typeAliasing1, AssertionsOn=true, ScribbleSource= ScribbleSource.File>

let S = Fib.S.instance
//et client = new AdderC()
(*let c = client.Init().receiveHELLO(S).finish()
let C = Fib.C.instance
let S = Fib.S.instance*)

let s = new Fib()
let c = s.Init()

type Runtime.IContext with
    member x.SetX(y) = printfn "Setting x to: %i" y
    member x.GetX() = printfn "Getting x"


let helloCallback1 ctx  =   
    printfn "hello callback1" 
    Async.RunSynchronously(Async.Sleep(5000))
    printfn "hello callback1" 
    let buf = 4
    buf

let helloCallback11 ()  =   
    printfn "hello callback1" 
    Async.RunSynchronously(Async.Sleep(5000))
    printfn "hello callback1" 
    let buf = 4
    buf

let helloCallback3  = 
    printfn "hello callback3"
    printfn "Done"
    let s =5
    s

let helloCallback2  x =   
    printfn "hello callback2"
    printfn "%A" x
    ()

let x = ()
let helloCallback4 (c: Fib.ADD)  =   
    printfn "hello callback2"
    c.send(S, helloCallback1).receiveBYE(S, helloCallback2).finish()

let helloCallback5 (c: Fib.BYE)  =   
    printfn "hello callback2"
    c.send(S, helloCallback1).receiveBYE(S, helloCallback2).finish()

let helloCallback6 (c: Fib.BYEADD)  =      
    printfn "hello callback2"
    let s = System.Console.ReadLine()
    if (s = "Hello") then c.selector<"BYE">()
    else c.selector<"ADD">()

(*
let (|BYE|RES|) (n, c:Fib.State9) =
    if n % 2 = 0 then BYE c else RES c
*)

let x = 1
let s1 = c.receiveHELLO(S, helloCallback2)
          .sendHELLO(S, helloCallback1)
          .sendHELLO(S, helloCallback1)
          .register_selector(helloCallback6)
          .select_handlers(helloCallback5,helloCallback4)

//let (x, s3) = s2.select(fun x -> if n % 2 = 0 then Fib.BYE else Fib.RES, args)

(*
match (x, s2) with 
    | BYE s3 -> s3.sendBYE(S, helloCallback1)
    | RES s3 -> s3.sendRES(S, helloCallback1) 
*)

//sendRES(S, helloCallback3).finish()

// define the active pattern

(* DESIRED API
let rec fib(c:Fib.State6) = 
    let s1 = c.sendHELLO(S, fun data -> data.set_u<5>)
    let s2 = s1.sendHELLO(S, fun data -> data.set_f<1>)
    let s3 = s2.branch() //receiveSUM(S, fun (x:LabelCase) y -> ())
    match s3 with 
    | Fib.Sum -> let s4 = s3.receiveSum(S, fun data x -> data.x=x; x).sendRes(S, fun data -> data.x)
                 let s4
    | Fib.Mul ->  s3.receiveMul(S, fun data x -> ()).finish()

let sumMul data = 
    if data.f>1 then "Sum"
    else "Mul"

*)

(*let p = new DomainModel.Buf<int>()
let p2 = new DomainModel.Buf<int>()
// INT -> ROLE -> INT 
let finalS = c.sendHELLO<0>(S, 5).sendHELLO<1>(S)

//let finalS = c.sendHELLO(S, 2).sendHELLO(S, 3)//receiveHELLO(S, p).sendHELLO(S, 2).receiveHELLO(S, p2).finish()
Async.RunSynchronously(Async.Sleep(2000))
finalS.finish()
*)
//printfn "Done too:%i!" (p.getValue())
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