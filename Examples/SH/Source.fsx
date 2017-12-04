#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "Above", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "plane", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "hello", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "Res", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BothIn", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BothOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Inersect", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Res1", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "One", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Two", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

// C:\Users\rn710\Repositories\scribble-java\scribble-assertions\src\test\scrib\assrt\sh\SH.scr

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

// C:/cygwin64/home/rhu/code/vs/scribble/github.com/rumineykova/Sast/Examples/Fibonacci/
type SH = 
    Provided.TypeProviderFile<"../../../Examples/SH/SHNew.scr" // Fully specified path to the scribble file
                               ,"SH" // name of the protocol
                               ,"P" // local role
                               ,"../../../Examples/SH/configSP.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.LocalExecutable // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ,ExplicitConnection = false
                               ,AssertionsOn = true>
let numIter = 3
let R = SH.R.instance
let C = SH.C.instance
let sh = new SH()
let rec calcClipPoints (vert: int list)  (c:SH.State48) =
    let res1 = new DomainModel.Buf<int>()    
    let res2 = new DomainModel.Buf<int>()    
    let res3 = new DomainModel.Buf<int>()    
    match vert with 
    | [hd] -> let c1 = c.sendClose(R).sendClose(C)
              printf "All points received"
              TimeMeasure.measureTime "SH done"
              c1.finish()
    | hd1::hd2::tail -> 
        let c1 = c.sendAbove(R, 1).receiveRes(R, res1).sendAbove(R, 2)
                  .receiveRes(R, res2).sendInersect(R).receiveRes(R, res3).sendTwo(C)
        calcClipPoints (hd2::tail) c1
        
let polygon = {1..1000} |> Seq.toList

let dum = new DomainModel.Buf<int>()    
let startC = sh.Start().sendhello(R, 1).receivehello(R, dum).sendhello(C, 1).sendplane(R, 1)
TimeMeasure.start()
let s = sprintf "SH measure with no assertions"
TimeMeasure.measureTime s
polygon |> calcClipPoints <| startC


//first |> fibrec 1 1 numIter

(*let rec fibrec a b iter (c0:Fib.State7) =
    let res = new DomainModel.Buf<int>()
    printfn"number of iter: %d" (numIter - iter)
    let c = c0.sendHELLO(S, a)


    
    match iter with
        |0 -> c.sendBYE(S).receiveBYE(S).finish()
        |n -> let c1 = c.sendADD(S, a)
              let c2 = c1.receiveRES(S, res)
              printfn "Fibo : %d" (res.getValue())
              Async.RunSynchronously(Async.Sleep(1000))
              fibrec b (res.getValue()) (n-1) c2
*)


//let check func dict = 


    
