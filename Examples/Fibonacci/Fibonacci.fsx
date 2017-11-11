#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "vertex", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "BothInOrOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Check", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "plane", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "NumPoits", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "BothIn", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "BothOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Itersection", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "OnePoit", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "TwoPoits", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },  
                   {"label" : "Close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

//let fn = @
// C:/cygwin64/home/rhu/code/vs/scribble/github.com/rumineykova/Sast/Examples/Fibonacci/
type Fib = 
    Provided.TypeProviderFile<"../../../Examples/SH/ShNewFSM_P.txt" // Fully specified path to the scribble file
                               ,"SH" // name of the protocol
                               ,"P" // local role
                               ,"../../../Examples/SH/configSP.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.File, // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ExplicitConnection=false>

let sh = new SH()
let S = SH.instance

let numIter = 3
let R = SH.R.instance
let C = SH.C.instance
let sh = new SH()
let rec calcClipPoints (vert: int list)  (c:SH.State27) =
    let res = new DomainModel.Buf<int>()    
    match vert with 
    | [hd] -> c.sendClose(R).sendClose(C).finish()
    | hd1::hd2::tail -> 
        let p = printf "Hello 1"
                6
        let c1 = c.sendcheck(R, hd1, hd2).receivePoitsToForward(R, res)
        match c1 with 
        | :? SH.noItersection as no -> no.receive(R).sendOnePoit1(P, h2)
        | :? SH.           

(*let rec fibrec a b iter (c0:Fib.State7) = 
    let res = new DomainModel.Buf<int>()
    printfn "number of iter: %d" (numIter - iter)
    let c = c0.sendHELLO(S, a)
    match iter with
        |0 -> 
            let c1 = c.sendBYE(S)
            let c2 = c1.receiveBYE(S)
            c2.finish()
        |n -> 
            let c1 = c.sendADD(S, a)
            let c2 = c1.receiveRES(S, res)

            printfn "Fibo : %d" (res.getValue())
            Async.RunSynchronously(Async.Sleep(1000))
            fibrec b (res.getValue()) (n-1) c2


let fibo = new Fib()
let first = fibo.Start()
let [<Literal>] myTwo = "fun (u:System.Int32) -> u < 3"
type DivisibleBy3 = Constraint.Numbers.ConstraintInt32<Rule = myTwo>

first |> fibrec 1 1 numIter
*)


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


    
