#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "vertex", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "BothInOrOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "check", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "plane", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "NumPoins", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "BothIn", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "BothOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Itersection", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "OnePoit", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "TwoPoist", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },  
                   {"label" : "Close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

// C:/cygwin64/home/rhu/code/vs/scribble/github.com/rumineykova/Sast/Examples/Fibonacci/
type SH = 
    Provided.TypeProviderFile<"../../../Examples/SH/ShFSM_S.txt" // Fully specified path to the scribble file
                               ,"SH" // name of the protocol
                               ,"P" // local role
                               ,"../../../Examples/SH/configSP.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.File // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ,ExplicitConnection = true>
let numIter = 3
let R = SH.R.instance
let C = SH.C.instance
let sh = new SH()
let rec calcClipPoints (vert: int list)  (c:SH.State27) =
    let res = new DomainModel.Buf<int>()    
    match vert with 
    | [hd] -> c.sendClose(R).sendClose(C).finish()
    | hd1::hd2::tail -> 
        let c1 = c.sendcheck(R, hd1, hd2).receivePoitsToForward(R, res)
        match c1 with 
        | :? SH.noItersection as no -> no.receive(R).sendOnePoit1(P, h2)
        | :? SH.                             
(*        calcClipPoints (hd2::tail) cotn
        let cont = match askC with 
                    | :? SH.BothInOrOut as inout -> 
                        let c1 = inout.receive(C, res)
                        if res.getValue()=1 then c1.sendaddP(P, hd2)  
                        else c1.sendnone(P)

                    | :? SH.Itersection as intr -> 
                        intr.receive(C, res).sendforwardP(P, res.getValue())
        calcClipPoints (hd2::tail) cotn
        *)
//let polygon = []


//polygon |> calcClipPoints <| sh.Start().sendplane(C, 1, 2, 3, 4)




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


    
