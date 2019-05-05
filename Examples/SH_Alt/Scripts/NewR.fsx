#r "../../src/Sast/bin/Debug/net452/Sast.dll"

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

type SH = 
    Provided.TypeProviderFile<"../../../Examples/SH2/SHNew.scr" // Fully specified path to the scribble file
                               ,"SH" // name of the protocol
                               ,"R" // local role
                               ,"../../../Examples/SH2/configCR.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.LocalExecutable // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ,ExplicitConnection = false
                               ,AssertionsOn=true>

let R = SH.R.instance
let P = SH.P.instance

let rec utilF () =
    let genRandomNumbers count =
        let rnd = System.Random()
        List.init count (fun _ -> rnd.Next ())

    let l = genRandomNumbers 1000 |> List.sort
    l

let intersection x = x

let rec calculate (c:SH.State16) =
    match c.branch() with 
    | :? SH.Close as c1 -> c1.receive(P).finish()
    | :? SH.Above as c2 -> 
        let p1 = new DomainModel.Buf<int>()    
        let p2 = new DomainModel.Buf<int>()
        let c3 = c2.receive(P, p1)
        let r1 = utilF ()
        let c4 = c3.sendRes(P, 0).receiveAbove(P, p2)
        let r2 = utilF ()
        let c5 = c4.sendRes(P, 1).branch()
        let next =  match c5 with 
                    | :? SH.BothIn as bin   -> bin.receive(P)
                    | :? SH.BothOut as bout -> bout.receive(P)
                    | :? SH.Inersect as it ->     
                        let c6 = it.receive(P)
                        let r3 = utilF () 
                        c6.sendRes(P, 2)
        calculate next



let res1 = new DomainModel.Buf<int>()    
let res2 = new DomainModel.Buf<int>()    
let sh = new SH()
let c = sh.Start().receivehello(P, res1).sendhello(P, 1)
let c1 = c.receiveplane(P, res2)     
calculate c1

//let inline xor a b = (a || b) && not (a && b)

(*let rec calcPoint (vert: int list) (c: SH.State36) = 
    match vert with 
    | [hd] -> c.sendClose(R).sendClose(C)
    | hd1::hd2::tail ->
        let b1 = new DomainModel.Buf<int>()
        let b2 = new DomainModel.Buf<int>()
        let c1 = c.sendAbove(R, hd1).receiveRes(R, b1).sendAbove(R, hd2).receiveRes(R, b2)
        let cont = 
            if (b1.getValue()=1 && b2.getValue()=1) then 
                c1.sendBothIn(R).sendBothIn(C, hd2)
            else if (b1.getValue() <> b1.getValue()) then 
                c1.sendBothOut(R).sendBothOut(C)
                else 
                    let intr = new DomainModel.Buf<int>()
                    let c2 = c1.sendItersect(R).receiveRes(R, intr)
                    if (b2.getValue()=1) then 
                        c2.sendOne(C, intr.getValue())
                    else 
                        c2.sendTwo(C, intr.getValue(), hd2)
        calcPoint (hd2::tail) cont*)




        //.sendCheck(R, hd1, hd2).receiveNumPoits(R, res).branch()
        //let cont = match res with 
        //    | :? SH.BothIn as c1 -> c1.receive(R).sendBothIn(C)
        //    | :? SH.BothOut as c2 -> c2.receive(R).sendBothOut(C)  
        //    | :? SH.Itersection as c3 -> c3.receive(R, point)

