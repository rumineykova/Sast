#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "Above", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "Res", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Itersect", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Two", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "One", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "BothIn", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "BothOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "plane", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"]} }, 
                   {"label" : "Close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

type SH = 
    Provided.TypeProviderFile<"../../../Examples/SH/ShNewFSM_P.txt" // Fully specified path to the scribble file
                               ,"SH" // name of the protocol
                               ,"P" // local role
                               ,"../../../Examples/SH/configSP.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.File // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ,ExplicitConnection = true>

let sh = new SH()
let R = SH.R.instance
let C = SH.C.instance
sh.Start().sendplane(R, 1, 2, 3, 4)

//let inline xor a b = (a || b) && not (a && b)

let rec calcPoint (vert: int list) (c: SH.State36) = 
    match vert with 
    | [hd] -> c.sendClose(R).sendClose(C).finish()
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
        calcPoint (hd2::tail) cont
    


        //.sendCheck(R, hd1, hd2).receiveNumPoits(R, res).branch()
        //let cont = match res with 
        //    | :? SH.BothIn as c1 -> c1.receive(R).sendBothIn(C)
        //    | :? SH.BothOut as c2 -> c2.receive(R).sendBothOut(C)  
        //    | :? SH.Itersection as c3 -> c3.receive(R, point)
        