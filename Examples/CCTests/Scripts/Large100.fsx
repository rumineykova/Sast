#r "../../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims = """ [ {"label" : "sendingMessage", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "RES", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BYE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "hello", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

type Seq = 
    Provided.TypeProviderFile<"../../../Examples/CCTest/Protocols/test100.scr","Test100","C"
        ,"../../../Examples/CCTest/Config/config.yaml",Delimiter=delims,TypeAliasing=typeAliasing
        ,ScribbleSource = ScribbleSource.LocalExecutable,AssertionsOn=true>

let session = new Seq()
let dummy = new DomainModel.Buf<int>()
let S = Seq.S.instance
let r = new DomainModel.Buf<int>()
let sessionCh = session.Start()
//let branch =  sessionCh.branch() 

let v1 = new DomainModel.Buf<int>()
let v2 = new DomainModel.Buf<int>()
let v3 = new DomainModel.Buf<int>()
let v4 = new DomainModel.Buf<int>()
let v5 = new DomainModel.Buf<int>()
let v6 = new DomainModel.Buf<int>()
let v7 = new DomainModel.Buf<int>()
let v8 = new DomainModel.Buf<int>()
let v9 = new DomainModel.Buf<int>()
let v10 = new DomainModel.Buf<int>()
let v11 = new DomainModel.Buf<int>()
let v12 = new DomainModel.Buf<int>()
let v13 = new DomainModel.Buf<int>()
let v14 = new DomainModel.Buf<int>()
let v15 = new DomainModel.Buf<int>()
let v16 = new DomainModel.Buf<int>()
let v17 = new DomainModel.Buf<int>()
let v18 = new DomainModel.Buf<int>()
let v19 = new DomainModel.Buf<int>()
let v20 = new DomainModel.Buf<int>()
let v21 = new DomainModel.Buf<int>()
let v22 = new DomainModel.Buf<int>()
let v23 = new DomainModel.Buf<int>()
let v24 = new DomainModel.Buf<int>()
let v25 = new DomainModel.Buf<int>()
let v26 = new DomainModel.Buf<int>()
let v27 = new DomainModel.Buf<int>()
let v28 = new DomainModel.Buf<int>()
let v29 = new DomainModel.Buf<int>()
let v30 = new DomainModel.Buf<int>()
let v31 = new DomainModel.Buf<int>()
let v32 = new DomainModel.Buf<int>()
let v33 = new DomainModel.Buf<int>()
let v34 = new DomainModel.Buf<int>()
let v35 = new DomainModel.Buf<int>()
let v36 = new DomainModel.Buf<int>()
let v37 = new DomainModel.Buf<int>()
let v38 = new DomainModel.Buf<int>()
let v39 = new DomainModel.Buf<int>()
let v40 = new DomainModel.Buf<int>()
let v41 = new DomainModel.Buf<int>()
let v42 = new DomainModel.Buf<int>()
let v43 = new DomainModel.Buf<int>()
let v44 = new DomainModel.Buf<int>()
let v45 = new DomainModel.Buf<int>()
let v46 = new DomainModel.Buf<int>()
let v47 = new DomainModel.Buf<int>()
let v48 = new DomainModel.Buf<int>()
let v49 = new DomainModel.Buf<int>()
//TimeMeasure.start()

let c1 = sessionCh.sendhello(S, 1).receivehello(S, v1)
//TimeMeasure.measureTime "Start 100"
printfn "start"
let rec loop count (c:Seq.State278) = 
    if count > 0 then 
        let c2 = c.sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
                        .sendhello(S, 1).receivehello(S, new DomainModel.Buf<int>())
        loop (count - 1) c2
    else printf "Done" 
let iter = 10
loop iter  c1        
let s = sprintf "Measure for seq with assertions %i" iter
//TimeMeasure.measureTime s