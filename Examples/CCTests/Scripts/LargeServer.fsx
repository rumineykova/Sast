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

// C:/Users/rn710/Repositories/TestGenerator/Scribble/test100.scr

type SeqS = 
    Provided.TypeProviderFile<"../../../Examples/CCTests/Protocols/test100.scr"
                               ,"Test100"
                               ,"S"
                               ,"../../../Examples/CCTests/Config/configServer.yaml"
                               ,Delimiter=delims
                               ,TypeAliasing=typeAliasing
                               ,ScribbleSource = ScribbleSource.LocalExecutable
                               ,ExplicitConnection=false
                               ,AssertionsOn=true>

let session = new SeqS()
let dummy = new DomainModel.Buf<int>()
let C = SeqS.C.instance
let r = new DomainModel.Buf<int>()

let sessionCh = session.Start().receivehello(C, r).sendhello(C, 1)

(*let v1 = new DomainModel.Buf<int>()
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
let v50 = new DomainModel.Buf<int>()
let v51 = new DomainModel.Buf<int>()
let v52 = new DomainModel.Buf<int>()
let v53 = new DomainModel.Buf<int>()
let v54 = new DomainModel.Buf<int>()
let v55 = new DomainModel.Buf<int>()
let v56 = new DomainModel.Buf<int>()
let v57 = new DomainModel.Buf<int>()
let v58 = new DomainModel.Buf<int>()
let v59 = new DomainModel.Buf<int>()
let v60 = new DomainModel.Buf<int>()
let v61 = new DomainModel.Buf<int>()
let v62 = new DomainModel.Buf<int>()
let v63 = new DomainModel.Buf<int>()
let v64 = new DomainModel.Buf<int>()
let v65 = new DomainModel.Buf<int>()
let v66 = new DomainModel.Buf<int>()
let v67 = new DomainModel.Buf<int>()
let v68 = new DomainModel.Buf<int>()
let v69 = new DomainModel.Buf<int>()
let v70 = new DomainModel.Buf<int>()
let v71 = new DomainModel.Buf<int>()
let v72 = new DomainModel.Buf<int>()
let v73 = new DomainModel.Buf<int>()
let v74 = new DomainModel.Buf<int>()
let v75 = new DomainModel.Buf<int>()
let v76 = new DomainModel.Buf<int>()
let v77 = new DomainModel.Buf<int>()
let v78 = new DomainModel.Buf<int>()
let v79 = new DomainModel.Buf<int>()
let v80 = new DomainModel.Buf<int>()
let v90 = new DomainModel.Buf<int>()
let v91 = new DomainModel.Buf<int>()
let v92 = new DomainModel.Buf<int>()
let v93 = new DomainModel.Buf<int>()
let v94 = new DomainModel.Buf<int>()
let v95 = new DomainModel.Buf<int>()
let v96 = new DomainModel.Buf<int>()
let v97 = new DomainModel.Buf<int>()
let v98 = new DomainModel.Buf<int>()
let v99 = new DomainModel.Buf<int>()
let v100 = new DomainModel.Buf<int>()
let v81 = new DomainModel.Buf<int>()
let v82 = new DomainModel.Buf<int>()
let v83 = new DomainModel.Buf<int>()
let v84 = new DomainModel.Buf<int>()
let v85 = new DomainModel.Buf<int>()
let v86 = new DomainModel.Buf<int>()
let v87 = new DomainModel.Buf<int>()
let v88 = new DomainModel.Buf<int>()
let v89 = new DomainModel.Buf<int>()

let v101 = new DomainModel.Buf<int>()
let v102 = new DomainModel.Buf<int>()*)
let mutable s = 1
//let mutable f = 1
printfn "Starting"
let rec loop i (c1:SeqS.State96) = 
    if (s>0) then 
        (*let rec innerloop (n:int) (s:int) =
            if n > 0 then s
            else  innerloop (n - 1) s
        let f =  innerloop i 1*)
        let c2 = c1.receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1).receivehello(C, new DomainModel.Buf<int>())
                    .sendhello(C, 1)
        loop i c2
    else true

loop 10 sessionCh  