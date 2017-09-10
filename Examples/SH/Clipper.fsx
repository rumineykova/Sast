#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "vertex", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "BothInOrOut", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "addP", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "none", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "plane", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Itersection", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

// C:/cygwin64/home/rhu/code/vs/scribble/github.com/rumineykova/Sast/Examples/Fibonacci/
type SH = 
    Provided.TypeProviderFile<"../../../Examples/SH/ShFSM_C.txt" // Fully specified path to the scribble file
                               ,"SH" // name of the protocol
                               ,"C" // local role
                               ,"../../../Examples/SH/configC.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.File // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ,ExplicitConnection = true>
let numIter = 3
let S = SH.S.instance

let ifInteresect (x:int) (y:int) = x + y
let getInteresectionP x y = (x - y)


let rec calculate (c:SH.State22) =
    let resx = new DomainModel.Buf<int>()    
    let resy = new DomainModel.Buf<int>()
    match c.branch() with 
    | :? SH.close as close -> close.receive(S).finish()     
    | :? SH.vertex as vertex -> 
        let c1 = vertex.receive(S, resx, resy)
        let v = ifInteresect (resx.getValue()) (resy.getValue())
        c1.sendBothInOrOut(S, v)
        let c2 = match v with 
                | 1 | 0  -> 
                    c1.sendBothInOrOut(S, f)

                | _ -> let p = getInteresectionP (resx.getValue()) (resy.getValue())
                       c1.sendItersection(S, p)
        calculate c2
               

let polygon = []
let res1 = new DomainModel.Buf<int>()    
let res2 = new DomainModel.Buf<int>()  
let res3 = new DomainModel.Buf<int>()  
let res4 = new DomainModel.Buf<int>()  
let sh = new SH()
let c = sh.Start().receiveplane(S, res1, res2, res3, res4) |> calculate 

let (x:int) = 1
let mutable dict = Map.empty<string, int>
let name = "x"
dict<-dict.Add(name, x)

//x <-2
printf "Value is: %i" (dict.Item(name))
//printf "Value is: %i" (dict.Count)

        


