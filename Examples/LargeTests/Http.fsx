#r "../../src/Sast/bin/Debug/Sast.dll"
open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "REQUESTL", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "HOST", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BODY", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "USERA", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "ACCEPT", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "ACCEPTL", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "ACCEPTE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "DNT", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "CONNECTION", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "UPGRADEIR", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "HTTPV", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "200", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "404", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "DATE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "SERVER", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },  
                   {"label" : "STRICTTS", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "ETAG", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "ACCEPTR", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "CONTENTL", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "VARY", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "CONTENTT", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "VIA", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "LASTM", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }
                   ]"""

[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

let file = "SutherlandHodgeman.src"
let path = @"C:\Users\rn710\Repositories\scribble-java\scribble-assertions\src\test\scrib\assrt\icse18\" + file
let testPath = @"C:/Users/rn710/Repositories/TestGenerator/Scribble/test100.scr"
//let f n = @
// C:/cygwin64/home/rhu/code/vs/scribble/github.com/rumineykova/Sast/Examples/Fibonacci/
type Fib = 
    Provided.TypeProviderFile<"C:/Users/rn710/Repositories/scribble-java/scribble-assertions/src/test/scrib/assrt/icse18/Http.scr" // Fully specified path to the scribble file
                               ,"Http" // name of the protocol
                               ,"C" // local role
                               ,"../../../Examples/Fibonacci/config.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.LocalExecutable // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ,ExplicitConnection=false
                               ,AssertionsOn=false>

let S = Fib.S.instance 
Fib.
 

let fib = new Fib()
let first = fib.Start()
let test = first.sendREQUESTL(S).sendACCEPT(S); 
        
//let test = f
//let first = fib.Start()
//let test = first

//let fib = new Fib()
//let first = fib.Start()

(*let fibo = new Fib()
let first = fibo.Start()
let test = first.sendsendingMessage(S, 2).sendsendingMessage(S, 2).sendsendingMessage(S, 2)*)

//first |> fibrec 1 1 numIter
