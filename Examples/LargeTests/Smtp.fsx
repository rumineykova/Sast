#r "../../src/Sast/bin/Debug/Sast.dll"
open ScribbleGenerativeTypeProvider

open ScribbleGenerativeTypeProvider.DomainModel
                        
[<Literal>]
let delims = """ [ {"label" : "220", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "221", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "250", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "2502", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "2503", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "2504", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "250d", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "2501", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "250d1", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "235", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "535", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "501", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "354", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "Ehlo", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "StartTls", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },  
                   {"label" : "Auth", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "Mail", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Rcpt", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Subject", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "DataLine", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Data", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "EndOfData", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "Quit", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] }
                   }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

let file = "SutherlandHodgeman.src"
let path = @"C:\Users\rn710\Repositories\scribble-java\scribble-assertions\src\test\scrib\assrt\icse18\" + file
let testPath = @"C:/Users/rn710/Repositories/TestGenerator/Scribble/test100.scr"
//let fn = @
let test = "C:/Users/rn710/Repositories/scribble-java/scribble-assertions/src/test/scrib/assrt/icse18/Smtp.scr"
// C:/cygwin64/home/rhu/code/vs/scribble/github.com/rumineykova/Sast/Examples/Fibonacci/
type Fib = 
    Provided.TypeProviderFile<"C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Examples/LargeTests/FSM/Smtp.txt" // Fully specified path to the scribble file
                               ,"Smtp" // name of the protocol
                               ,"C" // local role
                               ,"../../../Examples/Fibonacci/config.yaml" // config file containing IP and port for each role and the path to the scribble script
                               ,Delimiter=delims 
                               ,TypeAliasing=typeAliasing // give mapping from scribble base files to F# types
                               ,ScribbleSource = ScribbleSource.File, // choose one of the following options: (LocalExecutable | WebAPI | File)
                               ExplicitConnection=false>

let S = Fib.S.instance

let fib = new Fib()
let first = fib.Start()
let test = first.receive220(S).sendEhlo(S).branch()
match test with 
    | :? Fib.``535`` as c -> 
        let mc = c.receive(S).sendAuth(S).branch()
        match mc with 
        
//let test = f
//let first = fib.Start()
//let test = first

//let fib = new Fib()
//let first = fib.Start()

(*let fibo = new Fib()
let first = fibo.Start()
let test = first.sendsendingMessage(S, 2).sendsendingMessage(S, 2).sendsendingMessage(S, 2)*)

//first |> fibrec 1 1 numIter
