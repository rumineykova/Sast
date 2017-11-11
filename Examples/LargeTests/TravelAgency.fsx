#r "../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims = """ [ {"label" : "Query", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                    {"label" : "Quote", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "Dummy", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Bye", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "No", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Yes", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

//C:\Users\rn710\Repositories\scribble-java\scribble-demos\scrib\travel\src\travel\Travel.scr
// fsm is inL C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Examples/LargeTests/FSM/TravelAgencyA.txt
type Fib = 
    Provided.TypeProviderFile<"C:/Users/rn710/Repositories/scribble-java/scribble-demos/scrib/travel/src/travel/Travel.scr"
                               ,"Booking"
                               ,"A"
                               ,"../../../Examples/LargeTests/Config/configTA.yaml"
                               ,Delimiter=delims
                               ,TypeAliasing=typeAliasing
                               ,ScribbleSource = ScribbleSource.LocalExecutable
                               ,ExplicitConnection=true
                               ,AssertionsOn=false>


Fib.
let session = new Fib()
session.Start()
let dummy = new DomainModel.Buf<int>()

let r = new DomainModel.Buf<int>()
let sessionCh = session.Start()
//let branch =  sessionCh.branch() 
fibServer(sessionCh)

(*  
let rec fibrec a b iter (c:Fib.State14) =
    let res = new DomainModel.Buf<int>()
    printfn"number of iter: %d" (numIter - iter)
    match iter with
        |0 -> c.sendBYE(S).receiveBYE(S).finish()
        |n -> let c1 = c.sendADD(S, a, b)
              let c2 = c1.receiveRES(S, res)
              printfn "Fibo : %d" (res.getValue())
              Async.RunSynchronously(Async.Sleep(1000))
              fibrec b (res.getValue()) (n-1) c2

let fibo = new Fib()
let first = fibo.Start()
first |> fibrec 1 1 numIter

*)