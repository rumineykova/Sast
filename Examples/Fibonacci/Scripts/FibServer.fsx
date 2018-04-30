#r "../../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims1 = """ [ {"label" : "ADD", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "RES", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BYE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""

[<Literal>]
let typeAliasing1 = """ [ {"alias" : "int", "type": "System.Int32"},
                          {"alias" : "string", "type": "System.String"}] """

type Fib = 
    Provided.STP<"../../../Examples/Fibonacci/Protocols/Fib.scr"
                               ,"Adder"
                               ,"S"
                               ,"../../../Examples/Fibonacci/Config/configServer.yaml"
                               ,Delimiter=delims1
                               ,TypeAliasing=typeAliasing1
                               ,ScribbleSource = ScribbleSource.LocalExecutable
                               ,ExplicitConnection=false 
                               ,AssertionsOn=true>

let C = Fib.C.instance
let S = Fib.S.instance

let s = Fib().Init()
let p = new DomainModel.Buf<int>()
let p2 = new DomainModel.Buf<int>()
[<Literal>]
let f = 1

let test1 (x:int) (y:int) = 
    printf "First handler %i and %i" x y
    ()

let test2 (x:int) (y:int) = 
    printf "Second handler %i and %i" x y
    ()
(*
let byeCallback (x:Fib.BYE) =  
    printf "Bye handler" 
    x.receive(C).sendBYE(C).finish()

let helloCallback (x:Fib.HELLO) =   
    printf "Bye executed" 
    let buf = new DomainModel.Buf<int>()
    x.receive(C, buf).sendHELLO(C, 2).finish()
*)
// trysmth(f, fun y -> y + f)

//let newS = s.receiveHELLO(C, p).sendHELLO(C, 4).branch(test1, test2)

printfn "After Init: %i!!!" 1
let newS = s.receiveHELLO(C)
printfn "Done: %i!!!" 1 //(p.getValue())
let res = newS.branch(test1, test2)
//.branch((), p)

let receiveHello x y = x + y 
let receiveBye x y z = x + y + z


 
//sendHELLO<2>(C, 2).receiveHELLO(C, p).sendHELLO<f>(C, 3)

printfn "Done: %i!!!" 2 //(p.getValue())



(*
let rec fibServer (c0:Fib.State26) =
    let res1 = new DomainModel.Buf<int>()
    //let res2 = new DomainModel.Buf<int>()
    let res3 = new DomainModel.Buf<string>()
    //let res4 = new DomainModel.Buf<int>()
    //let res5 = new DomainModel.Buf<int>()
    //let c = c0.receiveHELLO(C, res1)
    //printfn"received Hello %i" (res1.getValue())
    let c = c0.receiveHELLO(C, res3)
    //printfn "After receive once"
    match c.branch() with 
        | :? Fib.BYE as bye-> 
            printfn"receive bye"
            bye.receive(C).sendBYE(C).finish()
        | :? Fib.ADD as add -> 
            printfn"receive add" 
            let c1 = add.receive(C, res1)
            let c2 = c1.sendRES(C, res1.getValue())
            fibServer c2

let session = new Fib()

let r = new DomainModel.Buf<int>()
let f = new DomainModel.Buf<int>()
let sessionCh = session.Start()//.accept(C)
let snd = sessionCh.sendHELLO(C, 3)
printfn "Just received"

let thr = snd.receiveHELLO(C, r, f)
printfn "The received values are %i and %i" (r.getValue()) (f.getValue())
printfn "Then send"
fibServer(thr)
*)