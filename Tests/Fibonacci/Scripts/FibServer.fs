﻿module Tests.FibServer

open ScribbleGenerativeTypeProvider

[<Literal>]
let delims1 = """ [ {"label" : "SUM", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "RES", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "BYE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "ADD", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""

[<Literal>]
let typeAliasing1 = """ [ {"alias" : "int", "type": "System.Int32"},
                          {"alias" : "string", "type": "System.String"}] """

type Fib = Provided.STP<"Fibonacci/FSM/Adder_S.txt", "Adder", "S"
    ,"Fibonacci/Config/configS.yaml", Delimiter=delims1
    ,TypeAliasing=typeAliasing1, AssertionsOn=true, ScribbleSource= ScribbleSource.File>

let C = Fib.C.instance
let S = Fib.S.instance

let l = Fib()
let s = l.Init()

let p = new DomainModel.Buf<int>()
let p2 = new DomainModel.Buf<int>()
[<Literal>]
let f = 1

let helloCallback1 x =
    let y = x + 1
    Async.RunSynchronously(Async.Sleep(5000))
    printfn "hello callBack1"
    printfn "%i" y
    ()

let helloCallback21a (s:Fib.InContext23) =
    printfn "hello callback21a"
    Async.RunSynchronously(Async.Sleep(5000))
    //let s =5
    s.setu<5>()

let helloCallback2a (s:Fib.InContext25) =
    printfn "hello callback2a"
    //let s =5
    s.setc<8>()

let helloCallback2b (s:Fib.InContext26) =
    printfn "hello callback2"
    Async.RunSynchronously(Async.Sleep(5000))
    //let s =5
    s.setd<6>()

let helloCallback3 x =
    printfn "hello callback3"
    Async.RunSynchronously(Async.Sleep(5000))
    printfn "%A" x
    let buf = new DomainModel.Buf<int>()
    printfn "Done"
    ()

let branchBYE (c: Fib.BYE) =
    printfn "executing branchBye"
    Async.RunSynchronously(Async.Sleep(5000))
    let s1 = c.receive(C, helloCallback1)
    let s2 = s1.sendBYE(C, helloCallback2b)
    printfn "done executing branchBye"
    s2.finish()

let branchRES (c: Fib.ADD) =
    printfn "executing branchBye"
    let s1 = c.receive(C, helloCallback1).sendBYE(C, helloCallback2a)
    s1.finish()


let s1 = s.sendHELLO(C, helloCallback21a) // InContext -> OutContext
let s2 = s1.receiveHELLO(C, helloCallback3)
let s3 = s2.on_branch(branchRES, branchBYE) // StateType -> StatetType; RES -> END

let start () = l.Start()

//register.State1 += hellocallback12
//register.State2 += hellocallback12



(*
let (|BYE|RES|) n =
  if n % 2 = 0 then BYE Fib.BYE else RES Fib.RES
*)


(*
let rec fibS(c:Fib.State14, n: int) =
    let s1 = c.receiveHELLO(S, fun data x -> data.x=x; ())
    let s2 = s1.receiveHELLO(S, fun data x -> data.y=x;())
    let s3 = s2.select(fun data -> "Sum")
    match s3 with
    | Fib.Sum -> let s4 = s3.sendSum(S, fun data x -> data.x + data.y).receiveRes(S, fun data x -> ())
                 fibS s4
    | Fib.Mul -> s3.sendMul(S, fun data -> data.x*data.y).finish()
*)
(*
let byeCallback (x:Fib.BYE) =
    printf "Bye handler"
    x.receive(C).finish()

let helloCallback1 (x:Fib.HELLO) =
    printfn "hello executed"
    let buf = new DomainModel.Buf<int>()
    x.receive(C, buf).finish()


let helloCallback (x:Fib.HELLO) =
    printfn "hello executed"
    let buf = new DomainModel.Buf<int>()
    x.receive(C, buf).finish()
*)
//s.receiveHELLO(C, p, p2)
//printfn "Done: %i!!!" 1 //(p.getValue())
//let res = newS.branch(test1, test2)

//let res = newS.send<0>(helloCallback)

//let res1 = newS.branch(byeCallback, helloCallback)


//let receiveHello x y = x + y
//let receiveBye x y z = x + y + z


//sendHELLO<2>(C, 2).receiveHELLO(C, p).sendHELLO<f>(C, 3)

//printfn "Done: %i!!!" 2 //(p.getValue())



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


(*
type UpdateMonad<'TState, 'TUpdate, 'T> =
  UM of ('TState -> 'TUpdate * 'T)

type State
type Update =
  static Unit    : Update
  static Combine : Update * Update -> Update
  static Apply   : State * Update -> State

let inline unit< ^S when ^S :
    (static member Unit : ^S)> () : ^S =
  (^S : (static member Unit : ^S) ())

/// Invokes Combine operation on a pair of ^S values
let inline (++)< ^S when ^S :
    (static member Combine : ^S * ^S -> ^S )> a b : ^S =
  (^S : (static member Combine : ^S * ^S -> ^S) (a, b))

/// Invokes Apply operation on state and update ^S * ^U
let inline apply< ^S, ^U when ^U :
    (static member Apply : ^S * ^U -> ^S )> s a : ^S =
  (^U : (static member Apply : ^S * ^U -> ^S) (s, a))

*)
(*
type LoggingBuilder() =
    let log p = printfn "expression is %A" p

    member this.Bind(x, f) =
        log x
        f x

    member this.Return(x) =
        x

let logger = new LoggingBuilder()

let loggerWorkflow =
    logger {
       let! x = 42
       let! y = x
       return y
    }


type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        |None -> None
        |Some a -> f a

    member this.Return(x) =
        Some(x)

let maybe = MaybeBuilder()
*)