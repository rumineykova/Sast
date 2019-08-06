﻿#r "../../../src/Sast/bin/Debug/Sast.dll"


open ScribbleGenerativeTypeProvider           
           
           
                        
[<Literal>]
let delims = """ [ {"label" : "SUM", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "ADD", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "RES", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BYE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "close", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing1 = """ [ {"alias" : "int", "type": "System.Int32"}, 
                          {"alias" : "string", "type": "System.String"}] """

(*
type Fib = Provided.STP<"../../../Examples/Fibonacci/Protocols/Fib.scr"
                       , "Adder" 
                       , "C"
                       , "../../../Examples/Fibonacci/Config/config.yaml"
                       ,Delimiter=delims
                       ,TypeAliasing=typeAliasing1
                       ,ScribbleSource = ScribbleSource.LocalExecutable
                       ,ExplicitConnection=false 
                       ,AssertionsOn=true>
*)

type Fib = 
   Provided.STP<"../Protocols/Fib.scr", "Adder", "C"
       ,"../Config/configC.yaml", Delimiter=delims
       ,TypeAliasing=typeAliasing1, AssertionsOn=true, ScribbleSource= ScribbleSource.LocalExecutable>


let S = Fib.S.instance
//et client = new AdderC()
(*let c = client.Init().receiveHELLO(S).finish()
let C = Fib.C.instance
let S = Fib.S.instance*)

let s = new Fib()
let c = s.Init()

type Runtime.IContext with
    member x.SetX(y) = printfn "Setting x to: %i" y
    member x.GetX() = printfn "Getting x:"

let helloCallback1 (ctx:Fib.InContext9) =   
    printfn "hello callback1" 
    Async.RunSynchronously(Async.Sleep(5000))
    printfn "hello callback1"  
    ctx.setv<2>()
    //let buf = 4
    //ctx.Add
    //buf

let helloCallback12A (ctx:Fib.InCtxADD2) =   
    printfn "hello callback1" 
    Async.RunSynchronously(Async.Sleep(5000))
    printfn "hello callback1"  
    ctx.seta<1>()

let helloCallback12B (ctx:Fib.InCtxBYE4) =   
    printfn "hello callback1" 
    Async.RunSynchronously(Async.Sleep(5000))
    printfn "hello callback1"  
    ctx.setb<0>()

let helloCallback123 ()  =
    printfn "hello callback1"
    Async.RunSynchronously(Async.Sleep(5000))
    printfn "hello callback1"
    let buf = 4
    buf

let helloCallback3  = 
    printfn "hello callback3"
    printfn "Done"
    let s =5
    s

let helloCallback2  x =   
    printfn "hello callback2"
    printfn "%A" x
    ()

let x = ()
let helloCallback4 (c: Fib.ADD)  =   
    printfn "hello callback2"
    c.send(S, helloCallback12A)
     .receiveBYE(S, helloCallback2)
     .finish()

let helloCallback5 (c: Fib.BYE)  =   
    printfn "hello callback2"
    c.send(S, helloCallback12B)
     .receiveBYE(S, helloCallback2)
     .finish()

let helloCallback6 (c: Fib.BYEADD)  =      
    printfn "hello callback2"
    let s = System.Console.ReadLine()
    if (s = "Hello") then c.selector<"BYE">()
    else c.selector<"ADD">()

let s1 = c.receiveHELLO(S, helloCallback2)
          .sendHELLO(S, helloCallback1)
          //.sendHELLO(S, helloCallback12)
          .register_selector(helloCallback6)
          .select_handlers(helloCallback5, helloCallback4)