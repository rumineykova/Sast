#r "../../../src/Sast/bin/Debug/Sast.dll"

#load "AdderData.fsx"
open ScribbleGenerativeTypeProvider
 
type AdderC = 
    Provided.TypeProviderFile<"../../../Examples/Adder/Protocols/Adder.scr", "Adder", "C"
        ,"../../../Examples/Adder/Config/configC.yaml", Delimiter=AdderData.delims
        ,TypeAliasing=AdderData.typeAliasing, AssertionsOn=true>

let numIter = 1000
let S = AdderC.S.instance

let rec adderRec a b iter (c0:AdderC.State8) = 
    let res = new DomainModel.Buf<int>()
    let c = c0.sendHELLO(S, 1)
    match iter with
        |0 -> 
            let c1 = c.sendBYE(S)
            let c2 = c1.receiveBYE(S)
            printfn "Fibo : %d" b
            let finalc = c2.finish()
            finalc
        |n -> 
            let c1 = c.sendADD(S, a).sendADD(S, b)
            printfn "Send ADD"   
            let c2 = c1.receiveRES(S, res)
            (*let foo s = if s > 0 then true else false 
            let result = foo (res.getValue())*)
            adderRec b (res.getValue()) (n-1) c2

let client = new AdderC()
let c = client.Init()
c |> adderRec 1 1 3