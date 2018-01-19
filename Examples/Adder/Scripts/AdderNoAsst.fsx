#r "../../../src/Sast/bin/Debug/Sast.dll"

#load "AdderData.fsx"
open ScribbleGenerativeTypeProvider
 
module Client =          
    (*type AdderC = 
        Provided.TypeProviderFile<"../../../Examples/Adder/Protocols/AdderNoAss.scr", "Adder", "C"
            ,"../../../Examples/Adder/Config/configC.yaml", Delimiter=AdderData.delims
            ,TypeAliasing=AdderData.typeAliasing, ScribbleSource = ScribbleSource.LocalExecutable , AssertionsOn =true>*)
    
    type AdderC = 
        Provided.TypeProviderFile<"../../../Examples/Adder/FSM/FSMAdderNoAssC.txt", "Adder", "C"
            ,"../../../Examples/Adder/Config/configC.yaml", Delimiter=AdderData.delims
            ,TypeAliasing=AdderData.typeAliasing, ScribbleSource = ScribbleSource.File , AssertionsOn =true>
`   

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

module Server = 
    (*type AdderS = 
        Provided.TypeProviderFile<"../../../Examples/Adder/Protocols/AdderNoAss.scr", "Adder", "S" ,"../../../Examples/Adder/Config/configS.yaml"
            ,Delimiter=AdderData.delims, TypeAliasing=AdderData.typeAliasing, AssertionsOn=true>
    *)
    
    type AdderS = 
        Provided.TypeProviderFile<"../../../Examples/Adder/FSM/FSM/AdderNoAssS.txt", "Adder", "S" ,"../../../Examples/Adder/Config/configS.yaml"
            ,Delimiter=AdderData.delims, TypeAliasing=AdderData.typeAliasing, ScribbleSource = ScribbleSource.File, AssertionsOn=true>
    

    let C = AdderS.C.instance

    let rec adderServer (c0:AdderS.State20) =
        let res0 = new DomainModel.Buf<int>()
        let c = c0.receiveHELLO(C, res0)
        //printfn "After receive once"
        match c.branch() with 
            | :? AdderS.BYE as bye-> 
                printfn"receive bye"
                bye.receive(C).sendBYE(C).finish()
            | :? AdderS.ADD as add -> 
                printfn"receive add" 
                let res1 = new DomainModel.Buf<int>()
                let res2 = new DomainModel.Buf<int>()

                let c1 = add.receive(C, res1).receiveADD(C, res2)
                let c2 = c1.sendRES(C, res1.getValue() + res2.getValue())
                adderServer c2

    let session = new AdderS()
    let sessionCh = session.Init()
    adderServer(sessionCh)