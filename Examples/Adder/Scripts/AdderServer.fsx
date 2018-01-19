#r "../../../src/Sast/bin/Debug/Sast.dll"
#load "AdderData.fsx"

open ScribbleGenerativeTypeProvider
                        

type AdderS = 
        Provided.TypeProviderFile<"../../../Examples/Adder/Protocols/Adder.scr", "Adder", "S" ,"../../../Examples/Adder/Config/configS.yaml"
            ,Delimiter=AdderData.delims, TypeAliasing=AdderData.typeAliasing, AssertionsOn=true>

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