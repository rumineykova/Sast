#r "../../../src/Sast/bin/Debug/Sast.dll"
#load "SHData.fsx"
open SHData
open ScribbleGenerativeTypeProvider
open ScribbleGenerativeTypeProvider.DomainModel

module Consumer = 
    type SH_C = 
        Provided.TypeProviderFile<"../../../Examples/SH/Protocols/SHHello.scr","SH", "C" ,"../../../Examples/SH/Config/configC.yaml" 
            ,Delimiter = SHData.delims ,TypeAliasing = SHData.typeAliasing ,ScribbleSource = ScribbleSource.LocalExecutable,
            AssertionsOn = true>
    
    let P = SH_C.P.instance
    let rec printPoints (c:SH_C.State29) =
        let res = new DomainModel.Buf<int>()    
        match c.branch() with 
        | :? SH_C.BothIn  as point -> 
            let c1 = point.receive(P, res)
            printf "Point received: %i" (res.getValue())
            printPoints c1  
        | :? SH_C.BothOut as none->  
            printf "No points received"
            printPoints (none.receive(P))
        | :? SH_C.SecOut as one -> 
            let c1 = one.receive(P, res)
            printf "One point received %i" (res.getValue())
            printPoints c1  
        | :? SH_C.SecIn as two -> 
            let res2 = new DomainModel.Buf<int>()    
            let c1 = two.receive(P, res, res2)
            printf "Two points received"
            printPoints c1
        | :? SH_C.Close as close -> close.receive(P).finish() 
         
    let sh_c = new SH_C()
    let dum = new DomainModel.Buf<int>()   
    printPoints (sh_c.Init().receivehello(P).sendhello(P))
