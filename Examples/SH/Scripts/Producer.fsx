#r "../../../src/Sast/bin/Debug/net452/Sast.dll"
#load "SHData.fsx"
open SHData
open ScribbleGenerativeTypeProvider
open ScribbleGenerativeTypeProvider.DomainModel

module Producer = 
    type SH_P = 
        Provided.TypeProviderFile<"../../../Examples/SH/Protocols/SHHello.scr", "SH" ,"P" , "../../../Examples/SH/Config/configP.yaml", SHData.delims 
            ,SHData.typeAliasing, ScribbleSource.LocalExecutable
            ,ExplicitConnection = false, AssertionsOn = true>
    
    let numIter = 3
    let R = SH_P.R.instance
    let C = SH_P.C.instance
    let sh = new SH_P()
    let rec calcClipPoints (vert: int list)  (c:SH_P.State52) =
        let res1 = new DomainModel.Buf<int>()    
        let res2 = new DomainModel.Buf<int>()    
        let res3 = new DomainModel.Buf<int>()    
        match vert with 
        | [hd] -> let c1 = c.sendClose(R).sendClose(C)
                  printf "All points received"
                  c1.finish()
        | hd1::hd2::tail -> 
            let c1 = c.sendIsAbove(R, hd1).receiveRes(R, res1).sendIsAbove(R, hd2)
                      .receiveRes(R, res2).sendIntersct(R, hd1, hd2).receiveRes(R, res3)
            let c1 = if (res2.getValue()=0) then c1.sendSecOut(C, res3.getValue())
                     else c1.sendSecIn(C, res3.getValue(), hd2)

            calcClipPoints (hd2::tail) c1
        
    let polygon = {1..10} |> Seq.toList

    let dum = new DomainModel.Buf<int>()    
    let startC = sh.Init().sendhello(R).receivehello(R).sendhello(C).receivehello(C).sendPlane(R, 1, 2, 3, 4)
    let s = sprintf "SH measure with no assertions"
    polygon |> calcClipPoints <| startC
    
