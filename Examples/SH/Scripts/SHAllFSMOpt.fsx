#r "../../../src/Sast/bin/Debug/Sast.dll"
#load "SHData.fsx"
open SHData
open ScribbleGenerativeTypeProvider
open ScribbleGenerativeTypeProvider.DomainModel
        
module Calculator =  
    type SH_R = 
        Provided.TypeProviderFile<"../../../Examples/SH/FSM/ShFSMOpt_R.txt", "SH", "R", "../../../Examples/SH/Config/configR.yaml"
            ,SHData.delims, SHData.typeAliasing, ScribbleSource.File
            ,ExplicitConnection = false, AssertionsOn=true>

    let R = SH_R.R.instance
    let P = SH_R.P.instance

    let rec utilF () =
        let genRandomNumbers count =
            let rnd = System.Random()
            List.init count (fun _ -> rnd.Next ())
        let l = genRandomNumbers 1000 |> List.sort
        l

    // pass the functions for above: Point -> (Point-> Point-> Point -> Point)
    // intersect: Point1 -> Point2  -> Point3 s
    let rec calculate (c:SH_R.State12) =
        match c.branch() with 
        | :? SH_R.Close as c1 -> c1.receive(P).finish()
        | :? SH_R.IsAbove as c2 -> 
            let p1 = new DomainModel.Buf<int>()    
            let p2 = new DomainModel.Buf<int>()
            let c3 = c2.receive(P, p1)
            let r1 = utilF () // call Is above for p1 and the rectangular 
            let c4 = c3.sendRes(P, 0).receiveIsAbove(P, p2) 
            let r2 = utilF ()
            let c5 = c4.sendRes(P, 1).branch()
            let next =  
                match c5 with 
                | :? SH_R.BothIn as bin   -> bin.receive(P)
                | :? SH_R.BothOut as bout -> bout.receive(P)
                | :? SH_R.Intersct as it -> 
                    let p3 = new DomainModel.Buf<int>()    
                    let p4 = new DomainModel.Buf<int>()
                    let c6 = it.receive(P, p3, p4)
                    let r3 = utilF () 
                    // calculate the intersection of p3(p1) and p4(2)
                    c6.sendRes(P, 2)
            calculate next

    let sh_r = new SH_R()
    let res1 = new DomainModel.Buf<int>()    
    let res2 = new DomainModel.Buf<int>()    
    let res3 = new DomainModel.Buf<int>()    
    let res4 = new DomainModel.Buf<int>()    
    let c = sh_r.Init().receivePlane(P, res1, res2, res3, res4) 
    calculate c


module Producer = 
    type SH_P = 
        Provided.TypeProviderFile<"../../../Examples/SH/FSM/ShFSMOpt_P.txt", "SH" ,"P" , "../../../Examples/SH/Config/configP.yaml", SHData.delims 
            ,SHData.typeAliasing, ScribbleSource.File
            ,ExplicitConnection = false, AssertionsOn = true>
    
    let numIter = 3
    let R = SH_P.R.instance
    let C = SH_P.C.instance
    let sh = new SH_P()
    let rec calcClipPoints (vert: int list)  (c:SH_P.State32) =
        let res1 = new DomainModel.Buf<int>()    
        let res2 = new DomainModel.Buf<int>()    
        let res3 = new DomainModel.Buf<int>()    
        match vert with 
        | [hd] -> let c1 = c.sendClose(R).sendClose(C)
                  printf "All points received"
                  c1.finish()
        | hd1::hd2::tail -> 
            let c1 = c.sendIsAbove(R, hd1).receiveRes(R, res1).sendIsAbove(R, hd2)
                      .receiveRes(R, res2).sendIntersct(R).receiveRes(R, res3)
            let c1 = if (res2.getValue()=0) then c1.sendSecOut(C)
                     else c1.sendSecIn(C)

            calcClipPoints (hd2::tail) c1
        
    let polygon = {1..10} |> Seq.toList

    let dum = new DomainModel.Buf<int>()    
    let startC = sh.Init().sendPlane(R, 1, 2, 3, 4)
    let s = sprintf "SH measure with no assertions"
    polygon |> calcClipPoints <| startC

module Consumer = 
    type SH_C = 
        Provided.TypeProviderFile<"../../../Examples/SH/FSM/ShFSMOpt_C.txt","SH", "C" ,"../../../Examples/SH/Config/configC.yaml" 
            ,Delimiter = SHData.delims ,TypeAliasing = SHData.typeAliasing ,ScribbleSource = ScribbleSource.File
            ,ExplicitConnection = true, AssertionsOn = true>
    
    let P = SH_C.P.instance
    let rec printPoints (c:SH_C.State44) =
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
    printPoints (sh_c.Init()
