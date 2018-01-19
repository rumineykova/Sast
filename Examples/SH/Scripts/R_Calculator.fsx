#r "../../../src/Sast/bin/Debug/Sast.dll"
#load "SHData.fsx"
open SHData
open ScribbleGenerativeTypeProvider
open ScribbleGenerativeTypeProvider.DomainModel
        
module Calculator =  
    type SH_R = 
        Provided.TypeProviderFile<"../../../Examples/SH/Protocols/SHHello.scr", "SH", "R", "../../../Examples/SH/Config/configR.yaml"
            ,SHData.delims, SHData.typeAliasing, ScribbleSource.LocalExecutable
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
    let rec calculate (c:SH_R.State16) =
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

    let c = sh_r.Init().receivehello(P).sendhello(P)
    c.receivePlane(P, res1, res2, res3, res4) |> calculate
        