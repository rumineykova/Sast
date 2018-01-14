module ScribbleGenerativeTypeProvider.Runtime
    open ScribbleGenerativeTypeProvider.DomainModel

    let setResults results (bufs:ISetResult []) = 
        Seq.zip results (Array.toSeq bufs) |> Seq.iter (fun (res,buf:ISetResult) -> buf.SetValue(res))
