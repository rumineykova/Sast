#r "../../src/Sast/bin/Debug/Sast.dll"

open System
open ScribbleGenerativeTypeProvider


[<Literal>]
let delims = """ [ {"label" : "GET", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "POST", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "HOST", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "BODY", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "HTTP", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "d200", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "d404", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "ContentLength", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "CONTENTType", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } } ] """


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "string", "type": "System.String"},
          {"alias" : "int", "type": "System.Int32"},
          {"alias" : "date", "type": "System.DateTime"} ] """

type Http = Provided.TypeProviderFile<"../../../Examples/Http/http.scr"
                                      ,"Http"
                                      ,"S"
                                      ,"../../../Examples/Http/http.yaml"
                                      ,Delimiter=delims
                                      ,TypeAliasing = typeAliasing  
                                      ,ScribbleSource = ScribbleSource.WebAPI>


let bufs = new DomainModel.Buf<string>()
let bufb = new DomainModel.Buf<byte[]>()
let server = Http.S.instance
let client = Http.C.instance
let sWorld = "Hello World!"
let sFahd = "Hello Fahd!"


let rec headerGet (branch: TypeChoices.Choice2)=
  match branch with
    | :? Http.HOST as host -> let result = host.receive(client,bufs)
                              headerGet (result.branch())
    | :? Http.BODY as body -> body.receive(client,bufs)                             

let rec headerPost (branch: TypeChoices.Choice2)=
  match branch with
    | :? Http.HOST as host -> let result = host.receive(client,bufs)
                              headerPost (result.branch())
    | :? Http.BODY as body -> body.receive(client,bufs)


let rec httpFun (firstState:Http.State24) =                                                            
  let verb = firstState.branch()
  match verb with
    | :? Http.GET as get -> let receiveGet = get.receive(client,bufs)
                            let branch = receiveGet.branch()
                            let result = headerGet branch
                            let httpSend = result.sendHTTP(client,"1.1")
                            let sent200 = httpSend.sendd200(client,"OK")
                            let contentType = sent200.sendCONTENTType(client,"text/html; charset=UTF-8")
                            let contentLength = contentType.sendContentLength(client,12)
                            let last = contentLength.sendBODY(client,sWorld)
                            last.finish()

    | :? Http.POST as post -> let receivePost = post.receive(client,bufs)
                              let branch = receivePost.branch()
                              let result = headerPost branch
                              let httpSend = result.sendHTTP(client,"1.1")
                              let sent404 = httpSend.sendd404(client,"NOT_FOUND")
                              let contentType = sent404.sendCONTENTType(client,"text/html; charset=UTF-8")
                              let contentLength = contentType.sendContentLength(client,11)
                              let last = contentLength.sendBODY(client,sFahd)
                              last.finish()
    | _ -> ()

let http = new Http()
http.Start() |> httpFun 




(*
HTTP/1.1 200 OK
Content-Type: text/html; charset=UTF-8
Content-Length: 12

Hello World!


HTTP/1.1 404 NOT_FOUND
Content-Type: text/html; charset=UTF-8
Content-Length: 12

Hello Fahd!



GET /index.html HTTP/1.1
Host: www.example.com

POST /index.fs HTTP/1.1
Host: www.example.com
*)