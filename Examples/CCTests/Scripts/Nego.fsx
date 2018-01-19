#r "../../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims = """ [ {"label" : "propose", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "accpt", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "confirm", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "reject", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "confirm", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

// C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Examples/LargeTests/FSM/NegoC.txt

type NegoC = 
    Provided.TypeProviderFile<"../../../Examples/CCTests/Protocols/Nego1.scr"
                               ,"Negotiation"
                               ,"C"
                               ,"../../../Examples/CCTests/Config/configNego.yaml"
                               ,Delimiter=delims
                               ,TypeAliasing=typeAliasing
                               ,ScribbleSource = ScribbleSource.LocalExecutable 
                               ,ExplicitConnection=true
                               ,AssertionsOn=false>



let session = new NegoC()
session.Init()
