#r "../../../src/Sast/bin/Debug/net452/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims = """ [ {"label" : "Query", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                    {"label" : "Quote", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "Dummy", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Bye", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "No", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "Yes", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""


[<Literal>]
let typeAliasing =
    """ [ {"alias" : "int", "type": "System.Int32"} ] """

//C:\Users\rn710\Repositories\scribble-java\scribble-demos\scrib\travel\src\travel\Travel.scr
// fsm is inL C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Examples/LargeTests/FSM/TravelAgencyA.txt
type TravelA = 
    Provided.TypeProviderFile<"../../../Examples/CCTests/Protocols/Travel.scr"
                               ,"Booking"
                               ,"A"
                               ,"../../../Examples/CCTests/Config/configTA.yaml"
                               ,Delimiter=delims
                               ,TypeAliasing=typeAliasing
                               ,ScribbleSource = ScribbleSource.LocalExecutable
                               ,ExplicitConnection=true
                               ,AssertionsOn=false>


let session = new TravelA()
session.Init()
