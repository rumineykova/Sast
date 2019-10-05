module ScribbleGenerativeTypeProvider.TPConfigParser

open System.Diagnostics
open System.IO
open ScribbleGenerativeTypeProvider.DomainModel
open ScribbleGenerativeTypeProvider.ScribbleParser

let invokeScribble pathToFile protocol localRole tempFileName assertionsOn =         
  let scribbleArgs = 
    match assertionsOn with 
      | false -> 
          let batFile = """%scribbleno%""" 
          sprintf """/C %s %s -fsm %s %s >> %s 2>&1 """ 
            batFile pathToFile protocol localRole tempFileName
      | true -> 
          let batFile = """%scribble%""" 
          sprintf """/C %s %s -assrt  -fsm %s %s -z3  >> %s 2>&1 """ 
            batFile pathToFile protocol localRole tempFileName
  let psi = ProcessStartInfo("cmd.exe", scribbleArgs)
  psi.UseShellExecute <- false; psi.CreateNoWindow <- true;                                                           
  // Run the cmd process and wait for its completion
  let p = new Process()
  p.StartInfo<- psi;                             
  let res = p.Start(); 
  p.WaitForExit()
  // Read the result from the executed script
  let parsedFile = File.ReadAllText(tempFileName) 
  // TODO:  Fix the parser not to care about starting/trailing spaces!
  let parsedScribble = parsedFile.ToString().Replace("\r\n\r\n", "\r\n")
  parsedScribble 

let parseCFSM parsedScribble protocol localRole typeAliasing = 
  let str = sprintf """{"code":"%s","proto":"%s","role":"%s"}""" "code" protocol localRole
  match Parsing.getFSMJson parsedScribble str typeAliasing with 
      | Some parsed -> 
          parsed
      | None -> failwith "The file given does not contain a valid fsm"

type ConfigParser(resolutionFolder: string) = 
        
  member this. parseDelimeters delimitaters = 
    // handle configFile delim parameter (used for serialisation)
    let mutable mapping = Map.empty<string,string list* string list * string list>
    let instance = MappingDelimiters.Parse(delimitaters)
    for elem in instance do
        let label = elem.Label
        let delims = elem.Delims
        let delim1 = delims.Delim1 |> Array.toList
        let delim2 = delims.Delim2 |> Array.toList
        let delim3 = delims.Delim3 |> Array.toList
        mapping <- mapping.Add(label,(delim1,delim2,delim3)) 
    DomainModel.modifyMap mapping
 
  member this.parseConfigFile configFilePath = 
    let configFile = Path.Combine(resolutionFolder, configFilePath)
    match File.Exists(configFile) with 
        | true -> DomainModel.config.Load(configFile)
        | false -> failwith ("The path to the config folder is not correct: " + resolutionFolder + " " + configFile)
