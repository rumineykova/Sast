#r "bin/debug/Sast.dll"

open ScribbleGenerativeTypeProvider.AsstScribbleParser
open ScribbleGenerativeTypeProvider.DomainModel

let test = """
digraph G {
compound = true;
"9" [ label="9:  <> True" ];
"9" -> "11" [ label="S!HELLO(x: int)(x > 7)<>" ];
"13" [ label="13:  <> True" ];
"13" -> "14" [ label="S!ADD(_dum3: int)True<>" ];
"13" -> "15" [ label="S!BYE()True<>" ];
"12" [ label="12:  <> True" ];
"12" -> "13" [ label="S!HELLO(_dum2: int)True<>" ];
"11" [ label="11:  <> True" ];
"11" -> "12" [ label="S?HELLO(_dum1: int)True<>" ];
"10" [ label="10:  <> True" ];
"15" [ label="15:  <> True" ];
"15" -> "10" [ label="S?BYE()True<>" ];
"14" [ label="14:  <> True" ];
"14" -> "10" [ label="S?RES(_dum4: int)True<>" ];
}

"""

let typeAliasing = Map.empty.Add("int", "System.Int32")

let config = sprintf """{"code":"%s","proto":"%s","role":"%s"}""" "code" "Adder" "C"
let testScribble = Parsing.getFSMJson test config typeAliasing

match Parsing.getFSMJson test config typeAliasing with 
    | Some parsed -> 
        parsed
    | None -> failwith "The file given does not contain a valid fsm"
