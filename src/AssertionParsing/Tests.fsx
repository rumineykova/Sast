#r "bin/debug/AssertionParsing.dll"

open AssertionParsing
open AssertionParsing.FuncGenerator
open AssertionParsing.AssertionParser
open AssertionParsing.InferredVarsParser
open AssertionParsing.Visitors
open Microsoft.FSharp.Quotations

//let newtest = getQExpr (parseAssertionExpr "x+3") 



parseInfVars "{y1:v2,y2:v2}"

let test expr = 
    match (parse expr) with 
        | Some res -> genLambdaFromExpr res 
        | None -> "No result"

let plus = <@ 2 + 3 @>

let s = Quotations.Expr.Applications(plus, [[Quotations.Expr.Value(2)];[Quotations.Expr.Value(3)]])



test "(x>3)"
test "x+1 < y+2 || x > 1"
test "x+1 <5 && x<5 && x=y" 
test "(x+1 <5 && x<5   && z=y)" 
test " x<3 && y<6 "
test " x+1 < y*3 "
test "x+1 > 4"
test "x=y"
test "x<y && x>10 || z+y>s1" 
test "x<y"



(*
let payloads = ["x"; "y"; "z"; "w"]
let inferred = [("x", "4");("z", "5")] |> dict
let buffers = ["1"; "2"]

let mergeBuffersAndpayloads payloads inferred buffers = 
    let mutable index = 0
    payloads |> List.map (fun elem -> 
                          if (inferred.ContainsKey(elem)) then inferred.Item elem
                          else let res = buffers.Item index
                               let index= index + 1
                               res)
*)

 