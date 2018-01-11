#r "bin/debug/AssertionParsing.dll"

open AssertionParsing
open AssertionParsing.FuncGenerator
open AssertionParsing.AssertionParser
open AssertionParsing.Visitors

let test expr = 
    match (parse expr) with 
        | Some res -> genLambdaFromExpr res 
        | None -> "No result"

test "(x<3)"
test "x+1 < y+2 || x > 1"
test "x+1 <5 && x<5 && x=y" 
test "(x+1 <5 && x<5   && z=y)" 
test " x<3 && y<6 "
test " x+1 < y*3 "
test "x+1 > 4"
test "x=y"
test "x<y && x>10 || z+y>s1" 
test "x<y"