
module Tests.Main

open Expecto

[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args Tests.Fib.fibTests
