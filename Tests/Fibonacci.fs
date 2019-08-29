module Tests.Fib

open Expecto

let runFibServer () =
    async {
        Tests.FibServer.start ()
    }

let runFibClient () =
    async {
        Tests.FibClient.start ()
    }

[<Tests>]
let fibTests =
    testList "Fibonacci" [
        test "Fibonacci runs" {
            [runFibServer (); runFibClient ()] |> Async.Parallel |> Async.RunSynchronously |> ignore
        }
    ]