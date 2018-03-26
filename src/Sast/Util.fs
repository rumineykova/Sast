namespace ScribbleGenerativeTypeProvider.Util

open System.IO
open System.Diagnostics

module Debug = 
    let doPrinting = true
    let print message data =
        if doPrinting then
            printfn "%s %A" message data

module TimeMeasure =     
    let mutable stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let path = "C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Evaluation/"
    let file = "C:/Users/rn710/Repositories/GenerativeTypeProviderExample/Evaluation/temp.txt"

    let start() = 
        stopWatch.Stop()
        stopWatch <- System.Diagnostics.Stopwatch.StartNew()

    let measureTime (step:string) = 
        stopWatch.Stop()
        let numSeconds = stopWatch.ElapsedTicks / Stopwatch.Frequency
        let curTime = sprintf "%s: %i \r\n" step stopWatch.ElapsedMilliseconds
        File.AppendAllText(file, curTime)
        stopWatch <- Stopwatch.StartNew()

module ListHelpers = 
    let rec alreadySeenLabel (labelList:(string*int) list) (elem:string*int) =
        match labelList with
            | [] -> false
            | (hdS, hdI)::tl ->  
                if hdS.Equals(elem |> fst) && hdI.Equals(elem |> snd) then true
                else alreadySeenLabel tl elem

    let rec alreadySeenOnlyLabel (liste:(string*int) list) (elem:string) =
        match liste with
            | [] -> false
            | (hdS,_)::tl ->  
                if hdS.Equals(elem) then true
                else alreadySeenOnlyLabel tl elem


    let rec containsRole (liste:string list) (role:string) =
        match liste with
            | [] -> false
            | hd::tl -> if hd.Equals(role) then true
                        else containsRole tl role

    let isInList (aList:_ list) x = 
        let rec aux list x In =
            match list with
                |[] -> In
                |hd :: tl -> match hd with
                                |n when n=x -> true
                                | _ -> aux tl x In
        in aux aList x false

    let isInOne (weirdList: string list list) (str:string) =
        let rec aux (list1: string list list) =
            match list1 with
                |[] -> false
                |hd::tl -> if (List.exists (fun elem -> elem = str) hd) then
                               true
                           else
                               aux tl
        aux weirdList
    