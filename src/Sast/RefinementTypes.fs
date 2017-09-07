module ScribbleGenerativeTypeProvider.RefinementTypes

open FSharp.Core.CompilerServices
open Microsoft.FSharp.Compiler.Interactive.Shell
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

open System.Reflection 
open System.Text
open System.IO


module Fsi =
    // Create an interactive checker instance 
//    let checker = FSharpChecker.Create()

    // Intialize output and input streams
    let sbOut = new StringBuilder()
    let sbErr = new StringBuilder()
    let inStream = new StringReader("")
    let outStream = new StringWriter(sbOut)
    let errStream = new StringWriter(sbErr)

    // Build command line arguments & start FSI session
    let argv = [| "C:\\fsi.exe" |]
    let allArgs = Array.append argv [|"--noninteractive"|]

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream) 


module RefinementTypes =
    open Fsi
    open System
    open System.Collections.Concurrent
    open FSharp.Reflection
    open Microsoft.FSharp.Compiler

    type FnRuleInfos =
        {
            fnType      : SourceCodeServices.FSharpType
            untypedFn   : obj list -> bool option
            argNames    : string list        
        }
    type FnRule =
        {
            fnName  : string
            fnInfos : FnRuleInfos
        }

    
    type ArgInfos =
        {
            argType : SourceCodeServices.FSharpType // Type maybe
            value   : obj option // boxed value
        }
    type Arg = 
        {
            argName     : string
            argInfos    : ArgInfos
        }
        
    let parseAndCheckSingleFile (input) = 
        let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")
        File.WriteAllText(file, input)
        let checker = SourceCodeServices.FSharpChecker.Create(keepAssemblyContents=true)

        let projOptions = 
            checker.GetProjectOptionsFromScript(file, input)
            |> Async.RunSynchronously

        checker.ParseAndCheckProject(projOptions |> fst) 
        |> Async.RunSynchronously


//    let fstRule = "fun y (x: System.Int32 List) -> x.Length < y+3"
//    let sndRule = "fun y -> 3 < y + 1"

    (*** ************************************************************************ ***)
    (*** ************************************************************************ ***)
    (***           2)  Extract variables in order for each functions              ***)
    (*** ************************************************************************ ***)
    (*** ************************************************************************ ***)
    
    let generateFooAndGetArgsInfos index ruleFunction =
        let input = 
            """
        module MyLibrary 
        open System
            """ 
        let foo = sprintf "let foo%i = %s" index ruleFunction
        let singleFile = input + foo

        let checkProjectResults = parseAndCheckSingleFile(singleFile)
        let err = checkProjectResults.Errors 
        // should be empty
        match err with 
        | [||] -> 
            let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
    
            let rec getKeys decls = 
                match decls with 
                | FSharpImplementationFileDeclaration.Entity (_, subDecls) -> 
                    let subDecl = subDecls.Head
                    getKeys subDecl
                | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(value, valueListList, e) ->
                    let argList =
                        [
                            for valueList in valueListList do
                                yield valueList.Head
                        ]
                    // TODO : Return a List of parameters with information regarding the type...
                    // TODO : Maybe add events between functions such that our functions can trigger 
                    // the evaluation at compile-time if that's possible (Not run-time because e verything will be written once!!)
                    
                    // we return the couple (foo_i, [param1, param2, param3])
                    (value.CompiledName,argList,value) 
                | FSharpImplementationFileDeclaration.InitAction(_) -> 
                    failwith "unexpected InitAction in file Declaration!! Issue with library not protocol specification" 

            checkedFile.Declarations.Head
            |> getKeys
        | _ ->
            failwith (sprintf "%A" err.[0])


//    let (foo,keys,value) = generateFooAndGetArgsInfos 1 fstRule    
//    let res = value.FullType

    (*** ************************************************************************ ***)
    (*** ************************************************************************ ***)
    (***           3)  Untyped evaluation of function rules                       ***)
    (*** ************************************************************************ ***)
    (*** ************************************************************************ ***)

    /// TODO : provide some nice verification over this untyped reflective verification
    /// TODO : Add error handling here in a proper manner
    let invokeUntype (fn:obj) (args : obj list) =
        let rec helper (partialFn : obj) (args : obj list)  =
            let fnType = partialFn.GetType()

            match args.IsEmpty with
            | false ->
                if FSharpType.IsFunction fnType then
                    let methodInfo =
                        fnType.GetMethods()
                        |> Array.filter (fun x -> x.Name = "Invoke" && x.GetParameters().Length = 1)
                        |> Array.head
                    let nextPartialFn = methodInfo.Invoke(partialFn, [| args.Head |])
                    helper nextPartialFn args.Tail
                else 
                    None
            | true ->
                if fnType = typeof<bool> then
                    let res = partialFn :?> bool
                    Some res
                else
                    None
        helper fn args
    
    // TODO : Provide error handling, this can throw weird exceptions need to wrap that properly
    let untypedEvaluation ruleFunction =
        //let test = "fun (p2:int) (res:int) (z:int) -> z = p2 && res = 0"
        let evaluatedExpression = fsiSession.EvalExpression(ruleFunction)
        evaluatedExpression.Value.ReflectionValue
        |> invokeUntype 
        
//    let res = untypedEvaluation sndRule    // [5;4]
//    let res2 = res [8;4]

    let createFnRule index ruleFunction = 
        let (fooName, args, fooType) = generateFooAndGetArgsInfos index ruleFunction
        let (untypedFoo) = untypedEvaluation ruleFunction
        
        let argList = 
            [ for arg in args do
                  yield { argName = arg.CompiledName
                          argInfos = 
                              { argType = arg.FullType
                                value = None } } ]
        
        let fnRule = 
            { fnName    = fooName
              fnInfos   = 
                  { 
                    fnType    = fooType.FullType              
                    untypedFn = untypedFoo
                    argNames = args |> List.map (fun x -> x.CompiledName) } }
        
        ((fnRule, argList),(fooName,fnRule.fnInfos.argNames))


    (*** ************************************************************************ ***)
    (*** ************************************************************************ ***)
    (***            4)  We add 2 Dictionaries for the rules                       ***)
    (*** ************************************************************************ ***)
    (*** ************************************************************************ ***)


    (*** ****************** ***)
    (***   Local Functions  ***)
    (*** ****************** ***)



//    let fstEl = createFnRule 1 fstRule
//    let sndEl = createFnRule 2 sndRule
//    
//
//    addToDict fstEl
//    addToDict sndEl
//    addFooFunction (fstEl |> fst)
//    addFooFunction (sndEl |> fst)
//    for arg in fstEl |> snd do
//        addArgInfos arg
//    for arg in sndEl |> snd do
//        addArgInfos arg
//
//    dictArgInfos
//    dictFunInfos
//    getArgValue "x"            




//module test =
//
////    type Rule<'a> =
////        | FinishedRule of ('a -> bool)
////        | UnfinishedRule of ('a -> Rule<'a>)
//
//    type Validation<'a> =
//        | ValidationError
//        | ValidationOk
//        | ValidationContinuation of ('a -> Validation<'a>)
//
//
//    let fn = "fun x y -> x<y" -> <@ fun x y -> x<y @>
//    
//    let fn = "fun x y -> x<y" int -> int -> bool
//    let fn2 = "fun x -> x = 7" int -> bool
//    
//
//    let something =
//        fun x ->
//            let inner y =
//                if x<y then
//                    ValidationOk
//                else
//                    ValidationError
//            if x<5 then
//                inner |> ValidationContinuation
//            else
//                ValidationError
//
//    let quotation = <@ x<y @>
//    
//    let sndQuot = 
//        <@@
//            let fn = 
//            
//        
//        @@>
//        
// 
//
//
//
//let c2 = c1.branch() 
//match c2 with 
//| :? Case1 as case1 ->    
//    let c3 = case1.branch()    
//    match c3 with       
//    | :? Case31 as case31 -> case31.SendHi().End()       
//    | :? Case32 as case32 -> case32.SendSomething().End()       
//    | :? Case33 as case33 -> case33.SendHi().ReceiveHiBack().End() 
//| :? Case2 as case2 -> case2.SendBye().End()        
//
//
//
//let h1 case1 =    
//    let h32 case32 = case32.SendSomething().End()    
//    let h33 case33 = case33.SendHi().ReceiveHiBack().End()    
//    let h31 case31 = case31.SendHi().End()    
//    
//    case1.branch().handle h31 h32 h33 
//    handle : (H31Type -> END) * (H32Type -> END) * (H33 -> END) -> END
//
//let h2 case2 = case2.SendBye().End() 
//let c2 = c1.branch().handle h1 h2
//
//
//                