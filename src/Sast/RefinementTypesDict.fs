module ScribbleGenerativeTypeProvider.RefinementTypesDict

open System.Collections.Concurrent
open ScribbleGenerativeTypeProvider.RefinementTypes.RefinementTypes

type LoopUpDict() =     
    let dictFunInfos = ConcurrentDictionary<string,FnRuleInfos>()
    let dictArgInfos = ConcurrentDictionary<string,ArgInfos>()
   
    let (|TryGetValueDict|_|) key (dict:ConcurrentDictionary<'a,'b>) =
        match dict.TryGetValue key with
        | true, args -> Some args
        | _ -> None

    member x.Index() = dictFunInfos.Count  

    member x.addFooFunction (fnRule:FnRule) = 
        let _ = dictFunInfos.AddOrUpdate(fnRule.fnName,fnRule.fnInfos,(fun _ _ -> fnRule.fnInfos))        
        ()

    // TODO : Throw exception if new arg type is different from old one
    member x.addArgInfos (arg:Arg) =
        let valueFactory _ (oldArgInfos:ArgInfos) =
            arg.argInfos

        let _ = dictArgInfos.AddOrUpdate(arg.argName,arg.argInfos,valueFactory)        
        ()
    
    member x.getArgValue (arg:string) = 
        match dictArgInfos with
        | TryGetValueDict arg args -> 
            match args.value with
            | None -> failwith "no value has been initialize yet"
            | Some value -> value
        | _ -> failwith "This arg hasn't been added yet to the dictionary"
    
    member x.getFooValue (fooName:string) = 
        match dictFunInfos with
        | TryGetValueDict fooName fnRule -> fnRule
        | _ -> failwith "This function doesn't exist in the dictionary yet"

    

    (*** ********************** ***)
    (***   Modular Functions    ***)
    (*** ********************** ***)    

    /// run the foo function built at compile-time 
    member x.runFooFunction (fooName:string) =
        let fnRule = x.getFooValue fooName        
        let untypedFn = fnRule.untypedFn
        let args = 
            let tmp = fnRule.argNames
            [ for arg in tmp do
                yield x.getArgValue arg
            ]
        untypedFn args

    /// at compile-time (we add all the [functions + arguments] from the assertion to the dictionaries)
    member x.addToDict (fnRule,argList) = 
        x.addFooFunction fnRule
        for arg in argList do
            x.addArgInfos arg
    
    /// at runtime (we add the value, associated to an argument and provided by the user, inside the arguments dictionary. We will then grab the value and evaluate the 
    /// assertions with the values put at run-time. It's also done this way, for latter when we'll have compile-time solutions.)
    member x.addArgValue (argName:string) (value:obj) =
        match dictArgInfos with
        | TryGetValueDict argName args -> 
            let newArgs =
                {   argName = argName 
                    argInfos = { args with value = Some value }
                } 
            x.addArgInfos newArgs
        | _ -> failwith "This arg hasn't been added yet to the dictionary"
        
let createlookUp = new LoopUpDict()