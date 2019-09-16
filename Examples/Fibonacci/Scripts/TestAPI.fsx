#r "../../../src/Sast/bin/Debug/Sast.dll"

open ScribbleGenerativeTypeProvider
                        
[<Literal>]
let delims1 = """ [ {"label" : "SUM", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } },
                   {"label" : "RES", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "BYE", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "ADD", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }, 
                   {"label" : "HELLO", "delims": {"delim1": [":"] , "delim2": [","] , "delim3": [";"] } }]"""

[<Literal>]
let typeAliasing1 = """ [ {"alias" : "int", "type": "System.Int32"},
                          {"alias" : "string", "type": "System.String"}] """

type Fib = Provided.STP<"../Protocols/Fib.scr", "Adder", "S"
    ,"../Config/configC.yaml", Delimiter=delims1
    ,TypeAliasing=typeAliasing1, AssertionsOn=true, ScribbleSource= ScribbleSource.LocalExecutable>



let C = Fib.C.instance
let S = Fib.S.instance

// instance of type provider 
let fibServer = Fib()

// State objects that correspond to CFSM states 
// 3) BranchingStates (state with more than one input transition): list of inputStates (split into label, and payload transitions)
// the internal transition is receive label 
// 4) SelectorStates (state with more than one output transition): list of outputStates (split into label and payload transition)
// the internal transition is send label 
// 5) TerminalState (state with no transitions) // we assume there is at least one terminal state // 1) InputStates (state with exactly one output transition)

// Additional "State objects" introduced by the API code generation 
// 1) InputStates (state with exactly one output transition) -> l = receive (label, payload)
// 2) OutputStates (state with exactly one input transition) -> l = send (label, payload)


// Handlers correspond to CFSM transitions 
// 1) handler ::= inputhandler | outpuHandler 
// 2) inputHandler:  B -> unit 
// 3) outputHandler: unit -> B 
// 4) selectors::= inputSelector | outputSelector 
// 5) outputSelector::= unit -> OutputState
// 6) inputSelector::= InputState -> unit 

// operations on states 
// 1) InputState.register_inputHandler: inputHandler -> State // where state is 1)-5)
// 2) OutState.register_outputHandler: outpuHandler -> State
// 2) BranchState.register_inputStates: (InputStates_i -> End)_i -> unit 
// 2) SelectorState.register_outputStates: outputSelector -> (OutputStates_i -> End)_i -> unit 

// Example: endpoint program for S 
// get the first state (State20)
let s:Fib.State20 = fibServer.Init() // delta(s, a) = ri.li(Ti)
let s1:Fib.State22 = s.register_outputHandler(fun () -> 2)
let s2:Fib.BranchState23 = s.register_inputHandler(fun x -> ())

if singleton: ((Label -> B) -> (State -> End) // ((Label -> B) -> State 
// otherwise ((Label_i-> B_i -> State_i -> End)_i 

(*Label-> B -> State -> End 
register_input: Label-> B -> unit -> State
cont: State-> Done *)

let s:SelectState20 = 
s0.register_outputs ( 
    fun () -> if smth then HELLO.setU<2>() else HELLO.setU<2>(), // unit -> Hello(int) 
    fun (v:int) s: BranchState22 -> // int -> BranchState22 -> Done 
            s.register_inputs( // (Label_i(B_i) -> State_i -> Done)_i
                fun a:int addState:SelectState24 -> // ADD1:(a:int -> State:SelectState24) -> Done 
                    addState.register_outputs(() -> ADD2(3), // unit -> Add(int) -> SelectState24 -> Done  
                        fun s:SelectState20' -> handler1 s) 



// =====================
let s:SelectState20 = 
s0.register_outputs ( 
    fun () -> HELLO.setU<2>(), // unit -> Hello(int) 
    fun (v:int) s: BranchState22 -> // int -> BranchState22 -> Done 
            s.register_inputs( // (Label_i(B_i) -> State_i -> Done)_i
                fun a:int addState:SelectState24 -> // ADD1:(a:int -> State:SelectState24) -> Done 
                    addState.register_outputs(() -> ADD2(3), // unit -> Add(int) -> SelectState24 -> Done  
                        fun s:SelectState20' -> handler1 s) 
SelectState: 
payload:a -> cont:b-> done

InputState
payload:a-> cont:b
b-> c

//let s':SelectState20' = 
//s0'.register_outputs ( // 
let handler1 s:SelectState20' -> 
    s.register_outputs(
    fun () -> HELLO.setU<-1>(), // unit -> Hello(int) 
    fun (v:int) s: BranchState22' -> // (Label_i(B_i) -> State_i -> Done)_i
        s.register_inputs(  // 
            fun a:int byeState:SelectState25' -> // ADD1:(a:int -> State:SelectState24) -> Done 
                byeState.register_outputs(() -> BYE2(3), // unit -> Add(int) -> SelectState24 -> State  
                    fun s:SelectState25 -> s.finish())







let s:SelectState20 = 
s0.register_outputs (
    fun () -> HELLO.setU<2>(), // unit -> Hello(int) 
    fun (v:int) s: BranchState22 -> // int -> BranchState22 -> Done 
            s.register_inputs( 
                fun a:int addState:SelectState24 -> // ADD1:(a:int -> State:SelectState24) -> Done 
                    addState.register_outputs(() -> ADD2(3), // unit -> Add(int) -> SelectState24 -> State  
                        fun s:SelectState20' -> s.finish()) 

                fun b:int byeState:SelectState25 -> // BYE1:(b:int, State:BranchState25) -> Done
                    byeState.register_outputs(() -> BYE2(3), fun s:DONE -> s.finish()) 


if singleton: (() -> (Label -> B)) -> (State -> End) // (() -> Label -> B) -> State
let s3:unit = s.register_outputs( // (() -> (Label_j(B_j)) -> ((State_i)-> End)_i  
    fun () -> //unit -> DU(B-> cont: State), 
        let a = consolereadline()
        if a==5 then Fib.Add(a)
        else Fib.Bye()
    
    fun addCont:OutputState24 -> // ADD:(State:OutputState24) -> End, 
        addState.register_outputHandler(...).finish()), 

    fun byeCont:OutputState25 -> // BYE:(State:OutputState25) -> End,
        byeState.register_outputHandler(...).finish())) 

let s3:unit = s.register_inputStates(
    ADD:(a:int, State:OutputState24) -> unit, 
    BYE:(b:int, State:OutputState25) -> unit,
    
    fun addState:InputStateAdd -> 
        addState.register_inputHandler(...)
                .register_outputHandler(...)), 

    fun byeState:InputStateBye -> 
        byeState.register_inputHandler()
                .register_outputHandler())) 


