//#r "./packages/FSharp.Data/lib/net45/FSharp.Data.dll"
//#r "./packages/FParsec/lib/net45-client/FParsecCS.dll"
//#r "./packages/FParsec/lib/net45-client/FParsec.dll"

namespace AssertionParsing
open FParsec
//===============


type Op =
        | Minus | Plus | Subtract | Multiply 
        | LT | GT | Eq | NotEq | LTEq | GTEq | AndOp | OrOp
        with 
            override x.ToString() =
                match x with
                | Minus -> "-"
                | Plus -> "+"
                | Subtract -> "-"
                | Multiply -> "*"
                | LT -> "<"
                | GT -> ">"
                | Eq -> "="
                | NotEq -> "<>"
                | LTEq -> "<="
                | GTEq -> ">="  
                | AndOp -> "&&"
                | OrOp -> "||"
            
            member x.ToExpr() = 
                match x with 
                    | Plus -> <@ (+) @>
                    | Subtract -> <@ (-) @>
                    | Multiply -> <@ (*) @>
                    | _ -> failwith "Only arithmetic operations are supported"

       

type identifier = string 
         
type Value =
        // Primitives
        | Bool  of bool   (* true | false *)
        | IntC of System.Int32 
        | Ident of identifier

type Expr =

        // Primitives
        | Literal of Value
        | Ident  of identifier (* ident *)

        | Args of Expr list
        | Arithmetic of Expr * Op * Expr
        | Comparison of Expr * Op * Expr
        | Logical    of Expr * Op * Expr
        | UnaryOp    of Op * Expr
        
        // Logical
        | Not        of Expr
        | BinLogical of Expr * Op * Expr
        | And        of Expr
        | Or        of Expr


module AssertionParser = 
    let ws = spaces
    let str = pstring
    let all, allRef = createParserForwardedToRef()
    
    let specialChars = ['+'; '-'; '*'; '(';')';'>';'<';'='; '&';'|';' ']
    
    let number c = isDigit c 
    let pConst: Parser<_, unit> = pint32 .>> ws |>> (fun x -> Literal(IntC(x)))
    let pVarName:Parser<_, unit> = (manyChars (noneOf specialChars)) |>> Ident
    let xbool =     (stringCIReturn "true" (Bool true)   |>> Literal)
                    <|> (stringCIReturn "false" (Bool false) |>> Literal)


    let xprim = pConst <|> xbool <|> pVarName  

    /// Configure the operator precedence parser to handle complex expressions
    let oppa = new OperatorPrecedenceParser<Expr, unit, unit>()

    let parithmetic = oppa.ExpressionParser

    oppa.TermParser <- (xprim .>> ws) 
    //<|> logicals .>> ws <|> between (str "(" .>> ws) (str ")" .>> ws) parithmetic

    type Assoc = Associativity
    
    /// Binary Operators
    oppa.AddOperator(InfixOperator("+", ws, 1, Assoc.Left,  fun x y -> Arithmetic(x, Plus, y)))
    oppa.AddOperator(InfixOperator("-", ws, 1, Assoc.Left,  fun x y -> Arithmetic(x, Minus, y)))    
    oppa.AddOperator(InfixOperator("*", ws, 2, Assoc.Left,  fun x y -> Arithmetic(x, Multiply, y)))


    /// Unary Operators
    oppa.AddOperator(PrefixOperator("-", ws, 6, true, fun x -> UnaryOp(Minus, x)))
    // Essentially a no-op
    oppa.AddOperator(PrefixOperator("+", ws, 6, true, fun x -> x))

    /// Logical Binary Operators
    let oppc = new OperatorPrecedenceParser<Expr, unit, unit>()
    let pcomparison = oppc.ExpressionParser
    let termc = (parithmetic .>> ws) //<|> between (str "(" .>> ws) (str ")" .>> ws) parithmetic
    oppc.TermParser <- termc

    oppc.AddOperator(InfixOperator("=",  ws, 1, Assoc.Left, fun x y -> Comparison(x, Eq,    y)))
    oppc.AddOperator(InfixOperator("<>", ws, 1, Assoc.Left, fun x y -> Comparison(x, NotEq, y)))
    oppc.AddOperator(InfixOperator("!=", ws, 1, Assoc.Left, fun x y -> Comparison(x, NotEq, y)))
    oppc.AddOperator(InfixOperator(">",  ws, 1, Assoc.Left, fun x y -> Comparison(x, GT,    y)))
    oppc.AddOperator(InfixOperator("<",  ws, 1, Assoc.Left, fun x y -> Comparison(x, LT,    y)))
    oppc.AddOperator(InfixOperator(">=", ws, 1, Assoc.Left, fun x y -> Comparison(x, GTEq,  y)))
    oppc.AddOperator(InfixOperator("<=", ws, 1, Assoc.Left, fun x y -> Comparison(x, LTEq,  y)))


    let oppb = new OperatorPrecedenceParser<Expr, unit, unit>()
    let bcomparison = oppb.ExpressionParser
    let termb = between (str "(" .>> ws) (str ")" .>> ws) bcomparison <|>  (pcomparison .>> ws) 
    oppb.TermParser <- termb

    oppb.AddOperator(InfixOperator("&&",  ws, 1, Assoc.Left, fun x y -> BinLogical(x, AndOp, y)))
    oppb.AddOperator(InfixOperator("||", ws, 1, Assoc.Left, fun x y -> BinLogical(x, OrOp , y)))


    // do exprRef := opp.ExpressionParser
    allRef := oppb.ExpressionParser


    // The full parser, terminating with an EOF marker
    let xparser = ws >>. all .>> ws .>> eof
    
    let parse expr = 
        match (run xparser expr) with 
            | Failure (error,_,_) -> 
                printfn "%s" error
                None
            | Success (res,_,_) -> Some res

    (*let testExprAsStr() = 
        run xparser "x+1 < y+2 || x+1" |> ignore
        run xparser "x+1 <5 && x<5   && x=y" |> ignore
        run xparser "(x+1 <5 && x<5   && x=y)" |> ignore
        run xparser " x<3 && y<6 " |> ignore
        run xparser " x+1 < y*3 "|> ignore
        run xparser "x+1 > 4" |> ignore
        run xparser "x=y" |> ignore
        run xparser "x<y && x>10" |> ignore
        run xparser "x<y" |> ignore*)

/// Parser for inferred variables as given in the labe field of the parsed CFSM. 
// The format is: {variableName:Expression}, where Expression is arithmetic expression 
module InferredVarsParser = 
    module InternalInferredVars = 
        let varName = manyChars (noneOf [',';')'; ':']) 
        let myExpr:Parser<_, unit> = 
            let normalChar 
                = satisfy (fun c -> c <> ';' && c<>'\\' && c<>'\"' && c<>'}'&& c<>',')
            (manyChars normalChar) 

        let singleRecord = pipe3 (spaces >>.varName) (pstring ":") (spaces >>. myExpr) 
                                     (fun name _ varExpr -> (sprintf "%s" name, sprintf "%s" varExpr))
    
        let all = spaces  >>. (sepBy singleRecord (pstring ","))  .>> spaces .>> eof |>> dict
    
    let parseInfVars infMap = 
        match (run InternalInferredVars.all infMap) with 
            | Failure (error,_,_) -> 
                printfn "%s" error
                None
            | Success (res,_,_) -> 
                if res.Count> 0 then Some res
                else None
