namespace AssertionParsing
open AssertionParsing.AssertionParser

module Visitors =
    let rec getStringRepr node = 
        match node with
        | Literal(Bool(value)) -> 
            value.ToString()
        | Literal(IntC(value)) -> 
            sprintf "%i" value
        | Ident(identifier) -> 
            identifier
        | UnaryOp(op, right) -> 
            sprintf "%s%s" (op.ToString()) (getStringRepr right)
        | Not(expr) -> 
            sprintf "not %s" (getStringRepr expr)
        | Arithmetic(left, op, right) 
        | Comparison(left, op, right) 
        | BinLogical(left, op, right)
        | Logical(left, op, right) -> 
            let leftStr = getStringRepr left
            let rightStr = getStringRepr right
            sprintf "%s %s %s" leftStr (op.ToString()) rightStr


    let rec getVars node = 
        match node with
        | Literal(Bool(value)) -> 
            set []
        | Literal(IntC(value)) -> 
            set []
        | Ident(identifier) -> 
            set [identifier]
        | UnaryOp(op, right) -> 
            getVars right
        | Not(expr) -> 
            getVars expr
        | Arithmetic(left, op, right) 
        | Comparison(left, op, right) 
        | BinLogical(left, op, right)
        | Logical(left, op, right) -> 
            let leftSet = getVars left
            let rightSet = getVars right
            let res = leftSet |> Set.union rightSet
            res 