namespace AssertionParsing

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

    let isEquality node = 
        match node with 
        | Comparison(left, op, right) -> 
            match op with 
            | Eq -> true
            | _ -> false
        | _ -> false 

    let rec getVars node = 
        match node with
        | Literal(Bool(_)) -> 
            set []
        | Literal(IntC(_)) -> 
            set []
        | Ident(identifier) -> 
            set [identifier]
        | UnaryOp(_, right) -> 
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

        (*let myDict = dict["x", 2; "y", 3]
    let rec getQuotationsExpr node = 
        match node with
        | Literal(Bool(value)) -> 
            Quotations.Expr.Value(value)
        | Literal(IntC(value)) -> 
            Quotations.Expr.Value(value)
        | Ident(identifier) -> 
            //Quotations.Expr.Var(new Quotations.Var(identifier, typeof<int>))
            //let getVal = myDict.Item identifier 
            <@@ myDict.Item identifier @@>
            //Quotations.Expr.Value(getVal)

        | Arithmetic(left, op, right) -> 
            let leftExpr = getQuotationsExpr left
            let rightExpr = getQuotationsExpr right
            let res = Quotations.Expr.Applications(op.ToExpr(), [[leftExpr]; [rightExpr]]) 
            res *)