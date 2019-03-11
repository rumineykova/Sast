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

    let rec evalArithExpr node = 
        match node with
        | Literal(IntC(value)) -> value
        | Ident(identifier) -> failwith "Free variable %s" identifier
        | Arithmetic(left, op, right) -> 
            let leftVal = (evalArithExpr left)
            let rightVal = (evalArithExpr right)
            match op with 
            | Plus -> leftVal+rightVal
            | Minus -> leftVal-rightVal
            | Multiply -> leftVal*rightVal
            | Subtract -> leftVal-rightVal
        | _ -> failwith "Expression is not in the correct format"

    let rec evalExpr node = 
        match node with
        | Literal(Bool(value)) -> value
        | Ident(identifier) -> failwith "Free variable %s" identifier
        | Comparison(left, op, right) -> 
            let leftVal = (evalArithExpr left)
            let rightVal = (evalArithExpr right)
            match op with 
            | LT -> leftVal<rightVal
            | GT -> leftVal>rightVal
            | Eq -> leftVal=rightVal
            | NotEq -> leftVal<>rightVal
            | LTEq -> leftVal<=rightVal
            | GTEq -> leftVal>=rightVal  
            | _ -> failwith "Error"
        | Logical(left, op, right) -> 
            let leftVal = (evalExpr left)
            let rightVal = (evalExpr right)
            match op with  
            | AndOp -> leftVal && rightVal
            | OrOp ->  leftVal && rightVal
            | _ -> failwith "Error"

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

    let rec subsExpr node (subsMap: Map<string, int>)= 
        match node with
        | Literal(Bool(value)) -> node
        | Literal(IntC(value)) -> node
        | Ident(identifier) -> 
            if (subsMap.ContainsKey(identifier)) 
                then Literal(IntC(subsMap.[identifier]))
            else node
        | UnaryOp(_, right) -> subsExpr right subsMap
        | Not(expr) -> subsExpr expr subsMap
        | Arithmetic(left, op, right) -> 
            let leftSub = subsExpr left subsMap
            let rightSub = subsExpr right subsMap
            let res = Arithmetic(leftSub, op, rightSub)
            res 
        | Comparison(left, op, right) -> 
            let leftSub = subsExpr left subsMap
            let rightSub = subsExpr right subsMap
            let res = Comparison(leftSub, op, rightSub)
            res 
        | BinLogical(left, op, right) -> 
            let leftSub = subsExpr left subsMap
            let rightSub = subsExpr right subsMap
            let res = BinLogical(leftSub, op, rightSub)
            res 
        | Logical(left, op, right) -> 
            let leftSub = subsExpr left subsMap
            let rightSub = subsExpr right subsMap
            let res = Logical(leftSub, op, rightSub)
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