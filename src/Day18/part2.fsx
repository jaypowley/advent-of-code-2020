#load "../../data/Day18_data.fsx"
#r "nuget: FParsec"

open FParsec
open System

let myData = Data.getData

type Operator = Add | Mul
type OperationData = { operator : Operator; left : Expression; right : Expression }
and Expression = 
    | Value of int64
    | Operation of OperationData
    | Parentheses of Expression

let poperator = 
    ((charReturn '*' Mul) <|> (charReturn '+' Add)) .>> spaces
let pvalue = pint64 .>> spaces |>> Value

let pexpression, pexpressionRef = createParserForwardedToRef<Expression, unit>()
let pexpression' = 
    (pipe2 poperator pexpression (fun op exp -> (op,exp))) |>> Some
    <|> preturn None
let constructExpression exp exp' = 
    match exp' with 
    | None -> exp
    | Some (op, right) -> Operation { left = exp; operator = op; right = right}
let pbetweenparens = ((between (pchar '(') (pchar ')'.>> spaces) pexpression) |>> Parentheses)
pexpressionRef :=
    pipe2 pvalue pexpression' constructExpression
    <|> pipe2 pbetweenparens pexpression' constructExpression
    <|> pbetweenparens

let parse (text : string) =
    match run (pexpression .>> eof) text with
    | Success (r,_,_) -> r
    | Failure (f,s,e) -> failwithf $"Failure: {(f,s,e)}"

let operate l op r =
    match op with
    | Add -> l + r
    | Mul -> l * r

let rec eval (exp : Expression) : int64 = 
    match exp with
    | Value v -> v
    | Parentheses e -> eval e
    | Operation { left = l; operator = op; right = r } ->
        match r with
        | Value rv -> operate (eval l) op rv
        | Parentheses r ->
            let rv = eval r
            operate (eval l) op rv        
        | Operation { left = rl; operator = rop; right = rr } as next ->
            let lv = eval l
            match op with
            | Add ->
                let operated = operate lv op (eval rl)
                eval <| Operation { left = Value operated; operator = rop; right = rr }
            | Mul ->
                let rightEvaluated = eval next
                operate lv op rightEvaluated

let parsed = myData |> Seq.map parse
parsed |> Seq.sumBy eval