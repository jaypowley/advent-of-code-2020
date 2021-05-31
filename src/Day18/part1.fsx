#load "../../data/Day18_data.fsx"

open System
open System.Text.RegularExpressions

let myData = Data.getData

type Token =
    | Add
    | Mul
    | Num of int64

let parseToken = function
    | "+" -> Add
    | "*" -> Mul
    | x   -> Num (int64 x)

let rec evalTokens = function
    | [Num a] -> string a
    | Num a::Add::Num b::tl -> Num (a+b)::tl |> evalTokens
    | Num a::Mul::Num b::tl -> Num (a*b)::tl |> evalTokens
    | x -> failwithf "unexpected: %A" x

let simpleEval (expression:string) =
    [   for m in Regex.Matches(expression, @"(\d+)|\+|\*") do
        yield parseToken m.Groups.[0].Value ]
    |> evalTokens

let regexReplaceF (pattern:string) fReplace (input:string) =
    Regex.Replace(input, pattern, fun m -> fReplace (m.Groups.[1].Value))

let rec eval (input:string) =
    if input.Contains "("
    then input |> regexReplaceF @"\(([^()]+)\)" simpleEval |> eval
    else simpleEval input

let solve (input) =
    input
    |> Array.toList
    |> List.sumBy (eval >> int64)

solve myData