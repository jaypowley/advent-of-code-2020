#load "../../data/Day10_data.fsx"

open System

let myData = Data.getData

let parse : string seq -> int seq = Seq.map int

let parsed = parse myData

let sorted = parsed |> Seq.sort |> Seq.toList
let last = sorted |> Seq.last
let totalChain = (0 :: sorted) @ [last + 3]

let deltas = totalChain |> List.pairwise |> List.map (fun (a,b) -> b-a) |> List.groupBy id
let ones = (snd deltas.[0]) |> Seq.length
let threes = (snd deltas.[1]) |> Seq.length
ones * threes