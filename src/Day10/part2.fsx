#load "../../data/Day10_data.fsx"

open System

let myData = Data.getData

let parse : string seq -> int seq = Seq.map int

let parsed = parse myData

let candidates joltage adapters =
    let rec candidates' acc joltage adapters =
        match adapters with
        | [] -> acc
        | a :: aas when a - joltage <= 3 ->  candidates' ((a, aas) ::acc) joltage aas
        | _ -> acc
    candidates' [] joltage adapters |> List.rev

let rec calculateNbPaths memo joltage adapters = 
    match memo |> Map.tryFind joltage with
    | Some v -> memo
    | None ->
        match adapters with
        | [_] -> 
            memo |> Map.add joltage 1L
        | adapters ->
            let cands = candidates joltage adapters 
            let nextMemo = cands |> Seq.fold (fun memo (a, aas) -> calculateNbPaths memo a aas) memo
            let sum = cands |> Seq.map fst |> Seq.map (fun c -> nextMemo |> Map.find c) |> Seq.sum
            nextMemo |> Map.add joltage sum

let solve adapters =
    let sorted = adapters |> List.sort
    let deviceAdapter = (sorted |> Seq.last) + 3
    let allAdapters = sorted @ [deviceAdapter]
    let memo = Map.empty
    
    calculateNbPaths memo 0 allAdapters |> Map.find 0

solve (List.ofSeq parsed)