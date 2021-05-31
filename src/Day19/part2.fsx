open System.IO
open System.Text.RegularExpressions

let dayDirectory = __SOURCE_DIRECTORY__
let srcDirectory = Directory.GetParent(dayDirectory) |> string
let rootDirectory = Directory.GetParent(srcDirectory) |> string
let dataFile = rootDirectory + "\\Data\\Day19_data.txt"
let myData = File.ReadAllLines dataFile

let createPattern rule (rules: Map<int, string>) =
    let rec build r n =
        match n, rules.[r] with
        | n, _ when n > 25 -> "" // No rule goes this deep
        | _, "\"a\"" -> "a"
        | _, "\"b\"" -> "b"
        | n, composite -> 
            composite.Split(" ") 
            |> Array.map (fun s -> if s = "|" then "|" else build (int s) (n + 1))
            |> Array.fold (+) ""
            |> sprintf "(%s)"

    sprintf "^%s$" (build rule 0)

let partOne (rules: Map<int, string>) messages =
    let pattern = createPattern 0 rules
    
    messages
    |> List.filter (fun m -> Regex.IsMatch(m, pattern))
    |> List.length

let input = 
    myData
    |> Seq.toList

let rules = 
    input 
    |> List.takeWhile (fun s -> s <> "")
    |> List.map (fun s -> s.Split(": "))
    |> List.map (fun s -> int s.[0], s.[1])
    |> Map.ofList

let messages = 
    input 
    |> List.skip (Map.count rules + 1)
    
let partTwo (rules: Map<int, string>) messages =
    let newRules = 
        rules
        |> Map.add 8 "42 | 42 8"
        |> Map.add 11 "42 31 | 42 11 31"

    partOne newRules messages

partTwo rules messages