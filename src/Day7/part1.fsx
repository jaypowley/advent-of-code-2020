#load "../../data/Day7_data.fsx"

open System
open System.Text.RegularExpressions

let myData = Data.getData

type LuggageRule = {
    BagColor: string
    Holds: Map<string, int>
}

let luggageRuleRegex = Regex(@"(^|\d+ )(.*?) bags?", RegexOptions.Compiled)
let parseLuggageRule line =
    let matches = luggageRuleRegex.Matches line
    {
        BagColor = (Seq.item 0 matches).Groups.[2].Value
        Holds = matches
            |> Seq.skip 1
            |> Seq.map (fun m -> m.Groups.[2].Value, int m.Groups.[1].Value)
            |> Map.ofSeq
    }

let addToMapOfSets key item map =
    match Map.tryFind key map with
    | Some items -> map |> Map.add key (Set.add item items)
    | None -> map |> Map.add key (Set [item])

let calcParentalMappings rules =
    rules
    |> Array.fold (fun map rule ->
        Map.toSeq rule.Holds
        |> Seq.map fst
        |> Seq.fold (fun map bag -> map |> addToMapOfSets bag rule.BagColor) map
    ) Map.empty

let findBagsThatEventuallyContains soughtColor parentalMap =
    let rec find map col =
        match Map.tryFind col map with
        | None -> Set [col]
        | Some bags ->
            bags
            |> Seq.map (find map)
            |> Seq.reduce Set.union
            |> Set.add col

    find parentalMap soughtColor
    |> Set.remove soughtColor

let rules = Array.Parallel.map parseLuggageRule myData
printfn "Parsed rules for %i different bag colors" (rules |> Array.distinctBy (fun r -> r.BagColor) |> Array.length)

let parentalMap = calcParentalMappings rules

let soughtColor = "shiny gold"
let bags = findBagsThatEventuallyContains soughtColor parentalMap
printfn "Number of bag colors that eventually contain '%s': %i" soughtColor bags.Count