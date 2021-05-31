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

let sumBagsDeep bagColor rules =
    let map =
        rules
        |> Array.Parallel.map (fun r -> (r.BagColor, r.Holds |> Map.toSeq))
        |> Map.ofArray
    let rec flattenBagSums col acc =
        seq {
            match Map.tryFind col map with
            | None -> ()
            | Some holdings ->
                yield! (holdings |> Seq.map (snd >> (*) acc))
                for (innerCol, count) in holdings do
                    yield! flattenBagSums innerCol (count * acc)
        }
    flattenBagSums bagColor 1
    |> Seq.sum

let rules = Array.Parallel.map parseLuggageRule myData
let soughtColor = "shiny gold"
printfn "Number of bags needed for '%s': %i" soughtColor (sumBagsDeep soughtColor rules)