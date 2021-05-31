#load "../../data/Day6_data.fsx"

open System

let myData = List.ofArray Data.getData

let getDataFromList data = 
    List.fold (fun grouped line ->
        match line with
        | "" -> [] :: grouped
        | line -> (line :: grouped.Head) :: grouped.Tail                
    ) [[]] data

let dataSets = getDataFromList myData |> Array.ofList

dataSets
|> Seq.sumBy (List.map Set
              >> Seq.reduce Set.intersect
              >> Seq.distinct
              >> Seq.length)
|> printfn "%d"
