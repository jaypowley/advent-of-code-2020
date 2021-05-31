#load "../../data/Day13_data.fsx"

open System

let myData = Data.getData

let parse (text : string seq) =
    let timestamp = text |> Seq.head |> int64
    let busids = 
        text 
        |> Seq.item 1 
        |> fun s -> s.Split([|','|]) 
        |> Seq.filter (fun c -> c <> "x") 
        |> Seq.map int64 
        |> Seq.toList
    (timestamp, busids )

let arrivalsFor busID =
    Seq.initInfinite (fun idx -> (idx |> int64) * busID) |> Seq.skip 1
    
let (timestamp, busids) = parse myData
let arrivalTimes = busids |> Seq.map arrivalsFor
let earliestByBusID = arrivalTimes |> Seq.map (Seq.find (fun arrival -> arrival >= timestamp))
let earliestBusIndex = earliestByBusID |> Seq.indexed |> Seq.minBy snd

let busID = busids |> Seq.item (fst earliestBusIndex)
let waitTime = snd earliestBusIndex - timestamp

waitTime * busID