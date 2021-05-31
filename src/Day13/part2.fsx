#load "../../data/Day13_data.fsx"

open System

let myData = Data.getData

let remainder idx busid =  (busid - (int64 idx % busid)) % busid

let parse (text : string) =
    let busids = 
        text 
        |> fun s -> s.Split([|','|]) 
        |> Seq.mapi (fun idx bus -> if bus = "x" then None else Some (idx, int64 bus))
        |> Seq.choose id
        |> Seq.map (fun (idx, busid) -> (remainder idx busid, busid))
    busids

let inverse Ni divisor = 
    [1L..divisor]
    |> Seq.find (fun d -> (Ni * d) % divisor = 1L)

let solve input =
    let buses = parse input
    let N = buses |> Seq.map snd |> Seq.reduce (*)
    buses
        |> Seq.map (fun (bi, divi) ->
            let Ni = N / divi
            let xi = inverse Ni divi
            let biNixi = bi * Ni * xi
            biNixi)
        |> Seq.sum
        |> (fun sum -> sum % N)

solve (myData |> Seq.item 1)