#load "../data/Day5_data.fsx"

open System
open System.Text.RegularExpressions

let myData = Seq.ofArray Data.getData

let seatIds =
    Seq.map ((fun line -> Regex.Replace(line, "B|R", "1")) 
    >> ((fun line -> Regex.Replace(line, "F|L", "0")) 
    >> (fun line -> Convert.ToInt32(line, 2)))) myData

let firstMissing xs =
    let rec loop (x :: y :: tl) =
        if x + 1 = y
        then loop (y :: tl)
        else x + 1
    loop xs

seatIds
|> Seq.sort
|> Seq.toList
|> firstMissing
|> printfn "%d"