#load "../../data/Day5_data.fsx"

open System
open System.Text.RegularExpressions

let myData = Seq.ofArray Data.getData

let seatIds =
    Seq.map ((fun line -> Regex.Replace(line, "B|R", "1")) 
    >> ((fun line -> Regex.Replace(line, "F|L", "0")) 
    >> (fun line -> Convert.ToInt32(line, 2)))) myData

printfn "%d" (seatIds |> Seq.max)