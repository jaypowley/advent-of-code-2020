#load "../../data/Day14_data.fsx"

open System
open System.Text.RegularExpressions

let myData = Data.getData

type Instruction = Mem of int64 * int64 | Mask of string

let parse text =
    let parseLine (line : string) =
        let maskLine = line.Split([|"mask = "|], System.StringSplitOptions.None)
        if maskLine.Length = 2
        then Mask maskLine.[1]
        else 
            let memRegex = Regex("mem\[(\d*)\] = (\d*)").Match(line)
            let address = memRegex.Groups.[1].Value |> int64
            let value = memRegex.Groups.[2].Value |> int64
            Mem (address, value)
    text
    |> Seq.map parseLine
    |> Seq.toList

type Computer = { currentMask : string; memory : Map<int64,int64> }
let init = { currentMask = "NO MASK"; memory = Map.empty }
let instructions = parse myData

let toBits (value : int64) =
    Convert.ToString(value,2).PadLeft(36,'0')

let fromBits (bits : char seq) =
    let s = bits |> Seq.map string |> String.concat ""
    Convert.ToInt64(s, 2)

let applyMask (mask : string) (value : int64) =
    let valuebits = toBits value
    
    let masked =
        Seq.zip mask valuebits
        |> Seq.map (fun (mb, vb) ->
            match mb with
            | 'X' -> vb
            | b   -> b)
    
    fromBits masked

let store computer address value =
    { computer with memory = computer.memory |> Map.add address value }

let runInstruction computer instruction =
    match instruction with
    | Mask m -> { computer with currentMask = m }
    | Mem (address, value) -> 
        let masked = applyMask computer.currentMask value
        store computer address masked

let final = instructions |> Seq.fold runInstruction init
final.memory |> Map.toList |> List.map snd |> List.sum