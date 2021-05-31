#load "../../data/Day14_data.fsx"

open System
open System.Text.RegularExpressions

let myData = Data.getData

type Instruction = Mem of int64 * int64 | Mask of string

let maskRegex = Regex("mask = (.*)")
let memRegex = Regex("mem\[(\d*)\] = (\d*)")
let (|AMask|_|) line =
    let m = maskRegex.Match(line)
    if m.Success 
    then m.Groups.[1].Value |> Some
    else None
let (|AMem|_|) line =
    let m = memRegex.Match(line)
    if m.Success 
    then (int64 m.Groups.[1].Value, int64 m.Groups.[2].Value) |> Some
    else None


let parse text =
    let parseLine (line : string) =
        match line with
        | AMask m -> Mask m
        | AMem (a,v) -> Mem (a,v)
        | u -> failwith $"Don't know how to parse line: {u}"
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
    System.Convert.ToInt64(s, 2)

let rec applyFloats bitstring = 
    match bitstring with
    | [] -> [[]]
    | b :: bs -> 
        match b with
        | '1' 
        | '0' -> 
            let rest = applyFloats bs
            rest |> List.map (fun a -> b :: a)
        | 'X' ->
            let rest = applyFloats bs
            let ones = rest |> List.map (fun a -> '1' :: a)
            let zeroes = rest |> List.map (fun a -> '0' :: a)
            List.append ones zeroes
        | unknown -> failwith $"Unknown bit: {unknown}"

let applyMask (mask : string) (value : int64) =
    let valuebits = toBits value
    
    let masked =
        Seq.zip mask valuebits
        |> Seq.map (fun (mb, vb) ->
            match mb with
            | '0' -> vb
            | '1' -> '1'
            | 'X'   -> 'X')
    
    applyFloats (masked |> List.ofSeq) |> List.map fromBits

let store computer address value =
    { computer with memory = computer.memory |> Map.add address value }

let runInstruction computer instruction =
    match instruction with
    | Mask m -> { computer with currentMask = m }
    | Mem (address, value) -> 
        let destinations = applyMask computer.currentMask address
        destinations |> Seq.fold (fun computer address -> store computer address value) computer
        
let final = instructions |> Seq.fold runInstruction init
final.memory |> Map.toList |> List.map snd |> List.sum