#load "../../data/Day16_data.fsx"

open System
open System.Text.RegularExpressions

let myData = Data.getData

type Ticket = int list

let parseTicket (text : string) =
    text.Split([|','|]) |> Seq.map int |> Seq.toList

type Rule = { field : string; ranges : (int*int) list }

let parseRule (text : string) =
    let ruleRegex = Regex("(.*): (\d*)-(\d*) or (\d*)-(\d*)")
    let m = ruleRegex.Match(text)
    let field = m.Groups.[1].Value
    let fl = m.Groups.[2].Value |> int
    let fu = m.Groups.[3].Value |> int
    let sl = m.Groups.[4].Value |> int
    let su = m.Groups.[5].Value |> int
    { field = field; ranges = [(fl,fu); (sl,su) ] }

type ParseResult = { rules : Rule list; ourTicket : Ticket; nearbyTickets : Ticket list }
let parse input =
    let noEmptyLines = input |> Seq.filter (fun l -> l <> "")
    let rules = noEmptyLines |> Seq.takeWhile (fun s -> s <> "your ticket:") |> Seq.map parseRule |> Seq.toList
    let ourTicket = noEmptyLines |> Seq.skip ((rules |> Seq.length) + 1) |> Seq.head |> parseTicket
    let nearbyTickets = noEmptyLines |> Seq.skipWhile ( (<>) "nearby tickets:" ) |> Seq.skip 1 |> Seq.map parseTicket |> Seq.toList
    { rules = rules; ourTicket = ourTicket; nearbyTickets = nearbyTickets }

let applies (rule : Rule) number =
    rule.ranges |> Seq.exists (fun (min,max) -> min <= number && number <= max)

let isValid rules number = 
    rules
    |> Seq.exists (fun r -> applies r number)

let parsed = parse myData

parsed.nearbyTickets 
|> Seq.collect id 
|> Seq.filter (fun field -> field |> (isValid parsed.rules) |> not) 
|> Seq.sum