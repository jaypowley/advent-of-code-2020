#r "nuget: FSharpx.Collections"
open FSharpx.Collections

type Cups = int RandomAccessList
type State = { cups : Cups; currentCup : int }

let input = "871369452"

let parse input : State =
    let parsed = (input |> Seq.map (string>>int) |> Seq.toList) @ [10..1_000_000]    
    let last,first = parsed |> List.last, parsed |> List.head
    let cups =        
        (0,0) :: (last,first) :: (parsed |> List.pairwise)
        |> List.sortBy fst
        |> List.map snd
        |> RandomAccessList.ofSeq
    { cups = cups; currentCup = first }

let wrap max n =
    if n = 0 
    then max
    else 1 + (n - 1) % max

let rec findDestination maximum current selected =
    if selected |> List.contains current
    then findDestination maximum ((current - 1) |> wrap maximum) selected
    else current

let cupAfter cup (cups : Cups) =
    cups |> RandomAccessList.nth cup

let withCupAfter cup nextcup cups =
    cups |> RandomAccessList.update cup nextcup

let move n (state : State) =
    let maximum = state.cups |> Seq.max
    let length = state.cups |> Seq.length
    let rec move n (state : State) =
        if n % 1_000_000 = 0 then printfn "moves: %d" n
        if n = 0 
        then state
        else
            let one = state.cups |> cupAfter state.currentCup
            let two = state.cups |> cupAfter one
            let three = state.cups |> cupAfter two
            let four = state.cups |> cupAfter three
            let destination = 
                findDestination 
                    maximum 
                    ((state.currentCup - 1) |> wrap maximum) 
                    [one;two;three]
            let afterDestination = state.cups |> cupAfter destination
            let nextCups =
                state.cups
                |> withCupAfter state.currentCup four
                |> withCupAfter three afterDestination
                |> withCupAfter destination one

            move (n-1) { currentCup = four; cups = nextCups }
    move n state

let final = input |> parse |> move 10_000_000
let second = final.cups |> cupAfter 1
let third = final.cups |> cupAfter second

(int64 second) * (int64 third)