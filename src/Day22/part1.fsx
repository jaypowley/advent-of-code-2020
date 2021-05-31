open System.IO

let dayDirectory = __SOURCE_DIRECTORY__
let srcDirectory = Directory.GetParent(dayDirectory) |> string
let rootDirectory = Directory.GetParent(srcDirectory) |> string
let dataFile = rootDirectory + "\\Data\\Day22_data.txt"
let myData = File.ReadAllLines dataFile

type Card = int
type Deck = { player : int; cards : int list }
type GameState = Deck * Deck

let parse (text : string seq) : GameState = 
    let p1 = text |> Seq.takeWhile ((<>) "") |> Seq.skip 1 |> Seq.map int |> Seq.toList
    let p2 = text |> Seq.skipWhile ((<>) "") |> Seq.skip 2 |> Seq.map int |> Seq.toList
    ({ player= 1; cards = p1 }, { player= 2; cards = p2 })

let playRound ((p1, p2) as state) : GameState =
    let p1Card :: p1Deck = p1.cards
    let p2Card :: p2Deck = p2.cards

    if p1Card > p2Card
    then { p1 with cards = p1Deck @ [p1Card;p2Card] }, { p2 with cards = p2Deck }
    else { p1 with cards = p1Deck }, { p2 with cards = p2Deck @ [p2Card;p1Card] }
    

let rec playGame ((p1, p2) as state) : GameState =
    match p1.cards, p2.cards with
    | [], _ -> state
    | _, [] -> state
    | _ -> state |> playRound |> playGame

let parsed = parse myData
let ending = playGame parsed

fst ending 
    |> (fun d -> d.cards) 
    |> Seq.rev 
    |> Seq.mapi (fun idx card -> (idx + 1) * card) 
    |> Seq.reduce (+)