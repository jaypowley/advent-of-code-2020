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

let mutable gameNb = 0L

let rec playRound (p1, p2) : GameState =
    let p1Card :: p1Deck = p1.cards
    let p2Card :: p2Deck = p2.cards

    if (p1Deck |> Seq.length) >= p1Card && (p2Deck |> Seq.length) >= p2Card
    then
        gameNb <- gameNb+1L
        if gameNb % 1_000L = 0L then printfn "starting sub-game %d" gameNb
        let subDeck1 = p1Deck |> List.take p1Card
        let subDeck2 = p2Deck |> List.take p2Card
        let winner = playGame Set.empty ({ p1 with cards = subDeck1}, { p2 with cards = subDeck2})
        match winner.player with
        | 1 -> { p1 with cards = p1Deck @ [p1Card;p2Card] }, { p2 with cards = p2Deck }
        | _ -> { p1 with cards = p1Deck }, { p2 with cards = p2Deck @ [p2Card;p1Card] }
    else 
        if p1Card > p2Card
        then { p1 with cards = p1Deck @ [p1Card;p2Card] }, { p2 with cards = p2Deck }
        else { p1 with cards = p1Deck }, { p2 with cards = p2Deck @ [p2Card;p1Card] }
and playGame history ((p1, p2) as state) : Deck =
    match p1.cards, p2.cards with
    | [], _ -> p2
    | _, [] -> p1
    | _ -> 
        let next = state |> playRound 
        if history |> Set.contains next
        then p1
        else playGame (history |> Set.add next) next

let score deck =
    deck 
    |> (fun d -> d.cards) 
    |> Seq.rev 
    |> Seq.mapi (fun idx card -> (idx + 1) * card) 
    |> Seq.reduce (+)

let parsed = parse myData

let winner = playGame Set.empty parsed 

score winner