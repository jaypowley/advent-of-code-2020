open System.IO

let dayDirectory = __SOURCE_DIRECTORY__
let srcDirectory = Directory.GetParent(dayDirectory) |> string
let rootDirectory = Directory.GetParent(srcDirectory) |> string
let dataFile = rootDirectory + "\\Data\\Day24_data.txt"
let myData = File.ReadAllLines dataFile

type Direction = NE | E | SE | SW | W | NW
type Color = White | Black

let parsePath (path : string) =
    let rec parsePath (path : char list) =
        match path with
        | [] -> []
        | 'n' :: 'e' :: ps -> NE :: parsePath ps
        | 's' :: 'e' :: ps -> SE :: parsePath ps
        | 'n' :: 'w' :: ps -> NW :: parsePath ps
        | 's' :: 'w' :: ps -> SW :: parsePath ps
        | 'e' :: ps -> E :: parsePath ps
        | 'w' :: ps -> W :: parsePath ps
    parsePath (path |> Seq.toList)

let move (x,y,z) direction =
    let (dx,dy,dz) =
        match direction with
        | E -> (1,1,0)
        | SE -> (1,0,-1)
        | SW -> (0,-1,-1)
        | W -> (-1,-1,0)
        | NW -> (-1,0,1)
        | NE -> (0,1,1)
    (x+dx,y+dy,z+dz)

let rec follow start path =
    match path with
    | [] -> start
    | d :: ds -> follow (move start d) ds

type State = Map<(int * int * int), Color>

let flip location state =
    match state |> Map.tryFind location with
    | None
    | Some White -> 
        state |> Map.add location Black
    | Some Black ->
        state |> Map.add location White

let flipTile state path =
    let location = follow (0,0,0) path
    state |> flip location
    
let paths = myData |> Seq.map parsePath |> Seq.toList
let flipped =
    paths
    |> List.fold flipTile Map.empty

flipped 
    |> Map.toSeq 
    |> Seq.map snd 
    |> Seq.filter (function | Black -> true | _ -> false) 
    |> Seq.length