open System.IO
open System.Text.RegularExpressions

let dayDirectory = __SOURCE_DIRECTORY__
let srcDirectory = Directory.GetParent(dayDirectory) |> string
let rootDirectory = Directory.GetParent(srcDirectory) |> string
let dataFile = rootDirectory + "\\Data\\Day20_data.txt"
let myData = File.ReadAllText dataFile

type Tile = { ID : int; contents : char[,] }
type TileConfigurations = { ID : int; configurations : char[,] list }

let m = array2D [[1;2];[3;4]]

let rot90 m =
    [for col in (Array2D.length1 m) - 1 .. -1 .. 0 -> 
        m.[0..,col]
    ]
    |> array2D

let flipHorizontal m =
    [for row in (Array2D.length1 m) - 1 .. -1 .. 0 -> 
        m.[row,0..]
    ]
    |> array2D  

let flipVertical m =
    [for row in 0 .. (Array2D.length1 m) - 1-> 
        m.[row,0..] |> Array.rev
    ]
    |> array2D

let parse (input : string) = 
    let tileIDRegex = Regex("Tile (\d*):")
    let parseTile (text :string) =
        let lines = text.Split([|"\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
        let id = (tileIDRegex.Match(lines.[0])).Groups.[1].Value |> int
        let contents = array2D lines.[1..]
        { ID = id; contents = contents }

    input.Split([|"\r\n\r\n"|], System.StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map parseTile
    |> Seq.toList

let borders (tile : 'a [,]) =
    [ 
        tile.[0,0..]
        tile.[(Array2D.length1 tile - 1),0..]
        tile.[0..,0]
        tile.[0..,(Array2D.length1 tile - 1)]
    ]

let configurations tile = 
    [tile
     tile |> rot90
     tile |> rot90 |> rot90
     tile |> rot90 |> rot90 |> rot90]
    |> List.collect (fun t -> [t; flipHorizontal t])

let nbUnmatchedBorders tile (configs : TileConfigurations list) = 
    let bords = borders tile.contents
    let nbMatchesPerBorder =
        bords
        |> Seq.map (fun border -> 
            configs 
            |> Seq.filter (fun config -> 
                config.configurations 
                |> List.exists (fun c -> c |> borders |> List.contains border)) 
            |> Seq.length)
    nbMatchesPerBorder |> Seq.filter ((=) 0) |> Seq.length

let solve input =
    let tiles = parse input
    let configs = tiles |> List.map (fun t -> { ID = t.ID; configurations = configurations t.contents})
    let corners = 
        tiles
        |> List.filter (fun t -> nbUnmatchedBorders t (configs |> List.filter (fun c ->  c.ID <> t.ID)) = 2)

    corners |> Seq.map (fun t -> int64 t.ID) |> Seq.reduce (*)

solve myData