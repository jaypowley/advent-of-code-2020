open System.IO
open System.Linq


let dayDirectory = __SOURCE_DIRECTORY__
let srcDirectory = Directory.GetParent(dayDirectory) |> string
let rootDirectory = Directory.GetParent(srcDirectory) |> string
let dataFile = rootDirectory + "\\Data\\Day20_data.txt"
let myData = System.IO.File.ReadAllLines dataFile |> Seq.chunkBySize(12) |> List.ofSeq

type Tile = 
    {
        name: int64
        data: char[,]
        hash: string list
    }

let monster = 
    let body = array2D [|
                            [|' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';' ';'#';' '|];
                            [|'#';' ';' ';' ';' ';'#';'#';' ';' ';' ';' ';'#';'#';' ';' ';' ';' ';'#';'#';'#'|];
                            [|' ';'#';' ';' ';'#';' ';' ';'#';' ';' ';'#';' ';' ';'#';' ';' ';'#';' ';' ';' '|]
                       |] 

    body
            |> Array2D.mapi(fun x y c -> (x, y, c)) 
            |> Seq.cast<int * int * char> 
            |> Seq.where(fun (_, _, c) -> c = '#') 
            |> Seq.map(fun (x, y, _) -> (y, x))
            |> List.ofSeq

let getHash (data: char[,]) =
    let calc (fwd: char[]) =
        let rev = fwd |> Array.rev
        (fwd |> Array.map(fun f -> string(f)) |> String.concat "", rev |> Array.map(fun f -> string(f)) |> String.concat "")

    let (hash1a, hash1b) = calc (Array.init 10 (fun x -> data.[x, 0])) //top
    let (hash2a, hash2b) = calc (Array.init 10 (fun x -> data.[9, x])) //right
    let (hash3a, hash3b) = calc (Array.init 10 (fun x -> data.[x, 9])) //bottom
    let (hash4a, hash4b) = calc (Array.init 10 (fun x -> data.[0, x])) //left

    [hash1a; hash1b; hash2a; hash2b; hash3a; hash3b; hash4a; hash4b]
        
let rotateRightData (data: char[,]) =
    let w, h = (data |> Array2D.length1), (data |> Array2D.length2)
    Array2D.init h w (fun c r -> data.[r, (w - c - 1)])
    
let rotateRightTile (tile: Tile) = 
    let data = rotateRightData tile.data
    let hash = getHash data
    
    {tile with data = data; hash = hash}

let flipHTile (tile: Tile) =
    let w, h = (tile.data |> Array2D.length1), (tile.data |> Array2D.length2)
    let data = Array2D.init w h (fun r c -> tile.data.[h - 1 - r, c])
    let hash = getHash data
    
    {tile with data = data; hash = hash}   

let flipVData (data: char[,]) =
    let w, h = (data |> Array2D.length1), (data |> Array2D.length2)
    Array2D.init w h (fun r c -> data.[r, w - 1 - c])
    
let flipVTile (tile: Tile) =
    let data = flipVData tile.data
    let hash = getHash data
    
    {tile with data = data; hash = hash}    

let rec arrange (step: int) (position: int * int) (tile1: Tile) (tile2: Tile) =
    let cc = Enumerable.Intersect(tile1.hash, tile2.hash) |> List.ofSeq
    let p1 = cc |> List.map(fun h -> tile1.hash |> List.findIndex(fun x -> x = h)) |> List.ofSeq
    let p2 = cc |> List.map(fun h -> tile2.hash |> List.findIndex(fun x -> x = h)) |> List.ofSeq
    
    if p1 = p2 then
        let (x, y) = position
        match (p1.[0], p1.[1]) with
        | (0, 1) -> ((x, y - 1), tile2 |> flipVTile)
        | (2, 3) -> ((x + 1, y), tile2 |> flipHTile)
        | (4, 5) -> ((x, y + 1), tile2 |> flipVTile)
        | (6, 7) -> ((x - 1, y), tile2 |> flipHTile)
        |   _   -> raise(System.Exception("Unable to match tile arrangement"))
    else
        let ntile2 = if step = 4 then flipHTile tile2 else rotateRightTile tile2
        arrange (step + 1) position tile1 ntile2

let rec organize (tiles: Tile list) (remaining: ((int * int) * Tile) list) (target: ((int * int) * Tile) list) (seen: int64 list) =
    if remaining.IsEmpty then target
    else
        let (position, tile) = remaining.Head
        let candidates = tiles.Tail |> Seq.map(fun t -> (t, t.hash.Intersect(tile.hash) |> List.ofSeq)) |> Seq.where(fun (_, h) -> h |> Seq.length > 0) |> List.ofSeq
        let arrangements = candidates |> List.map(fun (t, _) -> arrange 0 position tile t) |> List.ofSeq
        let exclude = (remaining |> List.map(fun (_, t) -> t.name)) @ (target |> List.map(fun (_, t) -> t.name))
        
        let nremaining = remaining.Tail @ (arrangements |> List.where(fun (_, a) -> not (exclude |> List.contains(a.name))))
        let ntarget = (position, tile) :: target
        let nseen = (arrangements |> List.map(fun (_, t) -> t.name)) @ (tile.name :: seen)

        organize tiles nremaining ntarget nseen

let radar (x: int) (y: int) (map: char[,]) (monster: (int * int) list) =
    let wx = map.GetLength(0)
    let wy = map.GetLength(1)

    let nc = monster |> List.map(fun (p, q) -> (x + p, y + q)) |> List.ofSeq
    let fd = nc |> List.map(fun (x, y) -> x < wx && y < wy && map.[x, y] = '#')
    
    fd |> List.forall(fun x -> x)

let chunkToTile (chunk: string[]) =
    let name = int64(chunk.[0].Substring(chunk.[0].Length - 5, 4))
    let data = Array2D.init 10 10 (fun x y -> chunk.[y+ 1].[x])
    let hash = getHash data

    {name = name; data = data; hash = hash}

let locate (r: char[,]) =
    r |> Array2D.mapi(fun x y _ -> radar x y r monster) |> Seq.cast<bool> |> Seq.where(fun x -> x) |> Seq.length
    
let execute =
    let subsize = 8
    let tiles = myData |> List.map(fun chunk -> chunkToTile chunk) |> List.ofSeq
    
    //let size = int(sqrt(double(tiles.Length)))
    let ordered = organize tiles [((0, 0), tiles.Head)] List.empty List.empty
    
    let minx = ordered |> List.map(fun ((x, _), _) -> x) |> List.min
    let maxx = ordered |> List.map(fun ((x, _), _) -> x) |> List.max
    let miny = ordered |> List.map(fun ((_, y), _) -> y) |> List.min
    let maxy = ordered |> List.map(fun ((_, y), _) -> y) |> List.max

    let width = (minx |> abs) + (maxx |> abs) + 1
    let height = (miny |> abs) + (maxy |> abs) + 1

    let tilemap = Array2D.init width height (fun x y -> ordered |> List.where(fun ((p, q), _) -> (x + minx) = p && (y + miny) = q) |> List.map(fun (_, t) -> t) |> List.head)
    let checksum = tilemap.[0, 0].name * tilemap.[width - 1, 0].name * tilemap.[width - 1, height - 1].name * tilemap.[0, height - 1].name
    printfn "Checksum of corner IDs: %d" checksum

    let picture = Array2D.init (width * subsize) (height * subsize) (fun x y -> tilemap.[x / subsize, y / subsize].data.[(x % subsize) + 1, (y % subsize) + 1])

    let p1 = picture
    let p2 = rotateRightData p1
    let p3 = rotateRightData p2
    let p4 = rotateRightData p3

    let p5 = flipVData picture
    let p6 = rotateRightData p5
    let p7 = rotateRightData p6
    let p8 = rotateRightData p7

    let results = [
        (p1 |> locate)
        (p2 |> locate)
        (p3 |> locate)
        (p4 |> locate)
        (p5 |> locate)
        (p6 |> locate)
        (p7 |> locate)
        (p8 |> locate)
    ]

    let monsters = results |> List.max
    let mlength = monster.Length
    let roughness = (picture |> Seq.cast<char> |> Seq.where(fun c -> c = '#') |> Seq.length) - monsters * mlength
    printfn "The water roughness is %d" roughness

execute