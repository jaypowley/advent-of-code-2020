#load "../../data/Day3_data.fsx"

open System
open System.Linq

let myData = List.ofArray Data.getData

type PathLocation = 
    {
        XCoord : int32
        YCoord : int32
        IsTree : bool
        DataLine : string
        Offset : int
        Character : char
    }

let appendToList l1 l2 =
    let rec rev acc = function
        | [] -> acc
        | x :: xs -> rev (x :: acc) xs
    rev [] (rev (rev [] l1) l2)

let getPath count x y =    
    let rec advancePath n z acc =         
        if acc >= count then 
            appendToList z [(n), (acc)]
        else 
            let newList = appendToList z [n, acc]
            advancePath (n + 3) newList (acc + 1)
    advancePath x y 1

let getOffSet x rowCount = x % rowCount

let getPathTrajectory (x, y) (data:string) = 
    let index = if x <= data.Length then x - 1
                elif (getOffSet x data.Length ) = 0 then 30
                else (getOffSet x data.Length ) - 1
    let isTree = data.ToCharArray().[index] = '#'
    { XCoord = x; YCoord = y; IsTree = isTree; DataLine = data; Offset = index; Character = data.ToCharArray().[index] }

let pathTaken = 
    [   
        let count = myData.Count()        
        for coord in (getPath count 1 []) do
            let index = (snd coord) - 1
            let line = List.item (index) myData
            yield getPathTrajectory coord line
    ]

let treesHit = List.filter (fun x -> x.IsTree) pathTaken
treesHit.Count()
