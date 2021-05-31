#load "../../data/Day1_data.fsx"

let myData = Data.getData

let target = 2020 

let getCartesianProduct xs ys = 
    xs |> List.collect (fun x -> ys |> List.map (fun y -> (x, y)))

let getSumEqualToTarget list = List.filter (fun (x, y) -> x + y = target) list

let product = 
    let allCombos = getCartesianProduct myData myData
    let a = fst (getSumEqualToTarget allCombos).Head
    let b = snd (getSumEqualToTarget allCombos).Head  
    a * b
    
product