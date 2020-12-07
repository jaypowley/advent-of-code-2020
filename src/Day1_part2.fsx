#load "../data/Day1_data.fsx"

let myData = Data.getData

let target = 2020 

let getCartesianProduct xs ys = 
    xs |> List.collect (fun x -> ys |> List.map (fun y -> (x, y)))

let getCartesianProductOfThree xs ys zs = 
  xs
  |> getCartesianProduct ys
  |> getCartesianProduct zs
  |> List.map (fun (zs, (ys, xs)) -> xs, ys, zs)

let getSumEqualToTarget list = List.filter (fun (x, y, z) -> x + y + z = target) list

let product = 
    let allCombos = getCartesianProductOfThree myData myData myData
    let (a, b, c) = (getSumEqualToTarget allCombos).Head    
    a * b * c
    
product