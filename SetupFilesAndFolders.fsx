open System.IO

let sourceFolder = "src/"
let dataFolder = "data/"

let createDaysFiles (day:int) = 
    //Create src folder if not exists
    let dayFolder = sprintf "%sDay%i" sourceFolder day
    Directory.CreateDirectory dayFolder |> ignore

    //Create part1 and part2 files
    let part1File = File.Create (dayFolder + "/" + "part1.fsx")
    part1File.Close()
    let part2File = File.Create (dayFolder + "/" + "part2.fsx")
    part2File.Close()

    //Create data file
    let fileName = sprintf "Day%i_data.fsx" day
    let dataFile = File.Create (dataFolder + fileName)
    dataFile.Close()
    printf "Success!!\r\n"

createDaysFiles 25