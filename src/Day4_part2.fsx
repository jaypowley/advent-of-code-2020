#load "../data/Day4_data.fsx"

open System
open System.Linq
open System.Text.RegularExpressions

type PassportData = 
    {
        BirthYear : string option
        IssueYear : string option
        ExpirationYear : string option
        Height : string option
        HairColor : string option
        EyeColor : string option
        PassportId : string option
        CountryId : string option        
    }

let myData = List.ofArray Data.getData

let getDataFromList data = 
    List.fold (fun grouped line ->
        match line with
        | "" -> [] :: grouped
        | line -> (line :: grouped.Head) :: grouped.Tail                
    ) [[]] data

let dataSets = getDataFromList myData |> Array.ofList

let kvpRegex = Regex(@"(\w+):([^ ]+)", RegexOptions.Compiled)

let parsePassportMapOfLines lines =
    Seq.collect kvpRegex.Matches lines
    |> Seq.map (fun m -> (m.Groups.[1].Value, m.Groups.[2].Value))
    |> Map.ofSeq

let parsePassportOfMap (map: Map<string,string>) =
    let tryFind key = Map.tryFind key map
    {
        BirthYear = tryFind "byr"
        IssueYear = tryFind "iyr"
        ExpirationYear = tryFind "eyr"
        Height = tryFind "hgt"
        HairColor = tryFind "hcl"
        EyeColor = tryFind "ecl"
        PassportId = tryFind "pid"
        CountryId = tryFind "cid"
    }

let colorRegex = Regex(@"^#[0-9a-f]{6}$", RegexOptions.Compiled)
let isValidColor = colorRegex.IsMatch

let isIntInRange min max i =
    i >= min && i <= max

let isValidIntInRange min max str =
    try
        isIntInRange min max (int str)
    with
    | _ -> false

let distanceRegex = Regex(@"^(\d+)(cm|in)$", RegexOptions.Compiled)
type Distance =
    | Centimeter of int
    | Inches of int

let isValidHeight str =
    let matchHeight str =
        let m = distanceRegex.Match str
        match m.Success with
        | true -> Some (int m.Groups.[1].Value, m.Groups.[2].Value)
        | false -> None

    match matchHeight str with
    | Some (i, "cm") -> isIntInRange 150 193 i
    | Some (i, "in") -> isIntInRange 59 76 i
    | _ -> false

let validEyeColors = Set.ofList [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
let isValidEyeColor str = Set.contains str validEyeColors

let pidRegex = Regex(@"^\d{9}$", RegexOptions.Compiled)
let isValidPid = pidRegex.IsMatch

let isSomeAnd f = function
    | None -> false
    | Some v -> f v

let validatePassport passport =
    passport.BirthYear          |> isSomeAnd (isValidIntInRange 1920 2002)
    && passport.IssueYear       |> isSomeAnd (isValidIntInRange 2010 2020)
    && passport.ExpirationYear  |> isSomeAnd (isValidIntInRange 2020 2030)
    && passport.Height          |> isSomeAnd isValidHeight
    && passport.HairColor       |> isSomeAnd isValidColor
    && passport.EyeColor        |> isSomeAnd isValidEyeColor
    && passport.PassportId      |> isSomeAnd isValidPid

let passports =
        dataSets
        |> Array.Parallel.map parsePassportMapOfLines
        |> Array.Parallel.map parsePassportOfMap

passports.Count()

let validPassports =  Array.filter validatePassport passports
validPassports.Count()