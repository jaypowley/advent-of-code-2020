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

let validatePassport passport =
    passport.BirthYear <> None
    && passport.IssueYear <> None
    && passport.ExpirationYear <> None
    && passport.Height <> None
    && passport.HairColor <> None
    && passport.EyeColor <> None
    && passport.PassportId <> None
    // && passport.CountryId <> None 

let passports =
        dataSets
        |> Array.Parallel.map parsePassportMapOfLines
        |> Array.Parallel.map parsePassportOfMap

//passports.Count()

let validPassports =  Array.filter validatePassport passports
validPassports.Count()