#load "../data/Day2_data.fsx"

open System
open System.Linq
open System.Text.RegularExpressions

let myData = List.ofArray Data.getData

type Accounts = 
    {
        Min : int32
        Max : int32
        Letter : char
        Password : string
        IsValid : bool
    }

let regexPattern = @"^(?'min'[0-9]+)-(?'max'[0-9]+)\s(?'letter'[a-z]):\s(?'pwd'[a-z]+)$"

let isValidPassword letterCount min max = min <= letterCount && letterCount <= max

let parseAndValidate r m =
    let r = Regex(r)
    let m1 = r.Match m

    if m1.Success then
        let min = Convert.ToInt32(m1.Groups.Item("min").Value)
        let max = Convert.ToInt32(m1.Groups.Item("max").Value)
        let letter = Convert.ToChar(m1.Groups.Item("letter").Value)
        let pwd = m1.Groups.Item("pwd").Value

        let count = pwd.Count(fun x -> x = letter)                

        { 
            Min = min; 
            Max = max; 
            Letter = letter; 
            Password = pwd; 
            IsValid = isValidPassword count min max
        }
        
    else
        { Min = 0; Max = 0; Letter = '-'; Password = m; IsValid = false }

let allAccounts = 
    [
        for line in myData do
            yield parseAndValidate regexPattern line

    ]

let goodPasswords = List.filter (fun x -> x.IsValid) allAccounts
goodPasswords.Count()