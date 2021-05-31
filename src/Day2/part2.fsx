#load "../../data/Day2_data.fsx"

open System
open System.Linq
open System.Text.RegularExpressions

let myData = List.ofArray Data.getData

type Accounts = 
    {
        Pos1 : int32
        Pos2 : int32
        Letter : char
        Password : string
        IsValid : bool
    }

let regexPattern = @"^(?'min'[0-9]+)-(?'max'[0-9]+)\s(?'letter'[a-z]):\s(?'pwd'[a-z]+)$"

let isChar1Matching letter char1 = letter = char1

let isChar2Matching letter char2 = letter = char2

let isChar1AndChar2Matching letter char1 char2 = (isChar1Matching letter char1 && isChar2Matching letter char2)

let isValidPassword letter char1 char2 = 
    ((isChar1Matching letter char1 || isChar2Matching letter char2) && isChar1AndChar2Matching letter char1 char2 |> not)

let parseAndValidate r m =
    let r = Regex(r)
    let m1 = r.Match m

    if m1.Success then
        let pos1 = Convert.ToInt32(m1.Groups.Item("min").Value)
        let pos2 = Convert.ToInt32(m1.Groups.Item("max").Value)
        let letter = Convert.ToChar(m1.Groups.Item("letter").Value)
        let pwd = m1.Groups.Item("pwd").Value

        let pwdPos1 = pwd.ToCharArray().[pos1-1] 
        let pwdPos2 = pwd.ToCharArray().[pos2-1] 

        { 
            Pos1 = pos1; 
            Pos2 = pos2; 
            Letter = letter; 
            Password = pwd; 
            IsValid = isValidPassword letter pwdPos1 pwdPos2
        }
        
    else
        { Pos1 = 0; Pos2 = 0; Letter = 'a'; Password = m; IsValid = false }


let allAccounts = 
    [
        for line in myData do
            yield parseAndValidate regexPattern line

    ]

let goodPasswords = List.filter (fun x -> x.IsValid) allAccounts
goodPasswords.Count() 