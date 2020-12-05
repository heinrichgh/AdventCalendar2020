module AdventCalendar2020.Day4

open System
open System.Text.RegularExpressions


let input = System.IO.File.ReadAllText("Day4/input.txt")

let requiredFields = [|
    "byr";
    "iyr";
    "eyr";
    "hgt";
    "hcl";
    "ecl";
    "pid"
|]

let validatePart1 (requiredFields:string[]) (passport:string) =
    requiredFields
    |> Array.map (fun req -> passport.IndexOf(req) <> -1)
    |> Array.fold (fun found accum -> found && accum) true

let part1 =
    input.Split("\n\n")
    |> Array.map (validatePart1 requiredFields)
    |> Array.filter id
    |> Array.length

let eyeColours = [|
        "amb";
        "blu";
        "brn";
        "gry";
        "grn";
        "hzl";
        "oth"
    |]

let validatePassportTuples passportTupleList =
    passportTupleList
    |> Array.map (fun passportTuple -> 
                        match passportTuple with
                        | ("byr", value) ->
                            let v = int value
                            v >= 1920 && v <= 2002
                        | ("iyr", value) -> 
                            let v = int value
                            v >= 2010 && v <= 2020
                        | ("eyr", value) -> 
                            let v = int value
                            v >= 2020 && v <= 2030
                        | ("hgt", value) when Regex.IsMatch(value, "\d+in") ->
                            let inches = int <| Regex.Match(value, "\d+").Value
                            inches >= 59 && inches <= 76
                        | ("hgt", value) when Regex.IsMatch(value, "\d+cm") -> 
                            let centimeters = int <| Regex.Match(value, "\d+").Value
                            centimeters >= 150 && centimeters <= 193
                        | ("hcl", value) ->
                            String.length value = 7 && Regex.IsMatch(value, "#[0-9a-f]{6}")
                        | ("ecl", value) ->
                            Array.map (fun eyeColour -> eyeColour = value) eyeColours 
                            |> Array.fold (fun valid accum -> valid || accum) false
                        | ("pid", value) ->
                            String.length value = 9 && Regex.IsMatch(value, "[0-9]{9}")
                        | _ -> false
                  )
    
let passportEntryToTuple (entry:string) =
    let entryArray = entry.Split(':')
    (entryArray.[0], entryArray.[1])
    
    
let splitPassportToTuples passport =
    passport
    |> Array.map passportEntryToTuple
    
let removeCidTuple passportTupleList =
    passportTupleList
    |> Array.filter (fun passportTuple -> 
                        match passportTuple with
                        | ("cid", _) -> false
                        | _ -> true
                )

let validatePart2 validatedPassportTupleList =
    validatedPassportTupleList |>
    Array.fold (fun valid accum -> valid && accum) true
    
let part2ManyMap =
    input.Split("\n\n")
    |> Array.map (fun passport -> passport.Split([|"\n"; " "|], StringSplitOptions.RemoveEmptyEntries))
    |> Array.map splitPassportToTuples
    |> Array.map removeCidTuple
    |> Array.map validatePassportTuples
    |> Array.filter (fun passportTupleList -> Array.length passportTupleList = Array.length requiredFields)    
    |> Array.filter validatePart2
    |> Array.length
    
let part2 =
    input.Split("\n\n")
    |> Array.map (
                    (fun (passport:string) -> passport.Split([|"\n"; " "|], StringSplitOptions.RemoveEmptyEntries))
                    >> splitPassportToTuples
                    >> removeCidTuple
                    >> validatePassportTuples
                )
    |> Array.filter (fun passportTupleList ->
        Array.length passportTupleList = Array.length requiredFields
        && validatePart2 passportTupleList)   
    |> Array.length