module AdventCalendar2020.Day4

open System


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

let validatePart1 (requiredFields:string[]) (passport:string) = requiredFields
                                                               |> Array.map (fun req -> passport.IndexOf(req) <> -1)
                                                               |> Array.fold (fun found accum -> found && accum) true

let part1 = input.Split("\n\n")
            |> Array.map (validatePart1 requiredFields)
            |> Array.filter id
            |> Array.length

