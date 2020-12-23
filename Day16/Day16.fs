module AdventCalendar2020.Day16

open System
open System.Text.RegularExpressions

//let input = System.IO.File.ReadAllText("Day16/test.txt")
//let newLine = Environment.NewLine
let input = System.IO.File.ReadAllText("Day16/input.txt")
let newLine = "\n"

let sections = input.Split(newLine + newLine)

let checkRule a b c d value =
    (value >= a && value <= b) || (value >= c && value <= d)

let ruleChecks =
    sections.[0].Split(newLine)
    |> Array.map (fun line ->
            Regex.Matches(line, ".+: (.+) or (.+)")
            |> Seq.cast<Match>
            |> Seq.map (fun regexMatch ->
                    Array.concat (seq [regexMatch.Groups.[1].ToString().Split('-'); regexMatch.Groups.[2].ToString().Split('-')])
                    |> Array.map int
                )
            |> Seq.head
        )
    |> Array.map (fun ruleLimits ->
            checkRule ruleLimits.[0] ruleLimits.[1] ruleLimits.[2] ruleLimits.[3]
        )


let part1 =
    sections.[2].Split(newLine)
    |> Array.tail
    |> Array.map (fun ticket -> ticket.Split(',', StringSplitOptions.RemoveEmptyEntries))
    |> Array.concat
    |> Array.map int
    |> Array.filter (fun ticketValue ->
            ruleChecks
            |> Array.map (fun ruleCheck -> ruleCheck ticketValue)
            |> Array.reduce (fun acc v -> acc || v)
            |> not
        )
    |> Array.sum


