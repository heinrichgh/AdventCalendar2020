module AdventCalendar2020.Day7

open System.Text.RegularExpressions

let input = System.IO.File.ReadAllLines("Day7/input.txt")

let getMainBagColour (rule:string) =
    rule.Substring(0, rule.IndexOf(" bags"))
    
let getContainingBagWithCount (rule:string) =    
    Regex.Matches(rule, "(\d+) (.+?) bag")
    |> Seq.cast<Match>
    |> Seq.map (fun regexMatch ->
                        match regexMatch with
                        | regexMatch when regexMatch.Success -> (regexMatch.Groups.[2].ToString(), int (regexMatch.Groups.[1].ToString()))
                        | _ -> ("", 0)
        )
    
let parsedRules =
    input
    |> Array.map (fun rule -> (getMainBagColour rule, getContainingBagWithCount rule))
    |> Map.ofArray
    
let rec convertToShinyGold (bagList:seq<string * int>) =
    let convert bag =
        match bag with
            | _ when (not <| parsedRules.ContainsKey(bag)) || parsedRules.ContainsKey(bag) && (Seq.length parsedRules.[bag]) = 0 ->
                0
            | _ -> convertToShinyGold parsedRules.[bag]
    
    bagList
    |> Seq.map (fun (bag, count) ->
                        match bag with
                            | "shiny gold" -> count
                            | _ -> convert bag
        
        )
    |> Seq.sum

let part1 =
    parsedRules
    |> Map.map (fun key -> convertToShinyGold)
    |> Map.filter (fun key -> fun value -> value > 0)
    |> Map.count

let rec calculateBagTotal (bag:string, count:int) =
    match count with
    | _ when (Seq.length parsedRules.[bag]) = 0 -> count
    | _ ->
        (parsedRules.[bag]
        |> Seq.map calculateBagTotal
        |> Seq.sum) * count + count // count the bags themselves    

let part2 =
    parsedRules.["shiny gold"]
    |> Seq.map calculateBagTotal
    |> Seq.sum
