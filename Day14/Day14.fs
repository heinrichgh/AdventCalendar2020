module AdventCalendar2020.Day14

open System
open System.Text.RegularExpressions
 
//let input = System.IO.File.ReadAllLines("Day14/test.txt") |> Array.toList  
let input = System.IO.File.ReadAllLines("Day14/input.txt") |> Array.toList  

let createOrMask (mask:string) =
    Convert.ToInt64(mask.Replace('X', '0'), 2)

let createAndMask (mask:string) =
    Convert.ToInt64(mask.Replace('X', '1'), 2)
    
let getMemDefinition line =
    Regex.Matches(line, "mem\[(\d+)\] = (\d+)")
    |> Seq.cast<Match>
    |> Seq.map (fun regexMatch ->
                        match regexMatch with
                        | regexMatch when regexMatch.Success -> (int <| regexMatch.Groups.[1].ToString(), int64 <| regexMatch.Groups.[2].ToString())
                        | _ -> (0,int64 0)
        )
    |> Seq.head
    
let applyMask memValue andMask orMask =
    ((memValue &&& andMask) ||| orMask)

let rec parseInput (input:string list) (andMask:Int64) (orMask: int64) (memoryMap:Map<int, int64>): Map<int,int64> =
    match input with
    | line::tail when line.Contains("mask") ->
        let mask = line.Split(' ').[2]
        parseInput tail (createAndMask mask) (createOrMask mask) memoryMap
    | line::tail when line.Contains("mem") ->
        let (memAddress, memValue) = getMemDefinition line        
        parseInput tail andMask orMask (memoryMap.Add(memAddress, (applyMask memValue andMask orMask)))
    | _ -> memoryMap
    
let part1 =
    parseInput input (int64 Int64.MaxValue) (int64 0) Map.empty
    |> Map.toArray
    |> Array.map snd
    |> Array.sum


//let part1 =
//    let andMask = createAndMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
//    let orMask = createOrMask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
//    printfn "%A" andMask
//    printfn "%A" (Convert.ToString(andMask, 2))
//    printfn "%A" orMask
//    printfn "%A" (Convert.ToString(orMask, 2))
//    printfn "%A" (applyMask (int64 0) andMask orMask)
    
    
    