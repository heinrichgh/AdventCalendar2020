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


let replaceFirstOccurrence (replace:char) (string:string) =
    let pos = string.IndexOf("X")
    string.Substring(0, pos) + (replace.ToString()) + string.Substring(pos + 1)
    
let rec replaceXsWithCombination mask nth (combinations:char[]) =
    if combinations.Length = nth then
        mask
    else
        replaceXsWithCombination (replaceFirstOccurrence combinations.[nth] mask) (nth+1) combinations
    

let createOrMaskA (mask:string) =
    Convert.ToInt64(mask.Replace('A', '0'), 2)

let createAndMaskA (mask:string) =
    Convert.ToInt64(mask.Replace('A', '1'), 2)
    
let getFloatingMasks (mask:string) =
    let countXs = mask.Split('X').Length-1
    let combinationCount = (pown 2 countXs) - 1
    let workingMask = mask.Replace('0', 'A').Replace('1', 'A')
    [|0..combinationCount|]
    |> Array.map (fun num ->
        let combinationString = Convert.ToString(num, 2)
        let combinations =
            if combinationString.Length < countXs then
                let padCount = countXs - combinationString.Length
                (([|1..padCount|] |> Array.fold (fun accu _ -> "0" + accu) "") + combinationString).ToCharArray()                
            else
                combinationString.ToCharArray()            
        replaceXsWithCombination workingMask 0 combinations         
        )
    
let createAndMasks (maskVariations:string[]) =
    maskVariations
    |> Array.map (fun mask -> Convert.ToInt64(mask.Replace('A', '1'), 2))
    
let createOrMasks (maskVariations:string[]) =
    maskVariations
    |> Array.map (fun mask -> Convert.ToInt64(mask.Replace('A', '0'), 2))
    
let applyMasks memAddress orMask floatingAndMask floatingOrMask =
    ((memAddress ||| orMask) ||| floatingOrMask) &&& floatingAndMask
    
let getMemDefinition2 line =
    Regex.Matches(line, "mem\[(\d+)\] = (\d+)")
    |> Seq.cast<Match>
    |> Seq.map (fun regexMatch ->
                        match regexMatch with
                        | regexMatch when regexMatch.Success -> (int64 <| regexMatch.Groups.[1].ToString(), int64 <| regexMatch.Groups.[2].ToString())
                        | _ -> (int64 0,int64 0)
        )
    |> Seq.head
    
let rec parseInput2 (input:string list) (orMask:int64) (floatingAndMasks:Int64[]) (floatingOrMasks: int64[]) (memoryMap:Map<int64, int64>): Map<int64,int64> =
    match input with
    | line::tail when line.Contains("mask") ->
        let mask = line.Split(' ').[2]
        let floatingMasks = getFloatingMasks mask
        parseInput2 tail (createOrMask mask) (createAndMasks floatingMasks) (createOrMasks floatingMasks) memoryMap
    | line::tail when line.Contains("mem") ->
        let (memAddress, memValue) = getMemDefinition2 line
        let newMemoryMap =
            [|0..floatingAndMasks.Length-1|]
            |> Array.map (fun i -> applyMasks memAddress orMask floatingAndMasks.[i] floatingOrMasks.[i]) 
            |> Array.fold (fun (accu:Map<int64, int64>) address -> accu.Add(address, memValue)) memoryMap
        parseInput2 tail orMask floatingAndMasks floatingOrMasks newMemoryMap
    | _ -> memoryMap
    
let part2 =
    parseInput2 input (int64 0) [||] [||] Map.empty
    |> Map.toArray
    |> Array.map snd
    |> Array.sum
    