module AdventCalendar2020.Day9

open System.Numerics

let input = System.IO.File.ReadAllLines("Day9/input.txt") |> Array.map BigInteger.Parse |> Array.toList

let checkNumber index =
    List.allPairs input.[index-25..index-1] input.[index-25..index-1]
    |> List.map (fun (a,b) -> (a + b))
    |> List.filter (fun sum -> sum = input.[index])
    |> List.length
    |> ((<) 0)

let part1 =
    [25..input.Length-1]
    |> List.map (fun index -> (input.[index], checkNumber index))
    |> List.filter (fun (_, found) -> not found)
    |> List.head
    |> fst

// 217430975

let sumRanges (indexRanges:(int*int) list) =
    indexRanges
    |> List.map (fun (startIdx, endIdx) -> (startIdx, endIdx, List.sum input.[startIdx..endIdx]))
    |> List.filter (fun (_, _, sum) -> sum = bigint 217430975)
    
let sumSmallestWithLargest (startIdx, endIdx, _) =
    let sortedRange = input.[startIdx..endIdx] |> List.sort
    sortedRange.Head + (List.rev sortedRange).Head
    
let part2 =
    [0..input.Length-1]
    |> List.map (
        (fun num -> List.allPairs [num] [num+1..input.Length-1])
        >> sumRanges)
    |> List.filter (fun list -> List.length list > 0 )
    |> List.head
    |> List.head
    |> sumSmallestWithLargest
    

// Parallel version of Part 2 - I do not get a result with this, keeps running forever... wonder why...

//let sumRangesArray (indexRanges:(int*int) []) = 
//        indexRanges
//        |> Array.Parallel.map (fun (startIdx, endIdx) -> (startIdx, endIdx, List.sum input.[startIdx..endIdx]))
//        |> Array.filter (fun (_, _, sum) -> sum = bigint 217430975)
//
//let part2Parallel =
//    [|0..input.Length-1|]
//    |> Array.Parallel.map (
//        (fun num -> Array.allPairs [|num|] [|num+1..input.Length-1|])
//        >> sumRangesArray
//        )
//    |> Array.filter (fun list -> Array.length list > 0 )
//    |> Array.head
//    |> Array.head
//    |> sumSmallestWithLargest
    

