module AdventCalendar2020.Day10

let input =
//    System.IO.File.ReadAllLines("Day10/testSmall.txt")
//    System.IO.File.ReadAllLines("Day10/test.txt")
    System.IO.File.ReadAllLines("Day10/input.txt")
    |> Array.map int
    |> Array.toList

let adapterListWithOutlet = 0 :: input |> List.sort

let adapterListWithLaptop =
    ((adapterListWithOutlet |> List.rev |> List.head)
     + 3)
    :: input
    |> List.sort

let joltageDifferances =
    List.zip adapterListWithOutlet adapterListWithLaptop
    |> List.map (fun (a, b) -> b - a)
    
let oneDifference =
    (joltageDifferances
     |> List.filter (fun v -> v = 1)
     |> List.length)
    
let threeDifference =
    (joltageDifferances
     |> List.filter (fun v -> v = 3)
     |> List.length)
    
let part1 = oneDifference * threeDifference

let fullList = 0 ::adapterListWithLaptop

// had to read up on this one
// https://brilliant.org/wiki/tribonacci-sequence/
let tribonacci = [1; 1; 1; 2; 4; 7; 13] |> List.map bigint

// Since our input only has gaps of size 1 or 3 (never 2 in the given input, though the rules allow for it)
// count the number of consecutive 1's, then use that count as a lookup index on the tribonacci sequence
// The tribonacci number at index x gives us the the number of combinations to go from a number y to a number y+x
// with no steps bigger than 3 (tribonacci is a very interesting number sequence...) 
let rec calcRunsOfOne current combinations list =
    match list with
    | a::b::tail when b-a = 1 ->
        calcRunsOfOne (current+1) combinations (b::tail)
    | a::b::tail when b-a <> 1 ->
        calcRunsOfOne 1 (combinations * tribonacci.[current]) (b::tail)
    | _ -> combinations
    
let part2 = calcRunsOfOne 1 (bigint 1) fullList