module AdventCalendar2020.Day11

let input =
    System.IO.File.ReadAllLines("Day11/test.txt")
//    System.IO.File.ReadAllLines("Day11/input.txt")
    |> Array.toList
    
let flatSeatings =
    input
    |> List.map (fun row -> row.ToCharArray() |> Array.toList)
    |> List.reduce List.append
    
let rowCount = input.Length
let colCount = input.[0].Length
    
let getSurroundingOccupationCount row col (seatings:char list) =
    let rowMin = max 0 (row-1)
    let rowMax = min (rowCount-1) (row+1)
    let colMin = max 0 (col-1)
    let colMax = min (colCount-1) (col+1)
    let surroundingIndices = List.allPairs [rowMin..rowMax] [colMin..colMax]
                            |> List.filter (fun (a,b) -> a <> row || b <> col)                                
    surroundingIndices
    |> List.map (fun (rowIdx, colIdx) -> seatings.[rowIdx * colCount + colIdx])
    |> List.filter (fun seat -> seat = '#')
    |> List.length
    
let applySeatingRule (seatings:char list) (row, col) =
    let idx = row * colCount + col
    let seat = seatings.[idx]
    match seat with
    | 'L' when (getSurroundingOccupationCount row col seatings) = 0 -> '#'
    | '#' when (getSurroundingOccupationCount row col seatings) >= 4 -> 'L'
    | _ -> seat
    
   
let allRowColCombinations = List.allPairs [0..rowCount-1] [0..colCount-1]
    
let rec computeFinalSeating (seatings:char list) =
    let newSeatings = 
        allRowColCombinations
        |> List.map (applySeatingRule seatings)
    match newSeatings with
    | _ when (List.compareWith Operators.compare newSeatings seatings) = 0 -> seatings
    | _ -> computeFinalSeating newSeatings
    
let part1 =
    computeFinalSeating flatSeatings
    |> List.filter (fun seat -> seat = '#')
    |> List.length