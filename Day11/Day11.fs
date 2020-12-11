module AdventCalendar2020.Day11

let input =
//    System.IO.File.ReadAllLines("Day11/test.txt")
    System.IO.File.ReadAllLines("Day11/input.txt")
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
    | _ when newSeatings = seatings -> seatings
    | _ -> computeFinalSeating newSeatings
    
let part1 =
    computeFinalSeating flatSeatings
    |> List.filter (fun seat -> seat = '#')
    |> List.length
    
let rec lookdirection rowDir colDir row col (seatings:char list) =
    let currRow = (row+rowDir)
    let currCol = (col+colDir)
    let idx = (row+rowDir) * colCount + (col+colDir)
    match (currRow, currCol) with
        | (-1, _) -> '.'
        | (_, -1) -> '.'
        | (r, _) when r = rowCount -> '.'
        | (_, c) when c = colCount -> '.'
        | _ ->
            match seatings.[idx] with
            | '.' -> lookdirection rowDir colDir currRow currCol seatings
            | '#' -> '#'
            | 'L' -> 'L'
            | _ -> 'N'
            
let getLookdirectionsOccupationCount row col (seatings:char list) =

    let surroundingDirections = List.allPairs [-1..1] [-1..1]
                                |> List.filter (fun (a,b) -> a <> 0 || b <> 0)                                
    surroundingDirections
    |> List.map (fun (rowDir, colDir) -> lookdirection rowDir colDir row col seatings)
    |> List.filter (fun seat -> seat = '#')
    |> List.length
    
let applyLookingSeatingRule (seatings:char list) (row, col) =
    let idx = row * colCount + col
    let seat = seatings.[idx]
    match seat with
    | 'L' when (getLookdirectionsOccupationCount row col seatings) = 0 -> '#'
    | '#' when (getLookdirectionsOccupationCount row col seatings) >= 5 -> 'L'
    | _ -> seat
           
let rec computeFinalSeatingPart2 (seatings:char list) =
    let newSeatings = 
        allRowColCombinations
        |> List.map (applyLookingSeatingRule seatings)
    match newSeatings with
    | _ when newSeatings = seatings -> seatings
    | _ -> computeFinalSeatingPart2 newSeatings
    
// not the fastest solution...but fast enough :-)        
let part2 = 
    computeFinalSeatingPart2 flatSeatings
    |> List.filter (fun seat -> seat = '#')
    |> List.length