module AdventCalendar2020.Day5

let input = System.IO.File.ReadAllLines("Day5/input.txt")

//FBFBBFFRLR 0 128
//BFBBFFRLR 0 64



let rec calculateRow (code:char list) minRow maxRow =
    match code with
    | 'F'::tail -> calculateRow tail minRow (maxRow - (maxRow-minRow)/2 - (maxRow-minRow)%2)
    | 'B'::tail -> calculateRow tail (minRow + (maxRow-minRow)/2 + (maxRow-minRow)%2) maxRow
    | _ -> minRow
    
    
let rec calculateColumn (code:char list) minColumn maxColumn =
    match code with
    | 'L'::tail -> calculateColumn tail minColumn (maxColumn - (maxColumn-minColumn)/2 - (maxColumn-minColumn)%2)
    | 'R'::tail -> calculateColumn tail (minColumn + (maxColumn-minColumn)/2 + (maxColumn-minColumn)%2) maxColumn
    | _ -> minColumn
    
let calculateSeat (code:string) =
    (calculateRow (code.[0..6] |> Seq.toList) 0 127, calculateColumn (code.[7..9] |> Seq.toList) 0 7)

let calculateSeatId (row, column) =
    row * 8 + column

let part1 =
    (input
    |> Array.map (
        calculateSeat
        >> calculateSeatId
        )
    |> Array.sortDescending).[0]
    
let seatIds =
    input
    |> Array.map (
        calculateSeat
        >> calculateSeatId
        )
    |> Array.sort
    |> Array.toList
    
let part2 =
    (
         List.zip (seatIds.[0..((List.length seatIds)-2)]) (seatIds.Tail)
        |> List.map (fun (a, b) -> (a, b, b - a))
        |> List.filter (fun (a, b, diff) -> diff = 2)
    ).Head
    |> (fun (a, b, diff) -> a + 1)
    
