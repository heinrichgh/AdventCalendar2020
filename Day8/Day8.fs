module AdventCalendar2020.Day8

open System

let input = System.IO.File.ReadAllLines("Day8/input.txt")

type instruction = (string * int * bool)

let instructionSet =
    input
    |> Array.map (fun line ->
                    let split = line.Split(" ", StringSplitOptions.RemoveEmptyEntries)
                    (split.[0], int split.[1], false)
                  )
    |> Array.toList
    
let updateInstruction (instructionSet:instruction list) instructionLine instruction =
    instructionSet.[..instructionLine-1] @ (instruction::instructionSet.[instructionLine+1..])

let rec execute (instructionLine:int) (accumulator:int) (instructionSet:instruction list)=
    let instruction = instructionSet.[instructionLine]
    match instruction with
    | ("acc", number, false) -> execute (instructionLine+1) (accumulator+number) (updateInstruction instructionSet instructionLine ("acc", number, true))
    | ("jmp", number, false) -> execute (instructionLine+number) accumulator (updateInstruction instructionSet instructionLine ("jmp", number, true))
    | ("nop", number, false) -> execute (instructionLine+1) accumulator (updateInstruction instructionSet instructionLine ("nop", number, true))
    | _ -> accumulator
    
let part1 = execute 0 0 instructionSet

let rec executeWithLineCheck (instructionLine:int) (accumulator:int) (instructionSet:instruction list) =    
    let instruction = instructionSet.[instructionLine]
    match instruction with    
    | ("acc", number, false) -> executeWithLineCheck (instructionLine+1) (accumulator+number) (updateInstruction instructionSet instructionLine ("acc", number, true))
    | ("jmp", number, false) when (instructionLine+number) < 0 || (instructionLine+number) > instructionSet.Length -> (0, false)
    | ("jmp", number, false) -> executeWithLineCheck (instructionLine+number) accumulator (updateInstruction instructionSet instructionLine ("jmp", number, true))
    | ("nop", number, false) -> executeWithLineCheck (instructionLine+1) accumulator (updateInstruction instructionSet instructionLine ("nop", number, true))
    | ("done", _, _) -> (accumulator, true)
    | _ -> (accumulator, false)

let instructionSetWithLineNumbers =
    instructionSet
    |> List.mapi (fun i (instruction, number, visited) -> (instruction, number, visited, i))
    
let indexesOfJmpAndNop =
    instructionSetWithLineNumbers
    |> List.filter (fun (instruction, _, _, _) -> instruction = "nop" || instruction = "jmp")
    |> List.map (fun (instruction, number, visited, idx) -> idx)

let addDoneOpp (instructionSet:instruction list) =
    instructionSet @ [("done", 0, false)]

let generateAlternateInstructionSet lineToChange =
    let instruction = instructionSet.[lineToChange]
    match instruction with
    | ("jmp", number, false) -> updateInstruction instructionSet lineToChange ("nop", number, false)
    | ("nop", number, false) -> updateInstruction instructionSet lineToChange ("jmp", number, false)
    | _ -> instructionSet

let part2 =
    indexesOfJmpAndNop
    |> List.map (generateAlternateInstructionSet >> addDoneOpp >> executeWithLineCheck 0 0)
    |> List.filter (fun (_, ranTillEnd) -> ranTillEnd = true)
    |> List.head
    |> fst
