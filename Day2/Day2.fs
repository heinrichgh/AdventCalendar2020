module AdventCalendar2020.Day2

open System

let count x = Seq.filter ((=) x) >> Seq.length

let part1 = System.IO.File.ReadLines("Day2/input.txt")
                   |> Seq.map (fun line -> line.Split([|"-"; ": "; " "|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList )
                   |> Seq.map (fun splitLine -> (int splitLine.[0], int splitLine.[1], count (char splitLine.[2]) splitLine.[3]))
                   |> Seq.filter (fun (min, max, count) -> count >= min && count <= max )
                   |> Seq.length

      
let checkPositions position1 position2 character (password:string) = (password.[position1-1] = character) <> (password.[position2-1] = character)
                                                                
let part2 = System.IO.File.ReadLines("Day2/input.txt")
                   |> Seq.map (fun line -> line.Split([|"-"; ": "; " "|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList )
                   |> Seq.map (fun splitLine -> checkPositions(int splitLine.[0]) (int splitLine.[1]) (char splitLine.[2]) splitLine.[3])
                   |> Seq.filter id
                   |> Seq.length