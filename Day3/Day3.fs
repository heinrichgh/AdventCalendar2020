module AdventCalendar2020.Day3

open System.Numerics

let map = System.IO.File.ReadAllLines("Day3/input.txt") |> Array.toList
let width = String.length map.Head
let height = List.length map

let slopeTreeCount (right, down) = Seq.initInfinite (fun idx -> ((idx*right) % width, idx*down))
                                |> Seq.takeWhile (fun (_, down) -> down < height)
                                |> Seq.toList
                                |> List.map (fun (a, b) -> map.[b].[a] = '#')
                                |> List.filter id
                                |> List.length

let part1 = slopeTreeCount (3, 1)

let slopes = [|
               (1,1);
               (3,1);
               (5,1);
               (7,1);
               (1,2);
               |]

let bigint (x:int) = bigint(x)

let part2 = slopes
            |> Array.map slopeTreeCount
            |> Array.map bigint
            |> Array.fold (fun accum a -> a * accum) (bigint 1)