module AdventCalendar2020.Day1

let numberList = System.IO.File.ReadLines("Day1/input.txt") |> Seq.map int |> Seq.toList
let part1 = (List.allPairs numberList numberList
                   |> List.map (fun (a, b) -> (a, b, a + b))
                   |> List.filter (fun (_, _, sum) -> sum = 2020)
                   |> List.map (fun (a, b, _) -> a * b)).Head

let part2 = (List.allPairs numberList numberList
                   |> List.allPairs numberList
                   |> List.map (fun (a, (b, c)) -> (a, b, c, a + b + c))
                   |> List.filter (fun (_, _, _, sum) -> sum = 2020)
                   |> List.map (fun (a, b, c, _) -> a * b * c)).Head
