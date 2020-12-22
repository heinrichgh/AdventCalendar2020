module AdventCalendar2020.Day13

open System

//let input = System.IO.File.ReadAllLines("Day13/test.txt") |> Array.toList   
let input = System.IO.File.ReadAllLines("Day13/input.txt") |> Array.toList   

let departTime = int input.Head
let busses =
    input.[1].Split(',')
    |> Array.filter (fun bus -> bus <> "x")
    |> Array.map int
    
let part1 =
    busses
    |> Array.map (fun bus ->
        let multiplier = Convert.ToInt32 (ceil ((float departTime) / (float bus)))
        let closestBusTime = bus * multiplier
        (bus, closestBusTime, closestBusTime - departTime))
    |> Array.sortBy (fun (_, _, timeDiff) -> timeDiff)
    |> Array.head
    |> (fun (bus, _, timeDiff) -> bus * timeDiff)
        
let bussesWithOffset =
    input.[1].Split(',')
    |> Array.mapi (fun idx bus -> (bus, bigint idx) )
    |> Array.filter (fun (bus, _) -> bus <> "x")
    |> Array.map (fun (bus, idx) -> (bigint (int bus), idx) )

//let biggestBus =
//    bussesWithOffset
//    |> Array.sortByDescending fst
//    |> Array.head
//    
//let biggestBusId =
//    fst biggestBus
//let startingTimestamp =
//    biggestBusId - (snd biggestBus)
//    
//    
//let testTimeStamp (timestamp:bigint) =
//    bussesWithOffset
//    |> Array.filter (fun (bus, offset) -> (timestamp+offset) % bus = bigint 0 )
//    |> Array.length = bussesWithOffset.Length
//
//let rec findEarliestTimestamp timestamp =
//    match testTimeStamp timestamp with
//    | false -> findEarliestTimestamp (timestamp+biggestBusId)
//    | true -> timestamp

//let part2 = findEarliestTimestamp startingTimestamp


// Chinese remainder theorem

// Sieving method

let testSet = [|(5, 4); (4, 3); (3, 0)|] |> Array.map (fun (a,b) -> (bigint a, bigint b))

let sortedDescendedOffsetBusses =
//    testSet
    bussesWithOffset
    |> Array.sortByDescending fst
    |> Array.map (fun (a, b) -> (a, (a-b)%a))
    |> Array.toList
    

let bigintZero = bigint 0

let rec searchSieve start step (busListLeft:(bigint * bigint) list) =
//    printfn "%A" (start, step, busListLeft)
    match busListLeft with
    | _::tail ->        
        let nextStart = start + step
        let modulo = fst (List.head busListLeft)
        let remainder = snd (List.head busListLeft)
        if (nextStart % modulo) = remainder then
            searchSieve nextStart (modulo * step) tail
        else
            searchSieve nextStart step busListLeft
    | [] -> start


let part2 =
    searchSieve (snd  <| List.head sortedDescendedOffsetBusses) (fst <| List.head sortedDescendedOffsetBusses) (List.tail sortedDescendedOffsetBusses)


//let rec gcd a b =
//    match b with
//    | 0 -> a
//    | _ -> gcd b (a%b)
//    
//
//let lcm a b =
//    (a*b) / (gcd a b)
//    


    
    