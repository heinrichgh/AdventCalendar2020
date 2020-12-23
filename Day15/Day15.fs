module AdventCalendar2020.Day15

let input = [|5;2;8;16;18;0;1|]
//let input = [|0;3;6;|]
//let input = [|1;3;2;|]
let tracker = input
            |> Array.mapi (fun i el -> (el, i+1))
            |> Map.ofArray

let rec playGame numberCount previousNumber (tracker:Map<int,int>) =
    let _ =
        if numberCount % 10000 = 0 then
            printfn "Progress %d" numberCount
                     
    if numberCount = 30000000 then
        previousNumber
    else
        let newNumber =
            if not (tracker.ContainsKey(previousNumber)) then
                0
            else
                let lastPos = tracker.[previousNumber]
                numberCount-lastPos       
        playGame (numberCount+1) newNumber (tracker.Add(previousNumber, numberCount))

let part1 = playGame (input.Length+1) 0 tracker