module AdventCalendar2020.Day12

open System

let input = System.IO.File.ReadAllLines("Day12/input.txt") |> Array.toList   
//let input = ["F10";"N3";"F7";"R90";"F11"]

let decodeCommand (command:string) =
    let action = command.[0]
    let units = int command.[1..]
    (action, units)
    
let rotateShip facing rotation =
    let rotateAmount =
        match rotation with
        | ('L', units) -> units
        | ('R', units) -> -units
        | _ -> 0
    (facing + 360 + rotateAmount) % 360
    
let moveShipForward facing units =
    match facing with
    | 0 -> (0, units)
    | 90 -> (units, 0)
    | 180 -> (0, -units)
    | 270 -> (-units, 0)
    | _ -> (0,0)

let rec steerShip facing northSouth eastWest commands =
    match commands with
    | head::tail ->
        let command = decodeCommand head
        match command with
        | ('N', units) -> steerShip facing (northSouth+units) eastWest tail
        | ('S', units) -> steerShip facing (northSouth-units) eastWest tail
        | ('E', units) -> steerShip facing northSouth (eastWest+units) tail
        | ('W', units) -> steerShip facing northSouth (eastWest-units) tail
        | ('L', _) -> steerShip (rotateShip facing command) northSouth eastWest tail
        | ('R', _) -> steerShip (rotateShip facing command) northSouth eastWest tail
        | ('F', units) ->
            let (northSouthMove, eastWestMove) = moveShipForward facing units
            steerShip facing (northSouth+northSouthMove) (eastWest+eastWestMove) tail
    | [] -> (northSouth, eastWest)
    
let shipsPosition = steerShip 0 0 0 input
let part1 = abs (fst shipsPosition) + abs(snd shipsPosition)

let degreesToRad (degrees:int) =
    (float degrees) * Math.PI / 180.0

let rotateWaypoint northSouth eastWest rotation =
    let rad = degreesToRad (
                    match rotation with
                        | ('L', units) -> units
                        | ('R', units) -> -units
                        | _ -> 0
                    )
    (Convert.ToInt32 (eastWest*sin(rad) + northSouth*cos(rad)), Convert.ToInt32 (eastWest*cos(rad) - northSouth*sin(rad)))
    
let rec steerShipWithWaypoint northSouth eastWest nSWaypoint eWWaypoint commands =
    match commands with
    | head::tail ->
        let command = decodeCommand head
        match command with
        | ('N', units) -> steerShipWithWaypoint northSouth eastWest (nSWaypoint+units) eWWaypoint tail
        | ('S', units) -> steerShipWithWaypoint northSouth eastWest (nSWaypoint-units) eWWaypoint tail
        | ('E', units) -> steerShipWithWaypoint northSouth eastWest nSWaypoint (eWWaypoint+units) tail
        | ('W', units) -> steerShipWithWaypoint northSouth eastWest nSWaypoint (eWWaypoint-units) tail
        | ('L', _) ->
            let (newWaypointNorthSouth, newWaypointEastWest) = rotateWaypoint (float nSWaypoint) (float eWWaypoint) command
            steerShipWithWaypoint northSouth eastWest newWaypointNorthSouth newWaypointEastWest tail
        | ('R', _) -> 
            let (newWaypointNorthSouth, newWaypointEastWest) = rotateWaypoint (float nSWaypoint) (float eWWaypoint) command
            steerShipWithWaypoint northSouth eastWest newWaypointNorthSouth newWaypointEastWest tail
        | ('F', units) ->
           steerShipWithWaypoint (northSouth + nSWaypoint*units) (eastWest + eWWaypoint*units) nSWaypoint eWWaypoint tail
        | _ -> (northSouth, eastWest)
    | [] -> (northSouth, eastWest)
        
        
        
let shipsPositionWaypointed = steerShipWithWaypoint 0 0 1 10 input
let part2 = abs (fst shipsPositionWaypointed) + abs(snd shipsPositionWaypointed)
        