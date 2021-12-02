module Day02

open Common

type Direction = Up | Down | Forward

let getInput () = readLines "Inputs/02.txt"

let parseLine line = 
    let directionString, distanceString = line |> splitBy " " |> unpack2
    let direction = match directionString with
                    | "up" -> Up
                    | "down" -> Down
                    | "forward" -> Forward
                    | unrecognised -> failwithf $"Unrecognised direction %s{unrecognised}"
    (direction, int distanceString)

let updatePosition (x, y) (direction, distance) =
    match direction with
    | Up -> (x, y - distance)
    | Down -> (x, y + distance)
    | Forward -> (x + distance, y)

let solveA directionsInput =
    let finalX, finalY =
        directionsInput 
        |> Seq.map parseLine
        |> Seq.fold updatePosition (0, 0)
    finalX * finalY
    
let updatePositionWithAim (x, y, aim) (direction, distance) =
    match direction with
    | Up -> (x, y, aim - distance)
    | Down -> (x, y, aim + distance)
    | Forward -> (x + distance, y + (aim * distance), aim)

let solveB directionsInput =
    let finalX, finalY, _ =
        directionsInput
        |> Seq.map parseLine
        |> Seq.fold updatePositionWithAim (0, 0, 0)
    finalX * finalY
    