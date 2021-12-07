module Day07

open Common

let getInput () = readSingleLine "Inputs/07.txt"

let parseInput input =
    input
    |> splitBy ","
    |> List.map int
        
let distanceFrom a b = abs (b - a)
let solveA input =
    let positions = parseInput input
    let totalCrabs = List.length positions
    let median =
        positions
        |> List.sort
        |> List.item (totalCrabs / 2)
    positions
    |> List.sumBy (distanceFrom median)

let fuelUsed distance =
    distance * (1 + distance) / 2

let calculateTotalFuelToTarget positions target =
   positions
   |> Seq.sumBy (distanceFrom target >> fuelUsed)

let solveB input =
    let positions = parseInput input
    let farthestCrab = List.max positions
    let possibleSolutions = Seq.init (farthestCrab + 1) id
    possibleSolutions
    |> Seq.map (calculateTotalFuelToTarget positions)
    |> Seq.min
