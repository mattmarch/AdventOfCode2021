module Day06

open Common

let getInput () = readLines "Inputs/06.txt" |> Seq.head

let initialState = List.init 9 (fun _ -> 0L)

let incrementFishCount counters thisFish =
    let previousFishCount = counters |> List.item thisFish
    counters |> List.updateAt thisFish (previousFishCount + 1L) 

let parseInput input =
    input
    |> splitBy ","
    |> List.map int
    |> List.fold incrementFishCount initialState
    
let runStep previousState =
    let newBirths = previousState |> List.item 0
    [
        // 0
        previousState |> List.item 1
        // 1
        previousState |> List.item 2
        // 2
        previousState |> List.item 3
        // 3
        previousState |> List.item 4
        // 4
        previousState |> List.item 5
        // 5
        previousState |> List.item 6
        // 6
        newBirths + (previousState |> List.item 7)
        // 7
        previousState |> List.item 8
        // 8
        newBirths
    ]

let rec runSteps numSteps startState =
    let nextState = runStep startState
    let stepsRemaining = numSteps - 1
    if stepsRemaining = 0 then
        nextState
    else
        runSteps stepsRemaining nextState
        
let solveA input =
    let fishCounts = parseInput input
    runSteps 80 fishCounts
    |> List.sum
    
let solveB input =
    let fishCounts = parseInput input
    runSteps 256 fishCounts
    |> List.sum
