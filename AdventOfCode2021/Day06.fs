module Day06

open Common

let getInput () = readSingleLine "Inputs/06.txt"

let incrementFishCount counters thisFish =
    let previousFishCount = counters |> List.item thisFish
    counters |> List.updateAt thisFish (previousFishCount + 1L) 

let parseInput input =
    let zeroFishCounts = [ for _ in 0 .. 8 -> 0L ] 
    input
    |> splitBy ","
    |> List.map int
    |> List.fold incrementFishCount zeroFishCounts

let runStep previousState =
    match previousState with
    | [p0; p1; p2; p3; p4; p5; p6; p7; p8] -> [p1; p2; p3; p4; p5; p6; p7 + p0; p8; p0]
    | _ -> failwithf $"Expected previous state to consist of exactly 9 values, instead got %i{List.length previousState}: %A{previousState}"

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
