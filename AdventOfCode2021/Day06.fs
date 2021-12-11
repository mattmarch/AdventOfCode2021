module Day06

open Common

let getInput () = readSingleLine "Inputs/06.txt"

let parseInput input =
    let fishCounts =
        input
        |> splitBy ","
        |> List.groupBy id
        |> List.map (fun (fishState, fishList) -> int fishState, List.length fishList |> int64)
        |> Map.ofList
    List.init 9 (fun i ->
        match fishCounts |> Map.tryFind i with
        | Some count -> count
        | None -> 0L
    )

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
