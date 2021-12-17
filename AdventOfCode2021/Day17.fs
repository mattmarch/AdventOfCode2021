module Day17

open System
open Common

type ShotResult =
    | Hit
    | Miss
    | XOvershoot
    | XUndershoot

let getInput () = readAll "Inputs/17.txt" |> trimString

let parseInput input =
    let xRange, yRange =
        input |> splitBy "x=" |> List.item 1 |> splitBy ", y=" |> unpack2
    let xMin, xMax = xRange |> splitBy ".." |> List.map int64 |> unpack2
    let yMin, yMax = yRange |> splitBy ".." |> List.map int64 |> unpack2
    (xMin, yMin), (xMax, yMax)

let nextStep ((x: int64, y: int64), (vx: int64, vy: int64)) =
    let nextVx =
        match Math.Sign vx with
        | 1 -> vx - 1L
        | 0 -> vx
        | -1 -> x + 1L
        | otherwise -> failwithf $"Math.Sign returned unexpected ${otherwise}"
    (x + vx, y + vy), (nextVx, vy - 1L)
    
let withinTarget ((xMin, yMin), (xMax, yMax)) (x: int64, y: int64) =
    match x >= xMin, x <= xMax,  y >= yMin && y <= yMax with
    | true, true, true -> Hit
    | false, true, true -> XUndershoot
    | true, false, true -> XOvershoot
    | _ -> Miss
    
let hasOvershot ((_, yMin), (_, _)) (_: int64, y: int64) =
    y < yMin

let playShot target initialVelocity =
    List.unfold
        (fun (pos, vel) -> if hasOvershot target pos then None else Some (pos, nextStep (pos, vel)))
        ((0,0), initialVelocity)
    
let calculateShotResult target trajectory =
    let relativeToTarget = trajectory |> List.map (withinTarget target)
    if relativeToTarget |> List.exists ((=) Hit) then
        Hit
    elif relativeToTarget |> List.forall ((=) Miss) then
        Miss
    elif relativeToTarget |> List.exists ((=) XUndershoot) then
        XUndershoot
    else
        XOvershoot
        
let tryWithYVelocity target yVelocity =
    let rec tryWithVelocity attemptedXvs (xv, yv) =
        let nextAttemptedXvs = xv :: attemptedXvs
        let trajectory = playShot target (xv, yv)
        match trajectory |> (calculateShotResult target) with
        | Hit -> Some trajectory
        | Miss -> None
        | XOvershoot ->
            if attemptedXvs |> List.contains (xv - 1L) then None else tryWithVelocity nextAttemptedXvs (xv - 1L, yv) 
        | XUndershoot ->
            if attemptedXvs |> List.contains (xv + 1L) then None else tryWithVelocity nextAttemptedXvs (xv + 1L, yv)
    tryWithVelocity [] (20, yVelocity)
        
let testAllVelocities target =
    let rec testNextVelocities misses bestTrajectory nextY =
        match tryWithYVelocity target nextY with
        | Some trajectory -> testNextVelocities 0 trajectory (nextY + 1L)
        | None ->
            if misses > 100 then
                bestTrajectory
            else
                testNextVelocities (misses + 1) bestTrajectory (nextY + 1L)
    match testNextVelocities 0 [] 0 with
    | [] -> failwithf "Found no successful trajectories"
    | result -> result
    
let solveA input =
    let target = parseInput input
    let bestTrajectory = testAllVelocities target
    bestTrajectory
    |> List.map snd
    |> List.max

let tryFindAllWithYVelocity target yVelocity =
    let rec tryNextVelocities foundVelocities (xv, yv) =
        let trajectory = playShot target (xv, yv)
        match trajectory |> (calculateShotResult target) with
        | Hit -> tryNextVelocities ((xv,yv) :: foundVelocities) (xv + 1L, yv)
        | XUndershoot -> tryNextVelocities foundVelocities (xv + 1L, yv)
        | XOvershoot
        | Miss -> foundVelocities
    tryNextVelocities [] (0, yVelocity)

        
let tryFindAllVelocities target =
    let rec testNextVelocities misses foundVelocities nextY =
        let foundVelocitiesThisTurn = tryFindAllWithYVelocity target nextY
        match foundVelocitiesThisTurn with
        | [] ->
            if misses > 100 then
                foundVelocities
            else
                testNextVelocities (misses + 1) foundVelocities (nextY + 1L)
        | newFoundVelocities -> testNextVelocities 0 (newFoundVelocities @ foundVelocities) (nextY + 1L)
    let yMin = target |> fst |> snd
    testNextVelocities 0 [] yMin

let solveB input =
    let target = parseInput input
    let allMatchingVelocities = tryFindAllVelocities target
    allMatchingVelocities
    |> List.length