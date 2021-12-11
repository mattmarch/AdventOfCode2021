module Day11

open Common

type Grid = Map<int*int, int>

let getInput () = readLines "Inputs/11.txt"

let charToInt: char -> int = string >> int

let parseRow (y: int, row: string) =
    row
    |> Seq.indexed
    |> Seq.map (fun (x, v) -> (x, y), charToInt v)

let parseGrid rows: Grid =
    rows
    |> Seq.indexed
    |> Seq.map parseRow
    |> Seq.concat
    |> Map.ofSeq

let findNeighbours (x, y) =
    [ (x-1, y-1); (x, y-1); (x+1, y-1); (x-1, y); (x+1, y); (x-1, y+1); (x, y+1); (x+1, y+1) ]

let incrementNeighbourEnergyFolder (energiesGrid: Grid) neighbourCoord =
    match energiesGrid |> Map.tryFind neighbourCoord with
    | None
    | Some 0 -> energiesGrid
    | Some neighbourEnergy -> energiesGrid |> Map.add neighbourCoord (neighbourEnergy + 1)

let incrementNeighboursEnergies energiesGrid coord =
    coord
    |> findNeighbours
    |> List.fold incrementNeighbourEnergyFolder energiesGrid

let rec runFlashes (energiesGrid: Grid, flashes: int) =
    let flashingOctopuses =
        energiesGrid
        |> Map.filter (fun _ e -> e >= 10)
        |> Map.keys
    match Seq.length flashingOctopuses with
    | 0 -> energiesGrid, flashes
    | newFlashes ->
        let gridWithFlashesSetToZero =
            flashingOctopuses
            |> Seq.fold (fun grid c -> grid |> Map.add c 0) energiesGrid
        let gridWithNeighboursIncreased =
            flashingOctopuses
            |> Seq.fold incrementNeighboursEnergies gridWithFlashesSetToZero
        runFlashes (gridWithNeighboursIncreased, flashes + newFlashes)

let runStep previousEnergiesGrid flashes =
    let gridWithInitialIncrease =
        previousEnergiesGrid |> Map.map (fun _ e -> e + 1)
    runFlashes (gridWithInitialIncrease, flashes)

let solveA input =
    let initialEnergiesGrid = parseGrid input
    List.init 100 id
    |> List.fold (fun (gridState, flashes) _ -> runStep gridState flashes) (initialEnergiesGrid, 0)
    |> snd

let rec getStepsUntilSynchronous previousGrid steps =
    let thisStep = steps + 1
    let nextGrid, flashes =
        runStep previousGrid 0
    if flashes = 100 then
        thisStep
    else
        getStepsUntilSynchronous nextGrid thisStep

let solveB input =
    let initialEnergiesGrid = parseGrid input
    getStepsUntilSynchronous initialEnergiesGrid 0
