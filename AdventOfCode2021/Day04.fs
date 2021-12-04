module Day04

open Common

type GridValue =
    | Marked of int
    | Unmarked of int

type BingoGrid = GridValue list list

let getInput () = readAll "Inputs/04.txt"

let parseBingoRow rowString =
    rowString
    |> splitBy " "
    |> List.filter (fun v -> v <> "")
    |> List.map(trimString >> int >> Unmarked)

let parseBingoGrid gridString: BingoGrid =
    gridString
    |> splitBy "\n"
    |> List.map parseBingoRow

let parseInput input =
    let inputSections = input |> splitBy "\n\n"
    let drawnNumberString, bingoGridStrings =
        match inputSections with
        | [] -> failwithf "Failed because input is empty"
        | drawnNumberString :: bingoGridStrings -> drawnNumberString, bingoGridStrings
    let drawnNumbers = drawnNumberString |> splitBy "," |> List.map int
    let bingoGrids = bingoGridStrings |> List.map parseBingoGrid
    drawnNumbers, bingoGrids
    
let isMarked gridValue =
    match gridValue with
    | Marked _ -> true
    | Unmarked _ -> false
    
let isRowComplete row =
    row |> List.forall isMarked

let anyRowComplete (grid: BingoGrid) =
    match grid |> List.tryFind isRowComplete with
    | Some _ -> true
    | None -> false

let isGridComplete (grid: BingoGrid) =
    anyRowComplete grid || anyRowComplete (List.transpose grid)
    
let markValueInRow value row =
    let matchingIndexes =
        row
        |> List.indexed
        |> List.filter (fun (_, v) -> v = Unmarked value)
        |> List.map fst
    matchingIndexes
    |> List.fold (fun row i -> row |> List.updateAt i (Marked value)) row
    
let markValueInGrid value grid =
    grid
    |> List.map (markValueInRow value)
    
let rec playBingo bingoGrids drawnNumbers =
    let nextNumber = drawnNumbers |> List.head
    let updatedBingoGrids =
        bingoGrids
        |> List.map (markValueInGrid nextNumber)
    match updatedBingoGrids |> List.tryFind isGridComplete with
    | Some grid -> nextNumber, grid
    | None -> playBingo updatedBingoGrids (List.tail drawnNumbers)

let getUnmarkedValueElseZero gridValue =
    match gridValue with
    | Marked _ -> 0
    | Unmarked v -> v

let solveA input =
    let drawnNumbers, bingoGrids = parseInput input
    let winningNumber, winningGrid = playBingo bingoGrids drawnNumbers
    let unmarkedValueSum =
        winningGrid
        |> List.concat
        |> List.sumBy getUnmarkedValueElseZero
    unmarkedValueSum * winningNumber    

let rec playBingoUntilEnd bingoGrids drawnNumbers =
    let nextNumber = drawnNumbers |> List.head
    let updatedBingoGrids =
        bingoGrids
        |> List.map (markValueInGrid nextNumber)
    match updatedBingoGrids with
    | [] -> failwithf "No bingo grids remaining, this shouldn't happen!"
    | [ grid ] when isGridComplete grid -> nextNumber, grid
    | gridList ->
        playBingoUntilEnd
            (gridList |> List.filter (isGridComplete >> not))
            (List.tail drawnNumbers)

let solveB input =
    let drawnNumbers, bingoGrids = parseInput input
    let finalNumber, losingGrid = playBingoUntilEnd bingoGrids drawnNumbers
    let unmarkedValueSum =
        losingGrid
        |> List.concat
        |> List.sumBy getUnmarkedValueElseZero
    unmarkedValueSum * finalNumber
    