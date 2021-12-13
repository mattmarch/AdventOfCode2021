module Day13

open System
open Common

type Fold =
    | XFold of int
    | YFold of int

let getInput () = readAll "Inputs/13.txt"

let parseCoordinateLine line =
    line
    |> splitBy ","
    |> List.map int
    |> unpack2
    
let parseFoldLine line =
    let instruction, foldPoint =
        line
        |> splitBy "="
        |> unpack2
    match instruction with
    | "fold along y" -> YFold (int foldPoint)
    | "fold along x" -> XFold (int foldPoint)
    | _ -> failwithf $"Unrecognised fold instruction %s{instruction}"

let parseAll input =
    let dotCoordinatesString, foldInstructionsString =
        input
        |> splitBy "\n\n"
        |> unpack2
    let dotCoordinates =
        dotCoordinatesString
        |> splitBy "\n"
        |> List.map parseCoordinateLine
    let foldInstructions =
        foldInstructionsString
        |> splitBy "\n"
        |> List.map parseFoldLine
    dotCoordinates, foldInstructions

let completeFold dotCoordsSet foldInstruction =
    let foldedDotCondition, foldedDotMapper =
        match foldInstruction with
        | XFold position ->
            (fun (x, _) -> x > position), (fun (x, y) -> 2 * position - x, y)
        | YFold position ->
            (fun (_, y) -> y > position), (fun (x, y) -> x, 2 * position - y)
    let newFoldedDotPositions =
        dotCoordsSet
        |> Set.filter foldedDotCondition
        |> Set.map foldedDotMapper
    let unfoldedDotPositions =
        dotCoordsSet
        |> Set.filter (foldedDotCondition >> not)
    Set.union unfoldedDotPositions newFoldedDotPositions

let solveA input =
    let dotCoords, foldInstructions = parseAll input
    let dotCoordSet = Set.ofList dotCoords
    foldInstructions
    |> List.head
    |> completeFold dotCoordSet
    |> Set.count
    
let getLineForPrinting maxX points yCoord =
    let xCoordsOnLine =
        points
        |> List.filter (fun (_, y) -> y = yCoord)
        |> List.map fst
    List.init (maxX + 1) (fun x -> if xCoordsOnLine |> List.contains x then "#" else " ")
    |> String.concat ""
    
let solveB input =
    let dotCoords, foldInstructions = parseAll input
    let dotCoordSet = Set.ofList dotCoords
    let foldedPaper =
        foldInstructions
        |> List.fold completeFold dotCoordSet
        |> Set.toList
    let maxX = foldedPaper |> List.map fst |> List.max
    let maxY = foldedPaper |> List.map snd |> List.max
    List.init (maxY + 1) (getLineForPrinting maxX foldedPaper)
    |> List.iter (printfn "%s")
    "Answer printed above"
    
   