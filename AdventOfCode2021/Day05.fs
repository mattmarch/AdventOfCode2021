module Day05

open Common

type Coord = {
    X: int
    Y: int
}

type Line = Coord * Coord

type Field = Map<Coord, int>

let getInput () = readLines "Inputs/05.txt"

let parseCoord coordString: Coord =
    let x, y =
        coordString
        |> splitBy ","
        |> List.map int
        |> unpack2
    { X = x; Y = y }

let parseLines line: Line =
    line
    |> splitBy " -> "
    |>List.map parseCoord
    |> unpack2
    
let isHorizontal (lineStart, lineEnd) =
    lineStart.Y = lineEnd.Y
    
let isVertical (lineStart, lineEnd) =
    lineStart.X = lineEnd.X
    
let pointsInRange rangeStart rangeEnd =
    let numberOfPoints = abs (rangeEnd - rangeStart) + 1
    let incrementDirection = if rangeEnd > rangeStart then 1 else -1
    List.init numberOfPoints (fun i -> rangeStart + (i * incrementDirection))

let getAllHorizontalPoints (lineStart, lineEnd) =
    pointsInRange lineStart.X lineEnd.X
    |> List.map (fun x -> { X = x; Y = lineStart.Y })
    
let getAllVerticalPoints (lineStart, lineEnd) =
    pointsInRange lineStart.Y lineEnd.Y
    |> List.map (fun y -> { X = lineStart.X; Y = y })

let markPointOnField (field: Field) (point: Coord) =
    match field |> Map.tryFind point with
    | Some existingCrossings -> field.Add (point, existingCrossings + 1)
    | None -> field.Add (point, 1)
    
let markPointsOnField field points =
    points |> List.fold markPointOnField field
    
let solveA input =
    let lines =
        input
        |> Seq.map parseLines
        |> Seq.toList
    let pointsFromHorizontalLines =
        lines
        |> List.filter isHorizontal
        |> List.map getAllHorizontalPoints
        |> List.concat
    let pointsFromVerticalLines =
        lines
        |> List.filter isVertical
        |> List.map getAllVerticalPoints
        |> List.concat
    let allPoints = List.concat [pointsFromHorizontalLines; pointsFromVerticalLines]
    let field = markPointsOnField (Map.ofList []) allPoints
    field
    |> Map.values
    |> Seq.filter (fun v -> v > 1)
    |> Seq.length