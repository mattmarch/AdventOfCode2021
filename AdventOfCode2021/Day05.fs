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
    
let isHorizontalOrVertical (lineStart, lineEnd) =
    lineStart.X = lineEnd.X || lineStart.Y = lineEnd.Y
    
let getCoordAlongLine start direction distance =
    {
        X = start.X + (distance * direction.X)
        Y = start.Y + (distance * direction.Y)
    }

let getDirection1d fromPoint toPoint =
    match fromPoint, toPoint with
    | fromPoint, toPoint when toPoint > fromPoint -> 1
    | fromPoint, toPoint when toPoint < fromPoint -> -1
    | _ -> 0

let getAllPointsOnLine (lineStart, lineEnd) =
    let numberOfPoints = 1 + List.max([
        abs (lineEnd.X - lineStart.X)
        abs (lineEnd.Y - lineStart.Y)
    ])
    let direction = {
        X = getDirection1d lineStart.X lineEnd.X
        Y = getDirection1d lineStart.Y lineEnd.Y
    }
    List.init numberOfPoints (getCoordAlongLine lineStart direction)

let markPointOnField (field: Field) (point: Coord) =
    match field |> Map.tryFind point with
    | Some existingCrossings -> field.Add (point, existingCrossings + 1)
    | None -> field.Add (point, 1)

let emptyField: Field = Map.ofList []
    
let markPointsOnField points =
    points |> List.fold markPointOnField emptyField
    
let solveA input =
    let lines =
        input
        |> Seq.map parseLines
        |> Seq.toList
    let allPoints =
        lines
        |> List.filter isHorizontalOrVertical
        |> List.map getAllPointsOnLine
        |> List.concat
    let field = markPointsOnField allPoints
    field
    |> Map.values
    |> Seq.filter (fun v -> v > 1)
    |> Seq.length

let solveB input =
    let lines =
        input
        |> Seq.map parseLines
        |> Seq.toList
    let allPoints =
        lines
        |> List.map getAllPointsOnLine
        |> List.concat
    let field = markPointsOnField allPoints
    field
    |> Map.values
    |> Seq.filter (fun v -> v > 1)
    |> Seq.length
