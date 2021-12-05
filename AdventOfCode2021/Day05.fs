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
    |> List.map parseCoord
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

let getAllCoordsOnLine (lineStart, lineEnd) =
    let numberOfCoords = 1 + List.max([
        abs (lineEnd.X - lineStart.X)
        abs (lineEnd.Y - lineStart.Y)
    ])
    let direction = {
        X = getDirection1d lineStart.X lineEnd.X
        Y = getDirection1d lineStart.Y lineEnd.Y
    }
    List.init numberOfCoords (getCoordAlongLine lineStart direction)

let markVentOnField (field: Field) (point: Coord) =
    match field |> Map.tryFind point with
    | Some existingVents -> field.Add (point, existingVents + 1)
    | None -> field.Add (point, 1)

let markVentsOnField points =
    let emptyField: Field = Map.ofList []
    points |> List.fold markVentOnField emptyField

let countOverlappingVents (field: Field) =
    field
    |> Map.values
    |> Seq.filter (fun v -> v > 1)
    |> Seq.length

let solveA input =
    let lines =
        input
        |> Seq.map parseLines
        |> Seq.toList
    let allVents =
        lines
        |> List.filter isHorizontalOrVertical
        |> List.map getAllCoordsOnLine
        |> List.concat
    let field = markVentsOnField allVents
    countOverlappingVents field

let solveB input =
    let lines =
        input
        |> Seq.map parseLines
        |> Seq.toList
    let allVents =
        lines
        |> List.map getAllCoordsOnLine
        |> List.concat
    let field = markVentsOnField allVents
    countOverlappingVents field
