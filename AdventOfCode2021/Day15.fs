module Day15

open Common

let getInput () = readLines "Inputs/15.txt"

let parseCharacter: char -> int = string >> int

let parseLine yIndex line =
    line
    |> Seq.indexed
    |> Seq.map (fun (x, c) -> (x, yIndex), parseCharacter c)

let parseAll input =
    input
    |> Seq.indexed
    |> Seq.map (fun (y, line) -> parseLine y line)
    |> Seq.concat
    |> Map.ofSeq

let getNeighbours (x, y) = [ x-1, y; x+1, y; x, y-1; x, y+1 ]

let setValue (x,y) value array =
    Array2D.set array x y (Some value)

let getValue (x,y) array =
    Array2D.get array x y

let traverseCavern (cavern: Map<int*int, int>) =
    let maxX, maxY = cavern |> Map.keys |> Seq.maxBy (fun (x,y) -> x + y)
    let startPoint = (0, 0)
    let cavernTotals =
        array2D (Array.init (maxY+1) (fun _ -> Array.create (maxX+1) None))
    cavernTotals |> setValue startPoint 0
    let recentlyPopulatedCoords = [startPoint, 0]
    let rec traverseNextSteps recentlyPopulated =
        let nextCoord, value = recentlyPopulated |> List.minBy snd
        if nextCoord = (maxX, maxY) then
            value
        else
            let neighbours =
                nextCoord
                |> getNeighbours
                |> List.choose (fun coord ->
                    match cavern |> Map.tryFind coord with
                    | None -> None
                    | Some cavernValue ->
                        match cavernTotals |> getValue coord with
                        | Some previousValue when previousValue > (cavernValue + value) -> Some (coord, cavernValue + value)
                        | None -> Some (coord, cavernValue + value)
                        | _ -> None
                    )
            neighbours |> List.iter (fun (coord, v) -> cavernTotals |> setValue coord v)
            let newRecentlyPopulated =
                (recentlyPopulated |> List.filter (fun (c, _) -> c <> nextCoord)) @ neighbours
            traverseNextSteps newRecentlyPopulated
    traverseNextSteps recentlyPopulatedCoords
            
let solveA input =
    let cavern =
        parseAll input
    traverseCavern cavern

let incrementAndWrapRiskLevel v inc = (v + inc - 1) % 9 + 1

let createLargeCavern smallCavern =
    let smallCavernItems = smallCavern |> Map.toList
    let xMax, yMax = smallCavernItems |> List.map fst |> List.maxBy (fun (x,y) -> x + y)
    let cavernXStretched =
        List.init 5 (fun i ->
            smallCavernItems
            |> List.map (fun ((x,y), v) -> (x + i*(xMax + 1), y), incrementAndWrapRiskLevel v i)
            )
        |> List.concat
    List.init 5 (fun i ->
        cavernXStretched
        |> List.map (fun ((x,y), v) -> (x, y + i*(yMax+1)), incrementAndWrapRiskLevel v i)
        )
    |> List.concat
    |> Map.ofList

let solveB input = 
    let cavern =
        parseAll input
        |> createLargeCavern
    traverseCavern cavern