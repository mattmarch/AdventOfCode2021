module Day09

open Common

type HeightMap = Map<int * int, int>

let getInput () = readLines "Inputs/09.txt"

let charToInt: char -> int = string >> int

let parseLine (y: int, line: string) =
    line
    |> Seq.indexed
    |> Seq.map (fun (x, h) -> ((x, y), charToInt h))

let parseInput (input: string seq): HeightMap =
    input
    |> Seq.indexed
    |> Seq.map parseLine
    |> Seq.concat
    |> Map.ofSeq

let getNeighbours (x, y) =
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

let lowPointChooser (heightMap: HeightMap) coord =
    let heightAtCoord = heightMap |> Map.find coord
    let lowerNeighbours =
        coord
        |> getNeighbours
        |> List.choose heightMap.TryFind
        |> List.filter (fun h -> h <= heightAtCoord)
    match lowerNeighbours with
    | [] -> Some (coord, heightAtCoord)
    | _ -> None
    
let solveA input =
    let heightmap = parseInput input
    heightmap
    |> Map.keys
    |> Seq.choose (lowPointChooser heightmap)
    |> Seq.sumBy (fun (_, h) -> h + 1)
    
let isInBasin heightmap coord =
    match heightmap |> Map.tryFind coord with
    | Some 9 -> false
    | Some _ -> true
    | None -> false
    
let findAllInBasin heightMap startingCoord =
    let rec findNeighboursRecursively alreadyFoundCoords coord =
        let newNeighboursInBasin =
            coord
            |> getNeighbours
            |> List.filter (not << (fun c -> alreadyFoundCoords |> List.contains c))
            |> List.filter (isInBasin heightMap)
        let updatedFoundCoords = alreadyFoundCoords @ newNeighboursInBasin
        newNeighboursInBasin
        |> List.fold findNeighboursRecursively updatedFoundCoords
    findNeighboursRecursively [ startingCoord ] startingCoord
    
let solveB input =
    let heightMap = parseInput input
    let lowPoints =
        heightMap
        |> Map.keys
        |> Seq.choose (lowPointChooser heightMap)
        |> Seq.map fst
    
    lowPoints
    |> Seq.map (findAllInBasin heightMap)
    |> Seq.map List.length
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.reduce (*)
