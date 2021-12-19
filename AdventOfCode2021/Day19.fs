module Day19

open Common

let getInput () = readAll "Inputs/19.txt"

let parseCoordinates line =
    line
    |> splitBy ","
    |> List.map int
    |> unpack3

let parseInput input =
    input
    |> splitBy "\n\n"
    |> List.map (splitBy "\n" >> List.tail >> List.map parseCoordinates)

let addCoord (ax, ay, az) (bx, by, bz) = (ax + bx, ay + by, az + bz)
let subtractCoord (ax, ay, az) (bx, by, bz) = (ax - bx, ay - by, az - bz)

let moveCoordinateOrigin newOrigin coord = subtractCoord coord newOrigin

//let rotateX (x, y, z) = (x, -z, y)
//let rotateY (x, y, z) = (z, y, -x)
//let rotateZ (x, y, z) = (-y, x, z)

let getAllRotations (x, y, z) = [
   (x, y, z); (x, -z, y); (x, -y, -z); (x, z, -y); (z, y, -x); (z, x, y);
   (z, -y, x); (z, -x, -y); (-x, y, -z); (-x, z, y); (-x, -y, z); (-x, -z, -y);
   (-z, y, x); (-z, -x, y); (-z, -y, -x); (-z, x, -y); (-y, x, z); (-y, -z, x);
   (-y, -x, -z); (-y, z, -x); (y, -x, z); (y, -z, -x); (y, x, -z); (y, z, x)
]

let tryFindOverlapWithSameOrigin scanner1Beacons scanner2Beacons =
    let scanner1Set = scanner1Beacons |> Set.ofList
    scanner2Beacons
    |> List.map getAllRotations
    |> List.transpose
    |> List.map Set.ofList
    |> List.tryPick (fun scanner2set ->
        if Set.intersect scanner1Set scanner2set |> Set.count >= 12 then Some scanner2set else None)

let tryFindOverlapScanner1OriginFixed scanner1Beacons scanner2Beacons =
    scanner2Beacons
    |> List.map (fun beaconPosition -> scanner2Beacons |> List.map (moveCoordinateOrigin beaconPosition))
    |> List.tryPick (tryFindOverlapWithSameOrigin scanner1Beacons)

let tryFindOverlap scanner1Beacons scanner2Beacons =
    scanner1Beacons
    |> List.map (fun beaconPosition -> scanner1Beacons |> List.map (moveCoordinateOrigin beaconPosition))
    |> List.tryPick (fun s1Beacons ->
        match tryFindOverlapScanner1OriginFixed s1Beacons scanner2Beacons with
        | Some foundBeacons -> Some (s1Beacons |> Set.ofList |> Set.union foundBeacons |> Set.toList)
        | None -> None)

let buildMatchingScannerRegions allScannerBeacons =
    let rec buildMap mapSoFar remainingBeacons =
        printfn $"Beacons remaining: {List.length remainingBeacons}"
        let foundIndex, combinedMap =
            remainingBeacons
            |> List.indexed
            |> List.pick (fun (i, scannerBeacons) ->
                tryFindOverlap mapSoFar scannerBeacons
                |> Option.map (fun foundBeacons -> i, foundBeacons))
        let newRemainingBeacons = remainingBeacons |> List.removeAt foundIndex
        if List.length newRemainingBeacons = 0 then
            combinedMap
        else
            buildMap combinedMap newRemainingBeacons
    
    match allScannerBeacons with
    | scanner0Beacons :: otherScanners -> buildMap scanner0Beacons otherScanners
    | [] -> failwithf "No beacons in input"
    
let solveA input =
    input
    |> parseInput
    |> buildMatchingScannerRegions
    |> List.length

let manhattanDistance ((ax, ay, az), (bx, by, bz)) =
    abs (ax - bx) + abs (ay - by) + abs (az - bz)

let solveB input =
    input
    |> parseInput
    |> buildMatchingScannerRegions
    |> allCombinations
    |> List.map manhattanDistance
    |> List.max