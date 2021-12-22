module Day22

open Common

type CubeState = On | Off

type Cuboid = {
    CubeState: CubeState
    XRange: int * int
    YRange: int * int
    ZRange: int * int
}

let getInput () = readLines "Inputs/22.txt"

let parseDimension dimensionString =
    let rangeString = dimensionString |> splitBy "=" |> List.last
    let startString, endString = rangeString |> splitBy ".." |> List.map int |> List.sort |> unpack2
    startString, endString

let parseLine line =
    let stateString, dimensionsString = line |> splitBy " " |> unpack2
    let state =
        match stateString with
        | "on" -> On
        | "off" -> Off
        | otherwise -> failwithf $"Unexpected state {otherwise}"
    let xRange, yRange, zRange = dimensionsString |> splitBy "," |> List.map parseDimension |> unpack3
    {
        CubeState = state; XRange = xRange; YRange = yRange; ZRange = zRange;
    }

let isWithinPartABound cuboid =
    let partABound = 50
    fst cuboid.XRange >= -partABound && snd cuboid.XRange <= partABound
    && fst cuboid.YRange >= -partABound && snd cuboid.YRange <= partABound
    && fst cuboid.ZRange >= -partABound && snd cuboid.ZRange <= partABound

let getIntersect1D (aStart, aEnd) (bStart, bEnd) =
    if bStart > aEnd || bEnd < aStart then
        None
    else
        Some (
            List.max [ aStart; bStart ],
            List.min [ aEnd; bEnd ]
            )

let getIntersect intersectState a b =
    let xIntersect = getIntersect1D a.XRange b.XRange
    let yIntersect = getIntersect1D a.YRange b.YRange
    let zIntersect = getIntersect1D a.ZRange b.ZRange
    match xIntersect, yIntersect, zIntersect with
    | Some x, Some y, Some z ->
        Some {
            CubeState = intersectState
            XRange = x
            YRange = y
            ZRange = z
        }
    | _ -> None
    
let getCuboidFromIntersect newCuboid oldCuboid =
    match newCuboid.CubeState, oldCuboid.CubeState with
    | On, On -> getIntersect Off newCuboid oldCuboid
    | On, Off -> getIntersect On newCuboid oldCuboid
    | Off, On -> getIntersect Off newCuboid oldCuboid
    | Off, Off -> getIntersect On newCuboid oldCuboid

let updateCuboidListWithNewCuboid cuboidList newCuboid =
    let overlapCuboids =
        cuboidList
        |> List.choose (getCuboidFromIntersect newCuboid)
    match newCuboid.CubeState with
    | On -> newCuboid :: overlapCuboids @ cuboidList
    | Off -> overlapCuboids @ cuboidList

let getDistance1D (lineStart, lineEnd) = int64 (1 + lineEnd - lineStart)

let getVolume cuboid =
    let sign = if cuboid.CubeState = On then 1L else -1L
    sign * (getDistance1D cuboid.XRange) * (getDistance1D cuboid.YRange) * (getDistance1D cuboid.ZRange)

let solveA input =
    input
    |> Seq.map parseLine
    |> Seq.filter isWithinPartABound
    |> Seq.fold updateCuboidListWithNewCuboid []
    |> Seq.sumBy getVolume

let solveB input =
    input
    |> Seq.map parseLine
    |> Seq.fold updateCuboidListWithNewCuboid []
    |> Seq.sumBy getVolume