module Day18

open Common

type ReductionNeeded =
    | Explode of int
    | Split of int
    | Nothing

let getInput () = readLines "Inputs/18.txt"

let charToInt c = int c - int '0'

let snailFishNumberFolder (depth, numberSoFar) nextChar =
    match nextChar with
    | '[' -> depth + 1, numberSoFar
    | ']' -> depth - 1, numberSoFar
    | ',' -> depth, numberSoFar
    | n -> depth, (charToInt n, depth) :: numberSoFar 

let parseSnailFishNumber snailFishString =
    snailFishString
    |> Seq.fold snailFishNumberFolder (0, [])
    |> snd
    |> List.rev

let addToSnailFishNumberWithDepth (snailFishValue, snailFishDepth) increment =
    (snailFishValue + increment, snailFishDepth)

let addToValueIfExists index increment snailFishNumber =
    if index < 0 || index >= List.length snailFishNumber then
        snailFishNumber
    else
        snailFishNumber
        |> List.updateAt index (addToSnailFishNumberWithDepth snailFishNumber.[index] increment)
    
let explodeValues firstIndex snailFishNumber =
    let secondIndex = firstIndex + 1
    snailFishNumber
    |> addToValueIfExists (firstIndex - 1) (fst snailFishNumber.[firstIndex])
    |> addToValueIfExists (secondIndex + 1) (fst snailFishNumber.[secondIndex])
    |> List.removeAt secondIndex
    |> List.updateAt firstIndex (0, (snd snailFishNumber.[firstIndex]) - 1)

let splitValue index (snailFishNumber: (int * int) list) =
    let valueAtIndex, depthAtIndex = snailFishNumber.[index]
    let splitValue1, splitValue2 = valueAtIndex / 2, (valueAtIndex + 1) / 2
    snailFishNumber
    |> List.removeAt index
    |> List.insertManyAt index [(splitValue1, depthAtIndex+1); (splitValue2, depthAtIndex+1)]

let checkForReductions snailFishNumber =
    let explodeIndex =
        snailFishNumber
        |> List.tryFindIndex (fun (_, d) -> d >= 5)
    let splitIndex =
        snailFishNumber
        |> List.tryFindIndex (fun (v, _) -> v >= 10)
    match explodeIndex, splitIndex with
    | Some i, _ -> Explode i
    | None, Some i -> Split i
    | None, None -> Nothing

let rec reduceSnailFishNumber snailFishNumber =
    match checkForReductions snailFishNumber with
    | Nothing -> snailFishNumber
    | Explode i -> snailFishNumber |> explodeValues i |> reduceSnailFishNumber
    | Split i -> snailFishNumber |> splitValue i |> reduceSnailFishNumber

let addSnailFishNumbers sfNumber1 sfNumber2 =
    sfNumber1 @ sfNumber2
    |> List.map (fun (value, depth) -> value, depth + 1)
    |> reduceSnailFishNumber

let rec getSnailFishNumberMagnitude snailFishNumber =
    let maxDepth = snailFishNumber |> List.map snd |> List.max
    let firstMaxDepthIndex =
        snailFishNumber
        |> List.findIndex (fun (_, depth) -> depth = maxDepth)
    let magnitude = 3 * fst snailFishNumber.[firstMaxDepthIndex] + 2 * fst snailFishNumber.[firstMaxDepthIndex+1]
    let combinedNumber =
        snailFishNumber
        |> List.removeManyAt firstMaxDepthIndex 2
        |> List.insertAt firstMaxDepthIndex (magnitude, maxDepth - 1)
    match List.tryExactlyOne combinedNumber with
    | Some (value, _) -> value
    | None -> getSnailFishNumberMagnitude combinedNumber

let solveA input =
    input
    |> Seq.map parseSnailFishNumber
    |> Seq.reduce addSnailFishNumbers
    |> getSnailFishNumberMagnitude
    
let allCombinations l =
    let indexedL = List.indexed l
    List.allPairs indexedL indexedL
    |> List.filter (fun ((i1, _), (i2, _)) -> i1 <> i2)
    |> List.map (fun ((_, n1), (_, n2)) -> n1, n2)

let solveB input =
    input
    |> Seq.map parseSnailFishNumber
    |> Seq.toList
    |> allCombinations
    |> List.map (fun pairNumbers -> pairNumbers ||> addSnailFishNumbers |> getSnailFishNumberMagnitude)
    |> List.max
    
