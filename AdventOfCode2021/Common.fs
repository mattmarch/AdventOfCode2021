module Common

open System
open System.IO

let readLines path: string seq = File.ReadLines path

let readAll path = File.ReadAllText(path)

let readSingleLine = readLines >> Seq.head

let splitBy (separator: string) (inputString: string): string list = 
    inputString.Split([|separator|], StringSplitOptions.None) |> Array.toList

let unpack2 l =
    match l with
    | [a; b] -> a, b
    | _ -> failwithf $"Tried to unpack2 list without exactly 2 elements: %A{l}"

let unpack3 l =
    match l with
    | [a; b; c] -> a, b, c
    | _ -> failwithf $"Tried to unpack3 list without exactly 3 elements: %A{l}"
    
let binaryStringToInt binaryString = Convert.ToInt32(binaryString, 2)

let trimString (inputString: String) = inputString.Trim()

let allCombinations l =
    let indexedL = List.indexed l
    List.allPairs indexedL indexedL
    |> List.filter (fun ((i1, _), (i2, _)) -> i1 <> i2)
    |> List.map (fun ((_, n1), (_, n2)) -> n1, n2)
    
// For using in F# interactive
let testInputPath s = $"AdventOfCode2021.Tests/TestInputs/{s}"