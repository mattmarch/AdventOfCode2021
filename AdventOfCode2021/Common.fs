module Common

open System
open System.IO

let readLines path: string seq = File.ReadLines path

let splitBy (separator: string) (inputString: string): string list = 
    inputString.Split([|separator|], StringSplitOptions.None) |> Array.toList

let unpack2 l =
    match l with
    | [a; b] -> a, b
    | _ -> failwithf $"Tried to unpack2 list without exactly 2 elements: %A{l}"
    
let binaryStringToInt binaryString = Convert.ToInt32(binaryString, 2)