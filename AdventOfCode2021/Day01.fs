module Day01

open Common

let getInput () = readLines "Inputs/01.txt" |> Seq.map int

let countIncreases = Seq.pairwise >> Seq.sumBy (fun (a, b) -> if b > a then 1 else 0)

let solveA = countIncreases

let solveB (depths: int seq) = 
    depths
    |> Seq.windowed 3
    |> Seq.map Array.sum
    |> countIncreases
