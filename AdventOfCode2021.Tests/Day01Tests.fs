module Day01Tests

open Xunit

open Common
open Day01

let getInput () = readLines "TestInputs/01.txt" |> Seq.map int

[<Fact>]
let ``Day01 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(7, (solveA testInput))


[<Fact>]
let ``Day01 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(5, (solveB testInput))