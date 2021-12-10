module Day10Tests

open Xunit

open Common
open Day10

let getInput () = readLines "TestInputs/10.txt"

[<Fact>]
let ``Day10 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(26397, (solveA testInput))


[<Fact>]
let ``Day10 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(288957L, (solveB testInput))