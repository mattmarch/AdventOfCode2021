module Day04Tests

open Xunit

open Common
open Day04

let getInput () = readAll "TestInputs/04.txt"

[<Fact>]
let ``Day04 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(4512, (solveA testInput))


[<Fact>]
let ``Day04 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(1924, (solveB testInput))