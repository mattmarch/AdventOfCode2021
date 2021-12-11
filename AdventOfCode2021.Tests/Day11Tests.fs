module Day11Tests

open Xunit

open Common
open Day11

let getInput () = readLines "TestInputs/11.txt"

[<Fact>]
let ``Day11 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(1656, (solveA testInput))


[<Fact>]
let ``Day11 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(195, (solveB testInput))