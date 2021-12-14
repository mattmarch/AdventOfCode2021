module Day14Tests

open Xunit

open Common
open Day14

let getInput () = readLines "TestInputs/14.txt"

[<Fact>]
let ``Day14 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(1588, (solveA testInput))

[<Fact>]
let ``Day14 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(2188189693529L, (solveB testInput))
