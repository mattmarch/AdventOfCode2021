module Day06Tests

open Xunit

open Common
open Day06

let getInput () = readSingleLine "TestInputs/06.txt"

[<Fact>]
let ``Day06 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(5934L, (solveA testInput))


[<Fact>]
let ``Day06 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(26984457539L, (solveB testInput))