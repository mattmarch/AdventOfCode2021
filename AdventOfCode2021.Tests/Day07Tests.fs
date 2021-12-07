module Day07Tests

open Xunit

open Common
open Day07

let getInput () = readSingleLine "TestInputs/07.txt"

[<Fact>]
let ``Day07 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(37, (solveA testInput))


[<Fact>]
let ``Day07 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(168, (solveB testInput))