module Day17Tests

open Xunit

open Common
open Day17

let getInput () = readAll "TestInputs/17.txt"

[<Fact>]
let ``Day17 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(45L, (solveA testInput))
    
[<Fact>]
let ``Day17 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(112L, (solveB testInput))
