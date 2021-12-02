module Day02Tests

open Xunit

open Common
open Day02

let getInput () = readLines "TestInputs/02.txt"

[<Fact>]
let ``Day02 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(150, (solveA testInput))


[<Fact>]
let ``Day02 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(900, (solveB testInput))