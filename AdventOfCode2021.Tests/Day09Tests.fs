module Day09Tests

open Xunit

open Common
open Day09

let getInput () = readLines "TestInputs/09.txt"

[<Fact>]
let ``Day09 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(15, (solveA testInput))


[<Fact>]
let ``Day08 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(1134, (solveB testInput))