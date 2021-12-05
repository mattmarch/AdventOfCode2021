module Day05Tests

open Xunit

open Common
open Day05

let getInput () = readLines "TestInputs/05.txt"

[<Fact>]
let ``Day05 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(5, (solveA testInput))


[<Fact>]
let ``Day05 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(12, (solveB testInput))