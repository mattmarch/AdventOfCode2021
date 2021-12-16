module Day15Tests

open Xunit

open Common
open Day15

let getInput () = readLines "TestInputs/15.txt"

[<Fact>]
let ``Day15 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(40, (solveA testInput))

[<Fact>]
let ``Day15 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(315, (solveB testInput))
