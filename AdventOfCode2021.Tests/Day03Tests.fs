module Day03Tests

open Xunit

open Common
open Day03

let getInput () = readLines "TestInputs/03.txt"

[<Fact>]
let ``Day03 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(198, (solveA testInput))


[<Fact>]
let ``Day03 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(230, (solveB testInput))