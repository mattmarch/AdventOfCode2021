module Day08Tests

open Xunit

open Common
open Day08

let getInput () = readLines "TestInputs/08.txt"

[<Fact>]
let ``Day08 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(26, (solveA testInput))


[<Fact>]
let ``Day08 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(61229, (solveB testInput))