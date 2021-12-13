module Day13Tests

open Xunit

open Common
open Day13

let getInput () = readAll "TestInputs/13.txt"

[<Fact>]
let ``Day13 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(17, (solveA testInput))
