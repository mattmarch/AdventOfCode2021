module Day21Tests

open Xunit

open Common
open Day21

let getInput () = readLines "TestInputs/21.txt"

[<Fact>]
let ``Day21 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(739785, (solveA testInput))
    
[<Fact>]
let ``Day21 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(444356092776315L, (solveB testInput))
