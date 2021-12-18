module Day18Tests

open Xunit

open Common
open Day18

let getInput () = readLines "TestInputs/18.txt"

[<Fact>]
let ``Day18 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(4140, (solveA testInput))
    
[<Fact>]
let ``Day18 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(3993, (solveB testInput))
