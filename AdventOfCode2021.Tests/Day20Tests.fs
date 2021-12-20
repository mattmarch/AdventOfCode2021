module Day20Tests

open Xunit

open Common
open Day20

let getInput () = readAll "TestInputs/20.txt"

[<Fact>]
let ``Day20 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(35, (solveA testInput))
    
[<Fact>]
let ``Day20 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(3351, (solveB testInput))
