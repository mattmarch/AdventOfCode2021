module Day19Tests

open Xunit

open Common
open Day19

let getInput () = readAll "TestInputs/19.txt"

[<Fact>]
let ``Day19 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(79, (solveA testInput))
    
[<Fact>]
let ``Day19 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(3621, (solveB testInput))
