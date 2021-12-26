module Day23Tests

open Xunit

open Common
open Day23

let getInput () = readLines "TestInputs/23.txt"


[<Fact>]
let ``Day23 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(12521, (solveA testInput))
    
[<Fact>]
let ``Day23 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(2758514936282235L, (solveB testInput))
