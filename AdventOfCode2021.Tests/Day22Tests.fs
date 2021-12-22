module Day22Tests

open Xunit

open Common
open Day22

let getInput () = readLines "TestInputs/22.txt"

[<Fact>]
let ``Day22 Part A`` () =
    let testInput = getInput ()
    Assert.Equal(590784L, (solveA testInput))
    
[<Fact>]
let ``Day22 Part B`` () =
    let testInput = getInput ()
    Assert.Equal(444356092776315L, (solveB testInput))
