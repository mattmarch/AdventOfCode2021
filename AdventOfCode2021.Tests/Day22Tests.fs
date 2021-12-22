module Day22Tests

open Xunit

open Common
open Day22

let getInputA () = readLines "TestInputs/22a.txt"
let getInputB () = readLines "TestInputs/22b.txt"


[<Fact>]
let ``Day22 Part A`` () =
    let testInput = getInputA ()
    Assert.Equal(590784L, (solveA testInput))
    
[<Fact>]
let ``Day22 Part B`` () =
    let testInput = getInputB ()
    Assert.Equal(2758514936282235L, (solveB testInput))
