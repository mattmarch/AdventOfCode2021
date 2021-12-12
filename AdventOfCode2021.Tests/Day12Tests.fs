module Day12Tests

open Xunit

open Common
open Day12

let getInputA () = readLines "TestInputs/12a.txt"
let getInputB () = readLines "TestInputs/12b.txt"
let getInputC () = readLines "TestInputs/12c.txt"

[<Fact>]
let ``Day12 Part A example 1`` () =
    let testInput = getInputA ()
    Assert.Equal(10, (solveA testInput))

[<Fact>]
let ``Day12 Part A example 2`` () =
    let testInput = getInputB ()
    Assert.Equal(19, (solveA testInput))

[<Fact>]
let ``Day12 Part A example 3`` () =
    let testInput = getInputC ()
    Assert.Equal(226, (solveA testInput))

[<Fact>]
let ``Day12 Part B example 1`` () =
    let testInput = getInputA ()
    Assert.Equal(36, (solveB testInput))

[<Fact>]
let ``Day12 Part B example 2`` () =
    let testInput = getInputB ()
    Assert.Equal(103, (solveB testInput))

[<Fact>]
let ``Day12 Part B example 3`` () =
    let testInput = getInputC ()
    Assert.Equal(3509, (solveB testInput))
