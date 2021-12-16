module Day16Tests

open Xunit

open Common
open Day16

let getInput () = readAll "TestInputs/16.txt"

[<Fact>]
let ``Day16 Part A Example 1`` () =
    Assert.Equal(16L, (solveA "8A004A801A8002F478"))
    
[<Fact>]
let ``Day16 Part A Example 2`` () =
    Assert.Equal(12L, (solveA "620080001611562C8802118E34"))
    
[<Fact>]
let ``Day16 Part A Example 3`` () =
    Assert.Equal(23L, (solveA "C0015000016115A2E0802F182340"))
    
[<Fact>]
let ``Day16 Part A Example 4`` () =
    Assert.Equal(31L, (solveA "A0016C880162017C3686B18A3D4780"))

[<Fact>]
let ``Day16 Part B Example 1`` () =
    Assert.Equal(3L, (solveB "C200B40A82"))

[<Fact>]
let ``Day16 Part B Example 2`` () =
    Assert.Equal(54L, (solveB "04005AC33890"))
    
[<Fact>]
let ``Day16 Part B Example 3`` () =
    Assert.Equal(7L, (solveB "880086C3E88112"))

[<Fact>]
let ``Day16 Part B Example 4`` () =
    Assert.Equal(9L, (solveB "CE00C43D881120"))

[<Fact>]
let ``Day16 Part B Example 5`` () =
    Assert.Equal(1L, (solveB "D8005AC2A8F0"))

[<Fact>]
let ``Day16 Part B Example 6`` () =
    Assert.Equal(0L, (solveB "F600BC2D8F"))

[<Fact>]
let ``Day16 Part B Example 7`` () =
    Assert.Equal(0L, (solveB "9C005AC2F8F0"))

[<Fact>]
let ``Day16 Part B Example 8`` () =
    Assert.Equal(1L, (solveB "9C0141080250320F1802104A08"))
