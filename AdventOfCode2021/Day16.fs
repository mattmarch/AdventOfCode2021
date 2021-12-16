module Day16

open Common

type Packet =
    | Literal of LiteralPacket
    | Operator of OperatorPacket

and LiteralPacket = {
    Version: int64
    Value: int64
}

and OperatorPacket = {
    Version: int64
    Type: int
    LengthType: int
    ContainedPackets: Packet list 
}

let getInput () = readAll "Inputs/16.txt" |> trimString

let hexadecimalCharToBinaryString hexadecimalChar =
    match hexadecimalChar with
    | '0' -> "0000"| '1' -> "0001"| '2' -> "0010"| '3' -> "0011"| '4' -> "0100"| '5' -> "0101"| '6' -> "0110"| '7' -> "0111"
    | '8' -> "1000"| '9' -> "1001"| 'A' -> "1010"| 'B' -> "1011"| 'C' -> "1100"| 'D' -> "1101"| 'E' -> "1110"| 'F' -> "1111"
    | _ -> failwithf $"Invalid hexadecimal character %c{hexadecimalChar}"

let hexadecimalToBinaryDigitList hexadeximal =
    hexadeximal
    |> Seq.map hexadecimalCharToBinaryString
    |> Seq.concat
    |> Seq.toList
    
let binaryListToInt64 binaryList =
    binaryList
    |> List.rev
    |> List.indexed
    |> List.sumBy (fun (i, x) -> if x = '1' then 1L <<< i else 0)
    
let binaryListToInt = binaryListToInt64 >> int

let rec readLiteral version previousBits literalBits =
    match literalBits with
    | '0' :: l0 :: l1 :: l2 :: l3 :: remainingBits ->
        let value = previousBits @ [l0;l1;l2;l3] |> binaryListToInt64
        Literal { Version = version; Value = value }, remainingBits
    | '1' :: l0 :: l1 :: l2 :: l3 :: remainingBits ->
        readLiteral version (previousBits @ [l0;l1;l2;l3]) remainingBits
    | _ -> failwithf $"Unexpected literal sequence %A{literalBits}"

and readOperator0Type operatorBits =
    let subpacketLength = operatorBits |> List.take 15 |> binaryListToInt
    let subpacketBits = operatorBits |> List.skip 15 |> List.take subpacketLength
    let remainingBits = operatorBits |> List.skip (15 + subpacketLength)
    readPackets [] subpacketBits, remainingBits

and readOperator1Type operatorBits =
    let numberOfSubpackets = operatorBits |> List.take 11 |> binaryListToInt
    List.init numberOfSubpackets id
    |> List.mapFold (fun bits _ -> readPacket bits) (operatorBits |> List.skip 11)
    

and readPackets previousPackets bits =
    match readPacket bits with
    | p, [] -> (p :: previousPackets) |> List.rev 
    | p, remainingBits -> readPackets (p :: previousPackets) remainingBits

and readOperator version opType operatorBits = 
    let opLengthType, (containedPackets, remainingBits) =
        match operatorBits with
        | '0' :: remaining -> 0, readOperator0Type remaining
        | '1' :: remaining -> 1, readOperator1Type remaining
        | _ -> failwithf $"Could not parse operator %A{operatorBits}"
    (
        Operator { Version = version; Type = opType; LengthType = opLengthType; ContainedPackets = containedPackets },
        remainingBits
    )

and readPacket packet =
    match packet with
    | v0 :: v1 :: v2 :: t0 :: t1 :: t2 :: remainingBits ->
        let versionNumber = [v0;v1;v2] |> binaryListToInt
        let packetType = [t0;t1;t2] |> binaryListToInt
        match packetType with
        | 4 -> readLiteral versionNumber [] remainingBits
        | _ -> readOperator versionNumber packetType remainingBits
    | _ -> failwithf $"Packet %A{packet} did not contain expected bits for version and type"

let rec sumVersions packet =
    match packet with
    | Literal p -> p.Version
    | Operator p -> p.Version + List.sumBy sumVersions p.ContainedPackets

let solveA input =
    input
    |> hexadecimalToBinaryDigitList
    |> readPacket
    |> fst
    |> sumVersions

let rec evaluateFullPacketRules packet =
    let evaluateConditionOnExactlyTwoPackets condition packetList =
        match packetList with
        | [a; b] -> if condition (evaluateFullPacketRules a) (evaluateFullPacketRules b) then 1L else 0L
        | _ -> failwithf $"Expected exactly two subpackets but got %i{List.length packetList}"
    
    match packet with
    | Literal p -> p.Value
    | Operator p ->
        match p.Type with
        | 0 -> p.ContainedPackets |> List.sumBy evaluateFullPacketRules
        | 1 -> p.ContainedPackets |> List.map evaluateFullPacketRules |> List.reduce (*)
        | 2 -> p.ContainedPackets |> List.map evaluateFullPacketRules |> List.min
        | 3 -> p.ContainedPackets |> List.map evaluateFullPacketRules |> List.max
        | 5 -> p.ContainedPackets |> evaluateConditionOnExactlyTwoPackets (>)
        | 6 -> p.ContainedPackets |> evaluateConditionOnExactlyTwoPackets (<)
        | 7 -> p.ContainedPackets |> evaluateConditionOnExactlyTwoPackets (=)
        | unrecognised -> failwithf $"Found unrecognised operator type {unrecognised}"

let solveB input =
    input
    |> hexadecimalToBinaryDigitList
    |> readPacket
    |> fst
    |> evaluateFullPacketRules
