module Day10

open Common

let getInput () = readLines "Inputs/10.txt"

let pairMatchingFolder (openPairsSoFar: char list) (nextChar: char) =
    match openPairsSoFar with
    | [] -> [ nextChar ]
    | lastOpen :: others ->
        match lastOpen, nextChar with
        | '(', ')'
        | '[', ']'
        | '{', '}'
        | '<', '>' -> others
        | _ -> nextChar :: openPairsSoFar

let matchValidPairsInLine line =
    line |> Seq.fold pairMatchingFolder []

let getCorruptionValueIfCorrupted c =
    match c with
    | ')' -> Some 3
    | ']' -> Some 57
    | '}' -> Some 1197
    | '>' -> Some 25137
    | _ -> None

let tryFindCorruptionValue unmatchedCharacters =
    unmatchedCharacters
    |> List.rev
    |> List.tryPick getCorruptionValueIfCorrupted

let solveA input =
    input
    |> Seq.map matchValidPairsInLine
    |> Seq.choose tryFindCorruptionValue
    |> Seq.sum
    
let getClosingCharacterValue c =
    match c with
    | '(' -> 1L
    | '[' -> 2L
    | '{' -> 3L
    | '<' -> 4L
    | _ -> failwithf $"Unexpected value %c{c} found in incomplete line"

let getLineCompletionScore unmatchedCharacters =
    unmatchedCharacters
    |> List.map getClosingCharacterValue
    |> List.reduce (fun acc nextVal -> acc * 5L + nextVal)

let uncorruptedFilter unmatchedCharacters =
    match tryFindCorruptionValue unmatchedCharacters with
    | Some _ -> false
    | None -> true

let solveB input =
    let autocompleteScores =
        input
        |> Seq.map matchValidPairsInLine
        |> Seq.filter uncorruptedFilter
        |> Seq.map getLineCompletionScore
        |> Seq.sort
    let middleIndex = Seq.length autocompleteScores / 2
    autocompleteScores |> Seq.item middleIndex