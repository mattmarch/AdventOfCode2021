module Day14

open System
open Common

let getInput () = readLines "Inputs/14.txt"

let parseInsertionLine line =
    let insertionPair, insertionElement = line |> splitBy " -> " |> unpack2
    (
        insertionPair |> Seq.toList |> unpack2,
        insertionElement |> Seq.exactlyOne
    )
    
let parseAll input =
    let template = input |> Seq.head |> List.ofSeq
    let insertionRules =
        input
        |> Seq.skip 2
        |> Seq.map parseInsertionLine
        |> Map.ofSeq
    template, insertionRules
    
let polymerise insertionRules inputPolymer =
    let allButFirstElement =
        inputPolymer
        |> List.pairwise
        |> List.map (fun (a, b) -> [ insertionRules |> Map.find (a, b); b ])
        |> List.concat
    (inputPolymer |> List.head) :: allButFirstElement

let rec runPolymeriseSteps insertionRules stepsRemaining inputPolymer =
    let resultPolymer = polymerise insertionRules inputPolymer
    let resultStepsRemaining = stepsRemaining - 1
    if resultStepsRemaining = 0 then
        resultPolymer
    else
        runPolymeriseSteps insertionRules resultStepsRemaining resultPolymer

let solveA input =
    let template, insertionRules = parseAll input
    let resultingPolymer = runPolymeriseSteps insertionRules 10 template
    let elementCounts =
        resultingPolymer
        |> List.countBy id
        |> List.map snd
    List.max elementCounts - List.min elementCounts

let deduplicateCounts (counts: ('a*int64) list) =
    counts
    |> List.groupBy fst
    |> List.map (fun (key, countsWithKey) -> key, countsWithKey |> List.sumBy snd)

let rec polymeriseFrequencyCounts insertionRules stepsRemaining previousPairFrequencies: ((char*char)*int64) list =
    let nextFrequencies =
        previousPairFrequencies
        |> List.map (fun ((a, b), count) ->
            let insertedValue = insertionRules |> Map.find (a,b)
            [ ((a, insertedValue), count); ((insertedValue, b), count) ]
            )
        |> List.concat
        |> deduplicateCounts
    let nextStepsRemaining = stepsRemaining - 1
    if nextStepsRemaining = 0 then
        nextFrequencies
    else
        polymeriseFrequencyCounts insertionRules nextStepsRemaining nextFrequencies

let solveB input =
    let template, insertionRules = parseAll input
    let inputPairCounts =
        template
        |> List.pairwise
        |> List.countBy id
        |> List.map (fun (p, count) -> p, int64 count)
    let finalPairFrequencyCounts = polymeriseFrequencyCounts insertionRules 40 inputPairCounts
    let elementCounts =
        finalPairFrequencyCounts
        |> List.map (fun ((a,b), count) -> [ a, count; b, count ])
        |> List.concat
        |> deduplicateCounts
        |> List.map (fun (element, count) -> element, (count + 1L) / 2L)
    let allCounts = elementCounts |> List.map snd
    List.max allCounts - List.min allCounts
