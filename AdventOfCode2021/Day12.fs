module Day12

open System
open Common

type CaveType =
    | BigCave of string
    | SmallCave of string
    | Start
    | End

type Cave = {
    CaveType: CaveType
    Passages: string list
}

let getInput () = readLines "Inputs/12.txt"

let parseConnection line =
    line
    |> splitBy "-"
    |> unpack2

let getConnectionsForCave allConnections caveName =
    allConnections
    |> List.choose (fun connection ->
        match connection with
        | thisCave, otherConnection when thisCave = caveName -> Some otherConnection
        | otherConnection, thisCave when thisCave = caveName -> Some otherConnection
        | _ -> None)

let createCave allConnections caveName =
    let caveType =
        match caveName with
        | "start" -> Start
        | "end" -> End
        | bigCave when bigCave |> Seq.head |> Char.IsUpper -> BigCave bigCave
        | smallCave -> SmallCave smallCave
    let connections = getConnectionsForCave allConnections caveName
    {
        CaveType = caveType
        Passages = connections
    }

let createCaveMap connections =
    let uniqueCaves =
        connections
        |> List.map (fun (a, b) -> [ a; b ])
        |> List.concat
        |> List.distinct
    uniqueCaves
    |> List.map (fun caveName -> caveName, createCave connections caveName)
    |> Map.ofList

let traverseAllRoutes caveMap =
    let startCaveName = "start"
    let startCave = caveMap |> Map.find startCaveName
    let rec traverseNextCaves pathSoFar nextCaveName =
        let nextCave = caveMap |> Map.find nextCaveName
        let updatedPath = nextCaveName :: pathSoFar
        match nextCave.CaveType with
        | End -> [ updatedPath ]
        | Start -> []
        | SmallCave s when pathSoFar |> List.contains s -> []
        | BigCave _ | SmallCave _ ->
            nextCave.Passages
            |> List.map (traverseNextCaves updatedPath)
            |> List.concat
    startCave.Passages
    |> List.map (traverseNextCaves [])
    |> List.concat
        
let solveA input =
    let caveMap =
        input
        |> Seq.map parseConnection
        |> List.ofSeq
        |> createCaveMap
    traverseAllRoutes caveMap
    |> List.length

let traverseAllRoutesPartB caveMap =
    let startCaveName = "start"
    let startCave = caveMap |> Map.find startCaveName
    let rec traverseNextCaves doubleVisitDone pathSoFar nextCaveName =
        let nextCave = caveMap |> Map.find nextCaveName
        let updatedPath = nextCaveName :: pathSoFar
        match nextCave.CaveType with
        | End -> [ updatedPath ]
        | Start -> []
        | SmallCave s when pathSoFar |> List.contains s ->
            if doubleVisitDone then
                []
            else
                nextCave.Passages
                |> List.map (traverseNextCaves true updatedPath)
                |> List.concat
        | BigCave _ | SmallCave _ ->
            nextCave.Passages
            |> List.map (traverseNextCaves doubleVisitDone updatedPath)
            |> List.concat
    startCave.Passages
    |> List.map (traverseNextCaves false [])
    |> List.concat
        
let solveB input =
    let caveMap =
        input
        |> Seq.map parseConnection
        |> List.ofSeq
        |> createCaveMap
    traverseAllRoutesPartB caveMap
    |> List.length
