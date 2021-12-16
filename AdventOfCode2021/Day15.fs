module Day15

open Common

type Location = {
    RiskLevel: int
    CumulativeRiskLevel: int option
    Checked: bool
}

type Cavern = Map<int*int, Location>

let getInput () = readLines "Inputs/15.txt"

let parseCharacter c =
    { RiskLevel = c |> string |> int; CumulativeRiskLevel = None; Checked = false }

let parseLine yIndex line =
    line
    |> Seq.indexed
    |> Seq.map (fun (x, c) -> (x, yIndex), parseCharacter c)

let parseAll input =
    input
    |> Seq.indexed
    |> Seq.map (fun (y, line) -> parseLine y line)
    |> Seq.concat
    |> Map.ofSeq

let updateLocation coord (updater: Location -> Location) (cavern: Cavern) =
    cavern
    |> Map.change coord (fun prevLocation ->
        match prevLocation with
        | Some location -> Some (updater location)
        | None -> failwithf $"Can't update location %A{coord} which doesn't exist"
        )


let updateCumulativeRisk coord newRiskLevel (cavern: Cavern) =
    cavern
    |> updateLocation coord (fun c -> { c with CumulativeRiskLevel = Some newRiskLevel })
    
let markLocationChecked coord cavern =
    cavern
    |> updateLocation coord (fun c -> { c with Checked = true })

let getNeighbours (x, y) = [ x-1, y; x+1, y; x, y-1; x, y+1 ]

let updateLocationIfShorter currentCumulative (cavern: Cavern) coord =
    match cavern |> Map.tryFind coord with
    | None -> cavern
    | Some location ->
        let newRiskLevel = location.RiskLevel + currentCumulative
        match location.CumulativeRiskLevel with
        | Some previousRiskLevel when previousRiskLevel <= newRiskLevel -> cavern
        | _ -> updateCumulativeRisk coord newRiskLevel cavern

let rec traverseCavern target (cavern: Cavern) =
    let nextPositionsToCheck =
        cavern
        |> Map.toList
        |> List.filter (fun (_, l) -> not l.Checked && l.CumulativeRiskLevel.IsSome)
        |> List.sortBy (fun (_, l) -> l.CumulativeRiskLevel)
        |> List.tryHead
    match nextPositionsToCheck with
    | None -> failwithf "Ran out of paths to check before reaching target"
    | Some (nextCoord, nextLocationInfo) when nextCoord = target -> nextLocationInfo.CumulativeRiskLevel.Value
    | Some (nextCoord, nextLocationInfo) ->
        let updatedCavern =
            nextCoord
            |> getNeighbours
            |> List.fold (updateLocationIfShorter nextLocationInfo.CumulativeRiskLevel.Value) cavern
            |> markLocationChecked nextCoord
        traverseCavern target updatedCavern

let solveA input =
    let initialCavern =
        parseAll input
        |> updateCumulativeRisk (0,0) 0
    let target =
        initialCavern
        |> Map.keys
        |> Seq.maxBy (fun (x,y) -> x + y)
    initialCavern
    |> traverseCavern target

let incrementAndWrapRiskLevel v inc = (v + inc - 1) % 9 + 1

let createLargeCavern smallCavern =
    let smallCavernItems = smallCavern |> Map.toList
    let (xMax, yMax), _ = smallCavernItems |> List.maxBy (fun ((x,y), _) -> x + y)
    let cavernXStretched =
        List.init 5 (fun i ->
            smallCavernItems
            |> List.map (fun ((x,y), loc) -> (x + i*(xMax+1), y), { loc with RiskLevel = incrementAndWrapRiskLevel loc.RiskLevel i })
            )
        |> List.concat
    List.init 5 (fun i ->
        cavernXStretched
        |> List.map (fun ((x,y), loc) -> (x, y + i*(yMax+1)), { loc with RiskLevel = incrementAndWrapRiskLevel loc.RiskLevel i })
        )
    |> List.concat
    |> Map.ofList

let solveB input =
    let initialCavern =
        parseAll input
        |> createLargeCavern
        |> updateCumulativeRisk (0,0) 0
    let target =
        initialCavern
        |> Map.keys
        |> Seq.maxBy (fun (x,y) -> x + y)
    initialCavern
    |> traverseCavern target
