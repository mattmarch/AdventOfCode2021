open System.Diagnostics

type SelectedDay =
    | AllDays
    | SingleDay of int

let printDayIfChosen chosenDay number solveA solveB getInput =
    match chosenDay with
    | SingleDay d when number <> d -> ()
    | _ ->
        let stopWatch = Stopwatch.StartNew()
        let input = getInput ()
        let partA = solveA input
        let partB = solveB input
        let elapsedMs = stopWatch.Elapsed.TotalMilliseconds
        printfn $"Day %i{number}\t part 1 = %15O{partA}\t\t part 2 = %15O{partB}\t\t in %f{elapsedMs}ms"

[<EntryPoint>]
let main argv =
    let dayToRun =
        if argv.Length > 0 then
            SingleDay (argv.[0] |> int)
        else
            AllDays
    let printDay number solveA solveB getInput = printDayIfChosen dayToRun number solveA solveB getInput
    
    printDay 1 Day01.solveA Day01.solveB Day01.getInput
    printDay 2 Day02.solveA Day02.solveB Day02.getInput
    printDay 3 Day03.solveA Day03.solveB Day03.getInput
    printDay 4 Day04.solveA Day04.solveB Day04.getInput
    printDay 5 Day05.solveA Day05.solveB Day05.getInput
    printDay 6 Day06.solveA Day06.solveB Day06.getInput
    printDay 7 Day07.solveA Day07.solveB Day07.getInput
    printDay 8 Day08.solveA Day08.solveB Day08.getInput
    printDay 9 Day09.solveA Day09.solveB Day09.getInput
    printDay 10 Day10.solveA Day10.solveB Day10.getInput
    printDay 11 Day11.solveA Day11.solveB Day11.getInput
    printDay 12 Day12.solveA Day12.solveB Day12.getInput
    printDay 13 Day13.solveA Day13.solveB Day13.getInput
    printDay 14 Day14.solveA Day14.solveB Day14.getInput
    printDay 15 Day15.solveA Day15.solveB Day15.getInput
    printDay 16 Day16.solveA Day16.solveB Day16.getInput
    printDay 17 Day17.solveA Day17.solveB Day17.getInput
    printDay 18 Day18.solveA Day18.solveB Day18.getInput
    printDay 19 Day19.solveA Day19.solveB Day19.getInput
    printDay 20 Day20.solveA Day20.solveB Day20.getInput
    printDay 21 Day21.solveA Day21.solveB Day21.getInput
    printDay 22 Day22.solveA Day22.solveB Day22.getInput
    printDay 23 Day23.solveA Day23.solveB Day23.getInput
    
    0 