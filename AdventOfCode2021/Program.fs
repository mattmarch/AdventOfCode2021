let printDay number solveA solveB getInput =
    let input = getInput ()
    printfn $"Day %i{number}, part A: %A{solveA input}, part B %A{solveB input}"

printDay 1 Day01.solveA Day01.solveB Day01.getInput
printDay 2 Day02.solveA Day02.solveB Day02.getInput
printDay 3 Day03.solveA Day03.solveB Day03.getInput
printDay 4 Day04.solveA Day04.solveB Day04.getInput
printDay 5 Day05.solveA Day05.solveB Day05.getInput
printDay 6 Day06.solveA Day06.solveB Day06.getInput
printDay 7 Day07.solveA Day07.solveB Day07.getInput
printDay 8 Day08.solveA Day08.solveB Day08.getInput
