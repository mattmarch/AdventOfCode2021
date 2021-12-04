let printDay number solveA solveB getInput =
    let input = getInput ()
    printfn $"Day %i{number}, part A: %i{solveA input}, part B %i{solveB input}"

printDay 1 Day01.solveA Day01.solveB Day01.getInput
printDay 2 Day02.solveA Day02.solveB Day02.getInput
printDay 3 Day03.solveA Day03.solveB Day03.getInput
printDay 4 Day04.solveA Day04.solveB Day04.getInput
