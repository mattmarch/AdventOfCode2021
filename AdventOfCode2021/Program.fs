let printDay number solveA solveB getInput =
    let input = getInput ()
    printfn $"Day %i{number}, part A: %i{solveA input}, part B %i{solveB input}"

printDay 1 Day01.solveA Day01.solveB Day01.getInput
printDay 2 Day02.solveA Day02.solveB Day02.getInput
