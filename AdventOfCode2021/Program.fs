let printDay number solveA solveB getInput =
    let input = getInput ()
    printfn "Day %i, part A: %i, part B %i" number (solveA input) (solveB input)

printDay 1 Day01.solveA Day01.solveB Day01.getInput
printDay 2 Day02.solveA Day02.solveB Day02.getInput
