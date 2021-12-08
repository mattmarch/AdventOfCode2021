module Day08

open Common

type Segment = A | B | C | D | E | F | G

let getInput () = readLines "Inputs/08.txt"

let parseSegment segment =
    match segment with
    | 'a' -> A
    | 'b' -> B
    | 'c' -> C
    | 'd' -> D
    | 'e' -> E
    | 'f' -> F
    | 'g' -> G
    | _ -> failwithf $"Unexpected character '%c{segment}' in input"

let parseDigitWires (digitWires: string) =
    digitWires
    |> Seq.map parseSegment
    |> Set.ofSeq

let parseGroupOfDigits groupOfDigits =
    groupOfDigits
    |> splitBy " "
    |> List.map parseDigitWires

let parseLine line =
    line
    |> splitBy " | "
    |> List.map parseGroupOfDigits
    |> unpack2

let uniqueSegmentNumbers = [2; 3; 4; 7]

let countUniqueSegmentNumberDigits digitList =
    digitList
    |> List.map Set.count
    |> List.filter (fun count -> uniqueSegmentNumbers |> List.contains count)
    |> List.length

let solveA input =
    input
    |> Seq.map parseLine
    |> Seq.sumBy (snd >> countUniqueSegmentNumberDigits)
    
let findDigitWithNumberSegments numberSegments (digits: Set<Segment> list) =
    digits |> List.find (fun digit -> Set.count digit = numberSegments)

let findOne = findDigitWithNumberSegments 2
let findFour = findDigitWithNumberSegments 4
let findSeven = findDigitWithNumberSegments 3
let findEight = findDigitWithNumberSegments 7

let countIntersect setA setB =
    Set.intersect setA setB |> Set.count

let isThree one digit =
    Set.count digit = 5
    && countIntersect digit one = 2
let findThree one = List.find (isThree one)

let isNine four digit =
    Set.count digit = 6
    && countIntersect digit four = 4
let findNine four = List.find (isNine four)

let isFive nine digit =
    Set.count digit = 5
    && countIntersect digit nine = 5
let findFive nine = List.find (isFive nine)

let isTwo nine digit =
    Set.count digit = 5
    && countIntersect digit nine = 4
let findTwo nine = List.find (isTwo nine)

let isZero one nine digit =
    Set.count digit = 6
    && countIntersect digit one = 2
    && countIntersect digit nine = 5
let findZero one nine = List.find (isZero one nine)

let isSix one nine digit =
    Set.count digit = 6
    && countIntersect digit one = 1
    && countIntersect digit nine = 5
let findSix one nine = List.find (isSix one nine)

let filterFoundDigits foundDigits = List.filter (fun d -> not (foundDigits |> List.contains d))

let solveDigits (digits: Set<Segment> list): Map<Set<Segment>, int> =
    let one = findOne digits
    let four = findFour digits
    let seven = findSeven digits
    let eight = findEight digits
    let digitsAfterStepOne = digits |> filterFoundDigits [ one; four; seven; eight ]
    let three = findThree one digitsAfterStepOne
    let nine = findNine four digitsAfterStepOne
    let digitsAfterStepTwo = digitsAfterStepOne |> filterFoundDigits [ three; nine ]
    let five = findFive nine digitsAfterStepTwo
    let two = findTwo nine digitsAfterStepTwo
    let zero = findZero one nine digitsAfterStepTwo
    let six = findSix one nine digitsAfterStepTwo
    Map.ofList [ zero, 0; one, 1; two, 2; three, 3; four, 4; five, 5; six, 6; seven, 7; eight, 8; nine, 9 ]

let readNumber digitMap digits =
    digits
    |> List.rev
    |> List.indexed
    |> List.sumBy (fun (i, digit) -> (pown 10 i) * (digitMap |> Map.find digit))

let solveLine (allDigits, displayedDigits) =
    let digitMap = solveDigits allDigits
    readNumber digitMap displayedDigits

let solveB input =
    input
    |> Seq.map parseLine
    |> Seq.sumBy solveLine
