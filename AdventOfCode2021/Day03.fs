module Day03

open Common

let getInput () = readLines "Inputs/03.txt"

let getBinaryDigitAtPosition number position =
    (number >>> position) &&& 1

let binaryDigitsToList numberOfDigits number =
    List.init numberOfDigits id
    |> List.map (getBinaryDigitAtPosition number)

let listToBinaryNumber threshold digitList =
    digitList
    |> List.indexed
    |> List.sumBy (fun (i, value) -> if value >= threshold then 1 <<< i else 0)

let invertBinaryNumber numberDigits number =
    (~~~ number) &&& (1 <<< numberDigits) - 1

let sumDigitLists = List.map2 (+)

let solveA (input: string seq) =
    let totalDigits = input |> Seq.head |> String.length
    let inputLength = input |> Seq.length
    
    let digitCounts = input
                    |> Seq.map binaryStringToInt
                    |> Seq.map (binaryDigitsToList totalDigits)
                    |> Seq.reduce sumDigitLists
    let gammaNumber = listToBinaryNumber (inputLength / 2) digitCounts
    let epsilonRate = invertBinaryNumber totalDigits gammaNumber
    gammaNumber * epsilonRate
    
let findMatchingRating (condition: int -> int -> bool) (digitLists: int list seq) =
    
    let getMostCommonDigits digitLists =
        let numbersRemaining = digitLists |> Seq.length
        digitLists
        |> Seq.reduce sumDigitLists
        |> List.map (fun x -> if x >= (numbersRemaining + 1) / 2 then 1 else 0)
        
    let firstIndex = (digitLists |> Seq.head |> List.length) - 1
    let rec findMatchingRatingAtIndex condition currentIndex digitLists =
        let mostCommonDigits = getMostCommonDigits digitLists
        let thisDigitCondition = mostCommonDigits |> List.item currentIndex |> condition
        let matchingDigitLists =
            digitLists
            |> Seq.filter (List.item currentIndex >> thisDigitCondition)
        match Seq.tryExactlyOne matchingDigitLists with
        | Some singleElement -> singleElement
        | None -> findMatchingRatingAtIndex condition (currentIndex - 1) matchingDigitLists
    findMatchingRatingAtIndex condition firstIndex digitLists

let solveB (input: string seq) =
    let totalDigits = input |> Seq.head |> String.length
    
    let digitLists =
        input
        |> Seq.map binaryStringToInt
        |> Seq.map (binaryDigitsToList totalDigits)
    let oxygenGeneratorRating =
        findMatchingRating (=) digitLists
        |> listToBinaryNumber 1
    let co2ScrubberRating =
        findMatchingRating (<>) digitLists
        |> listToBinaryNumber 1
    oxygenGeneratorRating * co2ScrubberRating