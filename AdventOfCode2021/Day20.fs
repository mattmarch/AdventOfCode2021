module Day20

open Common

type Pixel = On | Off

let getInput () = readAll "Inputs/20.txt"

let matchPixel c =
    match c with
    | '#' -> On
    | '.' -> Off
    | _ -> failwithf $"Invalid input char '%c{c}'"

let parseImageLine yIndex line =
    line
    |> Seq.mapi (fun x value -> (x, yIndex), matchPixel value)

let parseAll input =
    let algorithmString, imageInput =
        input |> splitBy "\n\n" |> unpack2
    let algorithm =
        algorithmString
        |> Seq.map matchPixel
        |> Seq.toList
    let image =
        imageInput
        |> splitBy "\n"
        |> Seq.mapi parseImageLine
        |> Seq.concat
        |> Map.ofSeq
    algorithm, image

let getBoundsPlusExtra image =
    let allCoords =
        image
        |> Map.toList
        |> List.map fst
    let xCoords = allCoords |> List.map fst
    let yCoords = allCoords |> List.map snd
    List.min xCoords - 1, List.min yCoords - 1,
    List.max xCoords + 1, List.max yCoords + 1

let getValueAtCoord backgroundValue image coord =
    match image |> Map.tryFind coord with
    | Some On -> On
    | Some Off -> Off
    | None -> backgroundValue

let pixelListToBinaryNumber pixelList =
    pixelList
    |> List.rev
    |> List.indexed
    |> List.sumBy (fun (i, pixel) -> (if pixel = On then 1 else 0) <<< i)

let getNewValue backgroundValue algorithm image (x,y): Pixel =
    let algorithmIndex =
        [ x-1, y-1; x, y-1; x+1, y-1; x-1, y; x, y; x+1, y; x-1, y+1; x, y+1; x+1, y+1 ]
        |> List.map (getValueAtCoord backgroundValue image)
        |> pixelListToBinaryNumber
    algorithm |> List.item algorithmIndex

let runEnhanceIteration backgroundValue algorithm image =
    let minX, minY, maxX, maxY = getBoundsPlusExtra image
    [minY..maxY]
    |> List.map (fun y ->
        [minX..maxX]
        |> List.map (fun x -> (x, y), getNewValue backgroundValue algorithm image (x,y)))
    |> List.concat
    |> Map.ofList

let rec runMultipleEnhanceIterations backgroundValue algorithm iterationsRemaining image =
    let newImage = runEnhanceIteration backgroundValue algorithm image
    let newIterationsRemaining = iterationsRemaining - 1
    if newIterationsRemaining = 0 then
        newImage
    else
        let backgroundImagePatch = List.replicate 9 backgroundValue
        let newBackgroundValue =
            algorithm |> List.item (pixelListToBinaryNumber backgroundImagePatch)
        runMultipleEnhanceIterations newBackgroundValue algorithm newIterationsRemaining newImage

let solveA input =
    let algorithm, image = parseAll input
    runMultipleEnhanceIterations Off algorithm 2 image
    |> Map.toList
    |> List.map snd
    |> List.filter ((=) On)
    |> List.length

let solveB input =
    let algorithm, image = parseAll input
    runMultipleEnhanceIterations Off algorithm 50 image
    |> Map.toList
    |> List.map snd
    |> List.filter ((=) On)
    |> List.length