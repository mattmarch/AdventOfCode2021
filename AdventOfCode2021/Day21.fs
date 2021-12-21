module Day21

open System.Collections.Generic
open Common

type GameState = {
    Turn: int
    P1Position: int
    P2Position: int
    P1Score: int
    P2Score: int
}

let getInput () = readLines "Inputs/21.txt"

let parseLine line =
    line
    |> splitBy " "
    |> List.last
    |> int
    
let getDiceRolls turn =
    [ 0; 1; 2 ]
    |> List.map (fun v -> (turn * 3 + v) % 100 + 1)

let movePosition start moveBy =
    (start + moveBy - 1) % 10 + 1 

let rec playDice gameState =
    let diceScore = getDiceRolls gameState.Turn |> List.sum
    let nextGameState =
        match gameState.Turn % 2 = 0 with
        | true ->
            let nextPosition = movePosition gameState.P1Position diceScore
            { gameState with
                P1Position = nextPosition
                P1Score = gameState.P1Score + nextPosition
                Turn = gameState.Turn + 1
            }
        | false ->
            let nextPosition = movePosition gameState.P2Position diceScore
            { gameState with
                P2Position = nextPosition
                P2Score = gameState.P2Score + nextPosition
                Turn = gameState.Turn + 1
            }
    if nextGameState.P1Score >= 1000 || nextGameState.P2Score >= 1000 then
        nextGameState
    else
        playDice nextGameState

let solveA input =
    let p1Start, p2Start =
        input |> Seq.map parseLine |> Seq.toList |> unpack2
    let initialGameState = {
        Turn = 0; P1Position = p1Start; P2Position = p2Start; P1Score = 0; P2Score = 0;
    }
    let finalGameState = playDice initialGameState
    let losingPlayerScore = List.min [ finalGameState.P1Score; finalGameState.P2Score ]
    let numberDieRolls = finalGameState.Turn * 3
    losingPlayerScore * numberDieRolls
    
type Turn = P1 | P2
    
type QuantumGameState = {
    P1Position: int
    P2Position: int
    P1Score: int
    P2Score: int
    Turn: Turn
}

let allQuantumDiceScores =
    let singleDieRolls = [ 1; 2; 3 ]
    List.allPairs singleDieRolls singleDieRolls
    |> List.allPairs singleDieRolls
    |> List.map (fun (a, (b, c)) -> a + b + c)

let playQuantumDice (gameState: QuantumGameState) =
    let cache = Dictionary<QuantumGameState, int64*int64>()
    let rec playNextStepsCached gameState =
        match cache.TryGetValue gameState with
        | true, v -> v
        | false, _ ->
            let v = playNextSteps gameState
            cache.Add (gameState, v)
            v
    and playNextSteps gameState =     
        let remainingGames, p1Wins, p2Wins =
            match gameState.Turn with
            | P1 ->
                let unwonGames, p1WonGames =
                    allQuantumDiceScores
                    |> List.map (fun score ->
                        let nextPosition = movePosition gameState.P1Position score
                        { gameState with
                               P1Position = nextPosition
                               P1Score = gameState.P1Score + nextPosition
                               Turn = P2
                        })
                    |> List.partition (fun g -> g.P1Score < 21)
                unwonGames, List.length p1WonGames |> int64, 0L
               
            | P2 ->
               let unwonGames, p2WonGames =
                   allQuantumDiceScores
                   |> List.map (fun score ->
                       let nextPosition = movePosition gameState.P2Position score
                       { gameState with
                               P2Position = nextPosition
                               P2Score = gameState.P2Score + nextPosition
                               Turn = P1
                       })
                   |> List.partition (fun g -> g.P2Score < 21)
               unwonGames, 0L, List.length p2WonGames |> int64
        let p1WinsInRemainingGames, p2WinsInRemainingGames =
           remainingGames
            |> List.map playNextStepsCached
            |> List.fold (fun (aTotal, bTotal) (a, b) -> aTotal + a, bTotal + b) (0L, 0L)
        p1Wins + p1WinsInRemainingGames, p2Wins + p2WinsInRemainingGames
    playNextStepsCached gameState

let solveB input =
    let p1Start, p2Start =
        input |> Seq.map parseLine |> Seq.toList |> unpack2
    let initialGameState = {
        Turn = P1; P1Position = p1Start; P2Position = p2Start; P1Score = 0; P2Score = 0;
    }
    let p1Wins, p2Wins = playQuantumDice initialGameState
    List.max [ p1Wins; p2Wins ]