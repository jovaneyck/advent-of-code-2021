#r "nuget: Unquote"
open Swensen.Unquote

type PlayerState = { position: int; score: int64 }

type Turn =
    | Player1
    | Player2

type GameState =
    { player1: int
      player2: int
      turn: Turn }

let initial pos1 pos2 =
    { player1 = pos1
      player2 = pos2
      turn = Player1 }

(*
Player 1 starting position: 4
Player 2 starting position: 8
*)
let example = initial 4 8

(*
Player 1 starting position: 8
Player 2 starting position: 6
*)
let input = initial 8 6

let wrap pos =
    if pos < 10 then
        pos
    else
        let next = pos - (10 * (pos / 10))
        if next = 0 then 10 else next

let allOptions =
    List.allPairs (List.allPairs [ 1; 2; 3 ] [ 1; 2; 3 ]) [ 1; 2; 3 ]
    |> List.map (fun ((a, b), c) -> a, b, c)
    |> List.map (fun (a, b, c) -> a + b + c)

//Takes all possible turns starting from a given state
let takeTurn
    (
        state: GameState,
        scores: (int64 * int64) list
    ) : (GameState * (int64 * int64) list) list =
    allOptions
    |> List.map
        (fun roll ->
            if state.turn = Player1 then
                let nextPosition = state.player1 + roll |> wrap

                { state with
                      player1 = nextPosition
                      turn = Player2 },
                scores
                |> List.map (fun s -> (fst s + int64 nextPosition, snd s))

            else
                let nextPosition = state.player2 + roll |> wrap

                { state with
                      player2 = nextPosition
                      turn = Player1 },
                scores
                |> List.map (fun s -> (fst s, snd s + int64 nextPosition)))

let rec playAllGames (totalWins1, totalWins2) (states: (GameState * ((int64 * int64) list)) list) =
    printfn "Working on %d different states" (states |> List.length)
    //states |> Seq.map fst |> Seq.iter (printfn "%A")

    let gamesWithWinners =
        states
        |> List.map
            (fun (state, scores) ->
                state,
                scores
                |> List.filter (fun score -> fst score >= 21 || snd score >= 21))
        |> List.filter (fun (_, scores) -> scores |> Seq.length > 0)
        |> List.map (fun (state, scores) -> scores |> List.map (fun s -> state, s))
        |> List.collect id

    let newPlayer1Wins: int64 =
        gamesWithWinners
        |> List.filter (fun (_, scores) -> fst scores >= 21)
        |> List.map (fun (_, scores) -> fst scores)
        |> Seq.sum
        |> int64

    let newPlayer2Wins: int64 =
        gamesWithWinners
        |> List.filter (fun (_, scores) -> snd scores >= 21)
        |> List.map (fun (_, scores) -> snd scores)
        |> Seq.sum
        |> int64

    let nextWinTally =
        (totalWins1 + newPlayer1Wins, totalWins2 + newPlayer2Wins)

    let winsPerState =
        gamesWithWinners
        |> List.groupBy fst
        |> List.map (fun (s, g) -> (s, g |> List.map snd))
        |> Map.ofList

    let ongoing =
        states
        |> List.map
            (fun (s, scores) ->
                let wins = winsPerState |> Map.tryFind s

                match wins with
                | Some wins ->
                    let newScores = scores |> List.except wins
                    (s, newScores)
                | None -> (s, scores))
        |> List.filter (fun (s, scores) -> scores |> Seq.length > 0)

    match ongoing with
    | [] -> nextWinTally
    | _ ->
        ongoing
        |> List.collect (fun (state, scores) -> takeTurn (state, scores))
        |> List.groupBy fst
        |> List.map (fun (s, group) -> s, (group |> List.collect snd))
        |> playAllGames nextWinTally

let endState =
    [ example, [ (0L, 0L) ] ] |> playAllGames (0L, 0L)

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
