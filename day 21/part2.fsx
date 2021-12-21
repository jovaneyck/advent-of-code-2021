#r "nuget: Unquote"
open Swensen.Unquote

type PlayerState = { position: int; score: int64 }

type Turn =
    | Player1
    | Player2

type GameState =
    { player1: PlayerState
      player2: PlayerState
      turn: Turn }

let initial pos1 pos2 =
    { player1 = { position = pos1; score = 0 }
      player2 = { position = pos2; score = 0 }
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

let takeTurn state =
    allOptions
    |> List.map
        (fun roll ->
            if state.turn = Player1 then
                let nextPosition = state.player1.position + roll |> wrap

                let nextScore =
                    state.player1.score + (int64 nextPosition)

                { state with
                      player1 =
                          { position = nextPosition
                            score = nextScore }
                      turn = Player2 }
            else
                let nextPosition = state.player2.position + roll |> wrap

                let nextScore =
                    state.player2.score + (int64 nextPosition)

                { state with
                      player2 =
                          { position = nextPosition
                            score = nextScore }
                      turn = Player1 })

let rec playUntilWinner (nbWinsPlayer1, nbWinsPlayer2) (states: (GameState * int64) list) =
    printfn "Working on %d different states" (states |> List.length)

    let gamesWithWinners, ongoing =
        states
        |> List.partition
            (fun (state, _) ->
                state.player1.score >= 21
                || state.player2.score >= 21)

    let player1Wins: int64 =
        gamesWithWinners
        |> List.filter (fun (s, _) -> s.player1.score >= 21)
        |> List.map (fun (_, occurrences) -> occurrences)
        |> Seq.sum
        |> int64

    let player2Wins: int64 =
        gamesWithWinners
        |> List.filter (fun (s, _) -> s.player2.score >= 21)
        |> List.map (fun (_, occurrences) -> occurrences)
        |> Seq.sum
        |> int64

    let nextWinTally =
        (nbWinsPlayer1 + player1Wins, nbWinsPlayer2 + player2Wins)

    match ongoing with
    | [] -> nextWinTally
    | _ ->
        ongoing
        |> List.collect
            (fun (state, occurrences) ->
                state
                |> takeTurn
                |> List.groupBy id
                |> List.map (fun (s, g) -> s, occurrences * (g |> List.length |> int64)))
        |> (playUntilWinner nextWinTally)

let endState =
    [ example, 1L ] |> playUntilWinner (0L, 0L)

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
