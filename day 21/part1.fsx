#r "nuget: Unquote"
open Swensen.Unquote

type PlayerState = { position: int; score: int }

type Turn =
    | Player1
    | Player2

type GameState =
    { player1: PlayerState
      player2: PlayerState
      turn: Turn
      die: int seq
      nbRolls: int }

let deterministicDie =
    [ 1 .. 100 ] |> Seq.replicate 10 |> Seq.collect id

let initial pos1 pos2 =
    { player1 = { position = pos1; score = 0 }
      player2 = { position = pos2; score = 0 }
      turn = Player1
      die = deterministicDie
      nbRolls = 0 }

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

let takeTurn state =
    let nbRolls = 3
    let roll = state.die |> Seq.take nbRolls |> Seq.sum
    let nextDie = state.die |> Seq.skip nbRolls

    if state.turn = Player1 then
        let nextPosition = state.player1.position + roll |> wrap
        let nextScore = state.player1.score + nextPosition

        { state with
              player1 =
                  { position = nextPosition
                    score = nextScore }
              die = nextDie
              nbRolls = state.nbRolls + nbRolls
              turn = Player2 }
    else
        let nextPosition = state.player2.position + roll |> wrap
        let nextScore = state.player2.score + nextPosition

        { state with
              player2 =
                  { position = nextPosition
                    score = nextScore }
              die = nextDie
              nbRolls = state.nbRolls + nbRolls
              turn = Player1 }

let rec playUntilWinner (state: GameState) =
    if state.player1.score >= 1000
       || state.player2.score >= 1000 then
        state
    else
        state |> takeTurn |> playUntilWinner

let endState = input |> playUntilWinner
674 * 747

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
