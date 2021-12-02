let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"forward 5
down 5
forward 8
up 3
down 8
forward 2"
        .Split("\n")

type Command =
    | Forward of int
    | Down of int
    | Up of int

let parse (commandText: string) =
    let [| rawCommand; rawDistance |] = commandText.Split(" ")
    let distance = rawDistance |> int

    let command =
        match rawCommand with
        | "forward" -> Forward
        | "up" -> Up
        | "down" -> Down

    command distance

type SubmarineState =
    { HorizontalPosition: int
      Depth: int
      Aim: int }

let initialState =
    { HorizontalPosition = 0
      Depth = 0
      Aim = 0 }

let applyCommand state command =
    match command with
    | Forward d ->
        { state with
              HorizontalPosition = state.HorizontalPosition + d
              Depth = state.Depth + (state.Aim * d) }
    | Down d -> { state with Aim = state.Aim + d }
    | Up d -> { state with Aim = state.Aim - d }

let solve commands =
    commands
    |> Seq.map parse
    |> Seq.fold applyCommand initialState


#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ parse "forward 3" = Forward 3 @>
    test <@ parse "up 2" = Up 2 @>
    test <@ parse "down 13" = Down 13 @>

    test
        <@ solve example = { HorizontalPosition = 15
                             Depth = 60
                             Aim = 10 } @>

    printfn "...done!"

run ()

let endState = input |> solve
endState.HorizontalPosition * endState.Depth
