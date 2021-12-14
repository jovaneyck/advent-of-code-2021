module App

let example =
    @"5483143223
    2745854711
    5264556173
    6141336146
    6357385478
    4167524645
    2176841721
    6882881134
    4846848554
    5283751526"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

open System
open Elmish

type Msg = Tick of DateTime

type Model =
    { time: DateTime
      generation: int
      grid: Octopods.Grid }

let init () =
    { time = DateTime.Now
      generation = 0
      grid = example |> Octopods.parse },
    Cmd.none

let tickGrid model =
    if model.grid |> Octopods.allFlashed then
        model
    else
        { model with
              generation = model.generation + 1
              grid = Octopods.takeStep model.grid }

let update (msg: Msg) model =
    match msg with
    | Tick timestamp ->
        model
        |> tickGrid
        |> fun m -> { m with time = timestamp }, Cmd.none

open Fable.React
open Fable.React.Props

let renderGrid (g: Octopods.Grid) =
    let maxX =
        g
        |> Map.toSeq
        |> Seq.maxBy (fst >> fst)
        |> fst
        |> fst

    let maxY =
        g
        |> Map.toSeq
        |> Seq.maxBy (fst >> snd)
        |> fst
        |> snd

    [ for x in 0 .. maxX do
          yield!
              [ for y in 0 .. maxY ->
                    let energyLevel = g |> Map.find (x, y)

                    span [ Style [ Color "yellow"
                                   Opacity(
                                       if energyLevel = 0 then
                                           1m
                                       else
                                           decimal energyLevel / 10m
                                   ) ] ] [
                        str (energyLevel |> string)
                    ] ]

          yield br [] ]

let view model dispatch : ReactElement =
    div [] [
        h3 [] [str "Animated octopods 🦑 v2021.12.11"]
        //br []
        str "Made with #fable, #elmish and lots of #adventofcode ♥"
        br []
        str "Source code available on "
        a [ Href "https://github.com/jovaneyck/advent-of-code-2021/tree/main/day%2011/animatedoctopods" ] [
            str "github"
        ]
        br []
        str "Generation:"
        str <| string model.generation
        br []
        str <| sprintf "%A" model.time
        br []
        br []
        div [] (renderGrid model.grid)
    ]

open Elmish.React
open Browser

let subscription model =
    let sub dispatch =
        window.setInterval ((fun _ -> dispatch (Tick System.DateTime.Now)), 100)
        |> ignore

    Cmd.ofSub sub

Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.withSubscription subscription
|> Program.run
