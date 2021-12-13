module App

open System
open Elmish

type Msg =
    | ButtonClick
    | Tick of DateTime

type Model = { message: string; time: DateTime }

let init () =
    { message = "Hello world from elmish-react"
      time = DateTime.Now },
    Cmd.none

let update (msg: Msg) model =
    match msg with
    | ButtonClick ->
        { model with
              message = model.message + "-clicked!" },
        Cmd.none
    | Tick timestamp -> { model with time = timestamp }, Cmd.none

open Fable.React
open Fable.React.Props

let view model dispatch : ReactElement =
    div [] [
        str model.message
        br []
        str <| sprintf "%A" model.time
        br []
        button [ OnClick(fun _ -> dispatch ButtonClick) ] [
            str "clicky"
        ]
    ]

open Elmish.React
open Browser

let subscription model =
    let sub dispatch =
        window.setInterval ((fun _ -> dispatch (Tick System.DateTime.Now)), 1000)
        |> ignore

    Cmd.ofSub sub

Program.mkProgram init update view
|> Program.withReactBatched "elmish-app"
|> Program.withSubscription subscription
|> Program.run
