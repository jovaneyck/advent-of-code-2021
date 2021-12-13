module App

open Elmish
type Msg = AMessage

let init () = "Hello world from elmish-react"
let update (msg: Msg) model = model + "-click"

open Fable.React
open Fable.React.Props

let view model dispatch : ReactElement =
    div [] [
        button [ OnClick(fun _ -> dispatch AMessage) ] [
            str "clicky"
        ]
        str model
    ]

open Elmish.React

Program.mkSimple init update view
|> Program.withReactBatched "elmish-app"
|> Program.run
