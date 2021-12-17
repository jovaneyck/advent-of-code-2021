type Range = { Min: int; Max: int }
type Target = { X: Range; Y: Range }
type Vector = { X: int; Y: int }

type State =
    { Position: Vector
      Velocity: Vector
      HighestPoint: int }

//target area: x=14..50, y=-267..-225
let input: Target =
    { X = { Min = 14; Max = 50 }
      Y = { Min = -267; Max = -225 } }

//x=20..30, y=-10..-5
//let example: Target =
//    { X = { Min = 20; Max = 30 }
//      Y = { Min = -10; Max = -5 } }

let step state =
    let newY = state.Position.Y + state.Velocity.Y

    { Position =
          { X = state.Position.X + state.Velocity.X
            Y = newY }
      Velocity =
          { X =
                if state.Velocity.X > 0 then
                    state.Velocity.X - 1
                else
                    0
            Y = state.Velocity.Y - 1 }
      HighestPoint = [ state.HighestPoint; newY ] |> Seq.max }

let init v =
    { Position = { X = 0; Y = 0 }
      Velocity = v
      HighestPoint = 0 }

let inRange (target: Target) (position: Vector) =
    (position.X < target.X.Min
     || position.X > target.X.Max
     || position.Y < target.Y.Min
     || position.Y > target.Y.Max)
    |> not

let flewPast (target: Target) (position: Vector) =
    position.X > target.X.Max
    || position.Y < target.Y.Min

let hitsTarget (target: Target) (v: Vector) =
    let rec eval (state: State) =
        if state.Position |> inRange target then
            Some(v, state.HighestPoint)
        else if state.Position |> flewPast target then
            None
        else
            let next = step state
            eval next

    eval (init v)

let target = input

let vectors =
    [ for x in 1 .. (target.X.Max) do
          //ASSUMPTION: only positive ys are interesting (?)
          for y in 1 .. 1000 -> { X = x; Y = y } ] //LOLO why 1000? I don't know

let part1 =
    vectors
    |> List.choose (hitsTarget target)
    |> List.maxBy (fun (v, _) -> v.Y)
    |> snd

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
