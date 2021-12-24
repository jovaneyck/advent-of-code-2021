#r "nuget: Unquote"
open Swensen.Unquote

type Color =
    | A
    | B
    | C
    | D

type Location = int * int
type Amphopod = Color * Location
type Floorplan = Amphopod list

(*
Leftmost hallway spot is the (0,0) origin
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
*)

let hallway = [ for y in 0 .. 10 -> (0, y) ]

let plan: Floorplan =
    [ (A, (2, 2))
      (B, (2, 1))
      (C, (4, 1))
      (D, (4, 2))
      (B, (6, 1))
      (C, (6, 2))
      (D, (8, 1))
      (A, (8, 2)) ]

let nextToAnySpotIn _ (x, y) = y = 1

let nextToHallway =
    plan
    |> List.filter (fun a -> nextToAnySpotIn hallway (snd a))

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"".Split("\n") |> Array.map (fun s -> s.Trim())

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()

//8964
//19182
