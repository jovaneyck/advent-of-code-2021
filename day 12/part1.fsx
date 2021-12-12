let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"start-A
    start-b
    A-c
    A-b
    b-d
    A-end
    b-end"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

type Cave =
    | Small of string
    | Big of string

let parseCave (cave: string) =
    if cave.[0] |> System.Char.IsUpper then
        Big cave
    else
        Small cave

let parseConnection (connection: string) =
    let [| one; other |] = connection.Split('-')
    (parseCave one, parseCave other)

let connections = input |> Array.map parseConnection

//let caves =
//    connections
//    |> Array.collect (fun (one, other) -> [| one; other |])
//    |> Array.distinct

let lookup =
    connections
    |> Array.append (
        connections
        |> Array.map (fun (fst, snd) -> (snd, fst))
    )
    |> Array.groupBy fst
    |> Array.map (fun (from, toes) -> (from, toes |> Array.map snd))
    |> Map.ofArray

let appendSmallCave cave visitedCaves =
    match cave with
    | Big _ -> visitedCaves
    | Small _ -> visitedCaves |> Set.add cave

let rec findPathsFrom path smallCavesVisited =

    let currentCave = path |> Seq.head

    if currentCave = Small "end" then
        [| path |]
    else
        let neighbours =
            lookup
            |> Map.find currentCave
            |> Array.filter (fun n -> smallCavesVisited |> Set.contains n |> not)

        if neighbours |> Seq.length = 0 then
            [||]
        else
            neighbours
            |> Array.collect (fun n -> findPathsFrom (n :: path) (smallCavesVisited |> appendSmallCave n))

let currentPath = [ Small "start" ]
let smallCavesVisited = Small "start" |> Set.singleton

let paths =
    findPathsFrom currentPath smallCavesVisited
    |> Array.distinct
    |> Array.map List.rev

paths |> Seq.length
//|> Array.map
//    (fun path ->
//        path
//        |> Seq.map
//            (function
//            | Big c -> c
//            | Small c -> c)
//        |> String.concat ",")
//|> Seq.iter (printfn "%A")

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
