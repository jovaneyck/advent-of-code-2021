#r "nuget: Unquote"
open Swensen.Unquote

type Cucumber =
    | East
    | South

type CucumberMap =
    { map: Map<int * int, Cucumber>
      maxRow: int
      maxColumn: int }

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"v...>>.vv>
    .vv>>.vv..
    >>.>v>...v
    >>v>>.>.v.
    v>v.vv.v..
    >.>>..v...
    .vv..>.>v.
    v.v..>>v.v
    ....v..v.>"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let parse (text: string seq) : CucumberMap =
    let parseCucumber =
        function
        | '>' -> East
        | 'v' -> South

    let map =
        [ for (r, row) in text |> Seq.indexed do
              for (c, col) in row |> Seq.indexed do
                  if col <> '.' then
                      (r, c), parseCucumber col ]
        |> Map.ofSeq

    let maxRow = text |> Seq.length
    let maxCol = text |> Seq.head |> Seq.length

    { map = map
      maxRow = maxRow - 1
      maxColumn = maxCol - 1 }

let nextEast maxCol (r, c) = (r, (c + 1) % (maxCol + 1))
let nextSouth maxRow (r, c) = ((r + 1) % (maxRow + 1), c)

let nextLocationFor map l c =
    let next =
        match c with
        | East -> nextEast map.maxColumn l
        | South -> nextSouth map.maxRow l

    match map.map |> Map.tryFind next with
    | None -> next, c
    | Some _ -> l, c

let moveEast map =
    let east =
        map.map
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c = East)

    let south =
        map.map
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c = South)

    let next =
        east
        |> Seq.map (fun (l, c) -> nextLocationFor map l c)
        |> Seq.append south
        |> Map.ofSeq

    { map with map = next }

let moveSouth map =
    let south =
        map.map
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c = South)

    let east =
        map.map
        |> Map.toSeq
        |> Seq.filter (fun (_, c) -> c = East)

    let next =
        south
        |> Seq.map (fun (l, c) -> nextLocationFor map l c)
        |> Seq.append east
        |> Map.ofSeq

    { map with map = next }

let step map = map |> moveEast |> moveSouth

let print map =
    [ for row in 0 .. map.maxRow do
          [ for col in 0 .. map.maxColumn ->
                match map.map |> Map.tryFind (row, col) with
                | None -> "."
                | Some c ->
                    (match c with
                     | East -> ">"
                     | South -> "v") ]
          |> String.concat "" ]
    |> String.concat "\n"
    |> printfn "%s\n"

let rec fixp n map =
    printfn "%d" n
    //map |> print
    let next = step map

    if next = map then
        n
    else
        fixp (n + 1) next

let solve map = fixp 1 map
input |> parse |> solve

let run () =
    printf "Testing..."
    test <@ example |> parse |> solve = 58 @>
    printfn "...done!"

run ()
