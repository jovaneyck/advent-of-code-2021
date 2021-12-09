let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"2199943210
    3987894921
    9856789892
    8767896789
    9899965678"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

type HeightMap = Map<int * int, int>

let parse input : HeightMap =
    [ for (row, line) in input |> Seq.indexed do
          for (column, digit) in line |> Seq.indexed -> (row, column), digit |> string |> int ]
    |> Map.ofSeq

let heightmap = parse input

let findNeighbours heightmap (x, y) =
    [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
    |> Seq.map (fun (dx, dy) -> (x + dx, y + dy))
    |> Seq.choose (fun location -> heightmap |> Map.tryFind location)

let locations = heightmap |> Map.toSeq

let isLowPoint (value, neighbourValues) = value < (neighbourValues |> Seq.min)
let riskLevel = (+) 1

let sumOfRiskLevels =
    locations
    |> Seq.map (fun (loc, value) -> value, findNeighbours heightmap loc)
    |> Seq.filter isLowPoint
    |> Seq.map fst
    |> Seq.map riskLevel
    |> Seq.sum

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
