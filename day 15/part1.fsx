let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"1163751742
    1381373672
    2136511328
    3694931569
    7463417111
    1319128137
    1359912421
    3125421639
    1293138521
    2311944581"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let parse input =
    [ for row in input do
          [ for col in row -> col |> string |> int ] ]
    |> array2D

let neighbours cave (x, y) =
    [ if x > 0 then yield (x - 1, y)
      if x < ((cave |> Array2D.length1) - 1) then
          yield (x + 1, y)
      if y > 0 then yield (x, y - 1)
      if y < ((cave |> Array2D.length1) - 1) then
          yield (x, y + 1) ]


let updateBestDistance distances (location, altDistance) =
    let currentBest = distances |> Map.find location

    if altDistance < currentBest then
        distances |> Map.add location altDistance
    else
        distances

let rec shortestPaths cave target workingList distances =
    printfn "We still have to process %d locations" (workingList |> Set.count)

    if workingList |> Set.isEmpty then
        distances
    else
        let currentLocation, distanceCurrentLocation =
            workingList
            |> Set.map (fun location -> location, distances |> Map.find location)
            |> Seq.minBy snd

        if currentLocation = target then //Stop, we're only interested in shortest path to target
            distances
        else
            let updatedWorkingList =
                workingList |> Set.remove currentLocation

            let updatedDistances =
                currentLocation
                |> neighbours cave
                |> List.map (fun (x, y) -> (x, y), (distanceCurrentLocation + (cave.[x, y])))
                |> List.fold updateBestDistance distances

            shortestPaths cave target updatedWorkingList updatedDistances

let solve input =
    let cave = parse input

    let dimension = (cave |> Array2D.length1) - 1

    let workingList =
        [ for x in 0 .. dimension do
              for y in 0 .. dimension -> (x, y) ]
        |> Set.ofList

    let distances =
        workingList
        |> Seq.map
            (fun coord ->
                if coord = (0, 0) then
                    coord, 0
                else
                    coord, System.Int32.MaxValue)
        |> Map.ofSeq

    let target = (dimension, dimension)

    let shortestDistances =
        shortestPaths cave target workingList distances

    let shortestDistanceToTarget = shortestDistances |> Map.find target
    shortestDistanceToTarget

#time
//YIKES: Real: 00:04:34.873, CPU: 00:04:19.750, GC gen0: 50663, gen1: 2447, gen2: 42
//solve input

#r "nuget: Unquote"

open Swensen.Unquote

let run () =
    printf "Testing..."

    test
        <@ neighbours
            (array2D [ [ 1; 2; 3 ]
                       [ 4; 5; 6 ]
                       [ 7; 8; 9 ] ])
            (1, 1) = [ (0, 1); (2, 1); (1, 0); (1, 2) ] @>

    test
        <@ neighbours
            (array2D [ [ 1; 2; 3 ]
                       [ 4; 5; 6 ]
                       [ 7; 8; 9 ] ])
            (0, 0) = [ (1, 0); (0, 1) ] @>

    test
        <@ neighbours
            (array2D [ [ 1; 2; 3 ]
                       [ 4; 5; 6 ]
                       [ 7; 8; 9 ] ])
            (1, 0) = [ (0, 0); (2, 0); (1, 1) ] @>

    test
        <@ neighbours
            (array2D [ [ 1; 2; 3 ]
                       [ 4; 5; 6 ]
                       [ 7; 8; 9 ] ])
            (2, 2) = [ (1, 2); (2, 1) ] @>

    test <@ solve example = 40 @>

    printfn "...done!"

run ()
