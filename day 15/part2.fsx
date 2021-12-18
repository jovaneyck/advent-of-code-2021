//Dijkstra
//Basic approach using an unsorted list as queue is pretty slow, even for part 1
//Let's rework using a priority queue!
#r "nuget: Fsharpx.Collections"
#r "nuget: Unquote"

open FSharpx.Collections
open Swensen.Unquote

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


let updateWorkingList workingList (location, distance) =
    workingList |> Heap.insert (distance, location)

let rec shortestPaths cave target (workingList: Heap<int * (int * int)>) visited =
    //printfn "We still have to process %d locations" (workingList |> Heap.length)


    let distanceCurrentLocation, currentLocation = workingList |> Heap.head

    if currentLocation = target then //Stop, we're only interested in shortest path to target
        distanceCurrentLocation
    else
        let neighbs = currentLocation |> neighbours cave

        let unvisitedNeighbs =
            neighbs
            |> List.filter (fun n -> visited |> Set.contains n |> not)

        let updatedWorkingList =
            unvisitedNeighbs
            |> List.map (fun (x, y) -> (x, y), (distanceCurrentLocation + (cave.[x, y])))
            |> List.fold updateWorkingList (workingList |> Heap.tail)

        let newVisited =
            visited
            |> Set.union (unvisitedNeighbs |> Set.ofSeq)

        shortestPaths cave target updatedWorkingList newVisited

let grow cave =

    let merge (nested: int [,] [,]) : int [,] =
        let outerDim = Array2D.length1 nested
        let innerDim = Array2D.length1 nested.[0, 0]

        [ for outer in 0 .. outerDim - 1 do
              for inner in 0 .. innerDim - 1 ->
                  nested.[outer, 0..]
                  |> Seq.collect (fun m -> m.[inner, 0..])
                  |> Seq.toList ]
        |> array2D

    let offsets =
        [ [ 0; 1; 2; 3; 4 ]
          [ 1; 2; 3; 4; 5 ]
          [ 2; 3; 4; 5; 6 ]
          [ 3; 4; 5; 6; 7 ]
          [ 4; 5; 6; 7; 8 ] ]
        |> array2D

    let wrap number =
        if number > 9 then
            number - 9
        else
            number

    offsets
    |> Array2D.map
        (fun offset ->
            cave
            |> Array2D.map (fun el -> offset + el |> wrap))
    |> merge

let solve input =
    let cave = input |> parse |> grow

    let dimension = (cave |> Array2D.length1) - 1

    let workingList =
        [ 0, (0, 0) ] //we start at origin with risk level 0
        |> Heap.ofSeq false //Aaaand we use a sorted priorityqueue/heap

    let target = (dimension, dimension)

    let shortestDistance =
        shortestPaths cave target workingList Set.empty

    shortestDistance

//#time
//Real: 00:00:00.105, CPU: 00:00:00.109, GC gen0: 38, gen1: 0, gen2: 0
//solve input

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

    test <@ solve example = 315 @>

    test
        <@ [ [ 8 ] ] |> array2D |> grow = ([ [ 8; 9; 1; 2; 3 ]
                                             [ 9; 1; 2; 3; 4 ]
                                             [ 1; 2; 3; 4; 5 ]
                                             [ 2; 3; 4; 5; 6 ]
                                             [ 3; 4; 5; 6; 7 ] ]
                                           |> array2D) @>

    printfn "...done!"

run ()

solve input
