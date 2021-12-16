//Dijkstra
//Basic approach using an unsorted list as queue is pretty slow, even for part 1
//Let's rework using a priority queue!

#r "nuget: Fsharpx.Collections"
open FSharpx.Collections

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

let grow cave =
    let scale = 2
    let dimension = (cave |> Array2D.length1) 
    let initializer row col = (cave.[row % scale,col % scale] + row + col) % 9
    Array2D.init (scale*dimension) (scale*dimension) initializer

array2D [[1;2];[3;4]] |> grow

let neighbours cave (x, y) =
    [ if x > 0 then yield (x - 1, y)
      if x < ((cave |> Array2D.length1) - 1) then
          yield (x + 1, y)
      if y > 0 then yield (x, y - 1)
      if y < ((cave |> Array2D.length1) - 1) then
          yield (x, y + 1) ]


let updateWorkingList workingList (location, distance) =
    workingList
    |> Heap.insert (distance, location)

let rec shortestPaths cave target (workingList : Heap<int*(int*int)>) visited =
    //printfn "We still have to process %d locations" (workingList |> Heap.length)


    let distanceCurrentLocation, currentLocation =
        workingList
        |> Heap.head

    if currentLocation = target then //Stop, we're only interested in shortest path to target
        distanceCurrentLocation
    else
        let neighbs = 
            currentLocation
            |> neighbours cave

        let unvisitedNeighbs =
            neighbs |> List.filter (fun n -> visited |> Set.contains n |> not)

        let updatedWorkingList =            
            unvisitedNeighbs
            |> List.map (fun (x, y) -> (x, y), (distanceCurrentLocation + (cave.[x, y])))
            |> List.fold updateWorkingList (workingList |> Heap.tail)

        let newVisited = visited |> Set.union (unvisitedNeighbs |> Set.ofSeq)

        shortestPaths cave target updatedWorkingList newVisited

let solve input =
    let cave = parse input

    let dimension = (cave |> Array2D.length1) - 1

    let workingList =
        [ 0, (0,0) ] //we start at origin with risk level 0
        |> Heap.ofSeq false //Aaaand we use a sorted priorityqueue/heap

    let target = (dimension, dimension)

    let shortestDistance =
        shortestPaths cave target workingList Set.empty

    shortestDistance

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