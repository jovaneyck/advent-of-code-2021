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
    |> Seq.choose
        (fun location ->
            heightmap
            |> Map.tryFind location
            |> Option.map (fun value -> location, value))

let isLowPoint value neighbourValues =
    value < (neighbourValues |> Seq.map snd |> Seq.min)

let locations = heightmap |> Map.toSeq

let lowPoints =
    locations
    |> Seq.map (fun (loc, value) -> (loc, value), findNeighbours heightmap loc)
    |> Seq.filter (fun ((_, value), neighbours) -> isLowPoint value neighbours)
    |> Seq.map (fst >> fst)

type GrowingBasin =
    { locations: (int * int) list
      currentHeight: int }

let growBasin unvisited b =
    let newPartOfBasin =
        b.locations
        |> Seq.collect (findNeighbours heightmap)
        |> Seq.distinct
        |> Seq.filter (fun (loc, value) -> value = b.currentHeight + 1)
        |> Seq.filter (fun (loc, value) -> unvisited |> List.contains loc)
        |> Seq.map fst
        |> Seq.toList

    let newBasin =
        { b with
              locations = b.locations @ newPartOfBasin
              currentHeight = b.currentHeight + 1 }

    let nextUnvisited = unvisited |> List.except newPartOfBasin

    newBasin, nextUnvisited

type State =
    { basins: GrowingBasin list
      unvisited: (int * int) list }

let growState state basin =
    let nextBasin, nextUnvisited = growBasin state.unvisited basin

    { state with
          basins = nextBasin :: state.basins
          unvisited = nextUnvisited }

let rec loop basins unvisited =
    if unvisited = [] then
        basins
    else
        let nextState =
            basins
            |> Seq.fold growState { basins = []; unvisited = unvisited }

        loop nextState.basins nextState.unvisited

let basins =
    lowPoints
    |> Seq.map
        (fun lowpoint ->
            { locations = [ lowpoint ]
              currentHeight = heightmap |> Map.find lowpoint })
    |> Seq.toList

let unvisited =
    heightmap
    |> Map.toList
    |> Seq.filter (fun (loc, value) -> value <> 9)
    |> Seq.map fst
    |> Seq.except lowPoints
    |> Seq.toList

let grown =
    loop basins unvisited
    |> List.map (fun growingBasin -> growingBasin.locations)

let solution =
    grown
    |> List.map List.length
    |> List.sortDescending
    |> List.take 3
    |> List.reduce (*)
