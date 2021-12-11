let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"5483143223
    2745854711
    5264556173
    6141336146
    6357385478
    4167524645
    2176841721
    6882881134
    4846848554
    5283751526"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

type Location = (int * int)
type EnergyLevel = int
type Grid = Map<Location, EnergyLevel>

let parse (lines: string seq) : Grid =
    [ for r, line in lines |> Seq.indexed do
          for c, cell in line |> Seq.indexed -> ((r, c), cell |> string |> int) ]
    |> Map.ofSeq

let findNeighbours grid (r, c) =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]
    |> Seq.map (fun (dr, dc) -> (r + dr, c + dc))
    |> Seq.filter (fun loc -> grid |> Map.containsKey loc)
    |> Seq.toList

let increaseEnergyLevelAt (grid: Grid) (loc: Location) : Grid =
    let currentLevel = grid |> Map.find loc
    grid |> Map.add loc (currentLevel + 1)

let takeStep grid =
    let rec resolveFlashes nbFlashes highlighted grid =
        let nextOctupusToFlash =
            grid
            |> Map.toList
            |> List.filter (fun (loc, level) -> highlighted |> Set.contains loc |> not)
            |> List.tryFind (fun (_, v) -> v > 9)
        //printfn "flashing: %A" nextOctupusToFlash
        match nextOctupusToFlash with
        | None ->
            //Done with this step!
            let resetGrid =
                grid
                |> Map.map
                    (fun location level ->
                        if highlighted |> Set.contains location then
                            0
                        else
                            level)

            resetGrid, nbFlashes
        | Some (loc, _) ->
            //An octopus flashes!
            let neighbours = findNeighbours grid loc

            let gridWithIncreasedNeighbours =
                neighbours |> List.fold increaseEnergyLevelAt grid

            resolveFlashes (nbFlashes + 1) (highlighted |> Set.add loc) gridWithIncreasedNeighbours

    grid
    |> Map.map (fun _ level -> level + 1)
    |> resolveFlashes 0 Set.empty

let rec repeat n nbFlashes grid =
    if n = 0 then
        nbFlashes
    else
        let nextGrid, nnbFlashes = grid |> takeStep
        repeat (n - 1) (nbFlashes + nnbFlashes) nextGrid

let grid = parse input

repeat 100 0 grid


#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
