module Octopods

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
    let rec resolveFlashes highlighted grid =
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

            resetGrid
        | Some (loc, _) ->
            //An octopus flashes!
            let neighbours = findNeighbours grid loc

            let gridWithIncreasedNeighbours =
                neighbours |> List.fold increaseEnergyLevelAt grid

            resolveFlashes (highlighted |> Set.add loc) gridWithIncreasedNeighbours

    grid
    |> Map.map (fun _ level -> level + 1)
    |> resolveFlashes Set.empty

let allFlashed (grid: Grid) =
    grid |> Map.forall (fun _ level -> level = 0)
