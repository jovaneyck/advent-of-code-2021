#r "nuget: Unquote"
open Swensen.Unquote

type State =
    | On
    | Off

type Coordinate = int * int * int
type Grid = Map<Coordinate, State>
type Range = int * int
type Cuboid = { X: Range; Y: Range; Z: Range }

type Instruction = { command: State; cuboid: Cuboid }

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"on x=10..12,y=10..12,z=10..12
    on x=11..13,y=11..13,z=11..13
    off x=9..11,y=9..11,z=9..11
    on x=10..10,y=10..10,z=10..10"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let larger_example =
    @"on x=-20..26,y=-36..17,z=-47..7
    on x=-20..33,y=-21..23,z=-26..28
    on x=-22..28,y=-29..23,z=-38..16
    on x=-46..7,y=-6..46,z=-50..-1
    on x=-49..1,y=-3..46,z=-24..28
    on x=2..47,y=-22..22,z=-23..27
    on x=-27..23,y=-28..26,z=-21..29
    on x=-39..5,y=-6..47,z=-3..44
    on x=-30..21,y=-8..43,z=-13..34
    on x=-22..26,y=-27..20,z=-29..19
    off x=-48..-32,y=26..41,z=-47..-37
    on x=-12..35,y=6..50,z=-50..-2
    off x=-48..-32,y=-32..-16,z=-15..-5
    on x=-18..26,y=-33..15,z=-7..46
    off x=-40..-22,y=-38..-28,z=23..41
    on x=-16..35,y=-41..10,z=-47..6
    off x=-32..-23,y=11..30,z=-14..3
    on x=-49..-5,y=-3..45,z=-29..18
    off x=18..30,y=-20..-8,z=-3..13
    on x=-41..9,y=-7..43,z=-33..15
    on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
    on x=967..23432,y=45373..81175,z=27513..53682"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())


let parseInstruction (i: string) : Instruction =
    let [| cmdText; cuboidText |] = i.Split(" ")

    let parseCmd =
        function
        | "on" -> On
        | "off" -> Off

    let cmd = parseCmd cmdText

    let [| [| xMin; xMax |]; [| yMin; yMax |]; [| zMin; zMax |] |] =
        cuboidText.Split(',')
        |> Array.map (fun range -> range.Substring(2))
        |> Array.map (fun range -> range.Split("..") |> Array.map int)

    { command = cmd
      cuboid =
          { X = (xMin, xMax)
            Y = (yMin, yMax)
            Z = (zMin, zMax) } }

let emptyGrid: Grid = Map.empty

//ASSUMPTION: we can just run the entire thing in infinite space and select the 50x50x50 cuboid in the end, no performance hit yay!

let allCoordinates (cuboid: Cuboid) : Coordinate list =
    [ for x in (fst cuboid.X) .. (snd cuboid.X) do
          for y in (fst cuboid.Y) .. (snd cuboid.Y) do
              for z in (fst cuboid.Z) .. (snd cuboid.Z) -> (x, y, z) ]

let updateGrid grid (loc, state) = grid |> Map.add loc state

let applyInstruction grid instruction =
    let updates =
        instruction.cuboid
        |> allCoordinates
        |> Seq.map (fun c -> c, instruction.command)

    updates |> Seq.fold updateGrid grid

let solve input =
    let instructions = input |> Array.map parseInstruction

    let grid =
        instructions
        |> Array.fold applyInstruction emptyGrid

    let inRange (min, max) pt = min <= pt && pt <= max

    let onCount =
        grid
        |> Map.filter (fun (x, y, z) _ -> [ x; y; z ] |> List.forall (inRange (-50, 50)))
        |> Map.filter (fun _ state -> state = On)
        |> Seq.length

    onCount

let run () =
    printf "Testing..."

    test
        <@ "on x=1..2,y=3..4,z=5..6666" |> parseInstruction = { command = On
                                                                cuboid =
                                                                    { X = (1, 2)
                                                                      Y = (3, 4)
                                                                      Z = (5, 6666) } } @>

    test <@ solve example = 39 @>
    printfn "...done!"

run ()
