#r "nuget: Unquote"
open Swensen.Unquote

type State =
    | On
    | Off

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

//Let's play in 1/2/3D space to get a feel for geometry algorithms!
module Lines =
    type T = int * int

    let intersect ((a, b): T) ((c, d): T) : bool = c <= b && a <= d

    //Generates all point to subdivide line (a,b) given points c and d if they fall on the line.
    let divide ((a, b): T) ((c, d): T) =
        [ yield a
          yield b
          if a <= c && c <= b then yield c
          if a <= d && d <= b then yield d ]
        |> Seq.sort
        |> Set.ofSeq
        |> Seq.pairwise

    let add one other =
        if intersect one other |> not then
            [ one; other ] |> Set.ofSeq
        else
            let oneParts = divide one other
            let otherParts = divide other one

            oneParts
            |> Seq.append otherParts
            |> Set.ofSeq
            |> Seq.rev
            |> Seq.mapi (fun i (x, y) -> if i = 0 then (x, y) else (x, y - 1)) //transform to exclusive ranges
            |> Set.ofSeq

    let subtract (one: T) (other: T) : Set<T> =
        if intersect one other |> not then
            Set.singleton one
        else
            let oneSplitPoint = divide one other |> Set.ofSeq
            let otherSplitPoints = divide other one |> Set.ofSeq

            Set.difference oneSplitPoint otherSplitPoints

    let runTests () =
        test <@ intersect (1, 2) (3, 4) |> not @>
        test <@ intersect (1, 2) (2, 4) @>
        test <@ intersect (1, 3) (2, 4) @>
        test <@ intersect (2, 4) (1, 3) @>
        test <@ intersect (1, 5) (2, 3) @>
        test <@ intersect (2, 3) (1, 6) @>
        test <@ add (1, 3) (4, 5) = ([ 1, 3; 4, 5 ] |> Set.ofSeq) @>
        test <@ add (1, 5) (3, 6) = ([ 1, 2; 3, 4; 5, 6 ] |> Set.ofSeq) @>
        test <@ add (3, 6) (1, 5) = ([ 1, 2; 3, 4; 5, 6 ] |> Set.ofSeq) @>
        test <@ subtract (1, 10) (2, 5) = ([ 1, 1; 6, 10 ] |> Set.ofSeq) @>
        test <@ subtract (1, 10) (8, 12) = ([ 1, 7 ] |> Set.ofSeq) @>
        test <@ subtract (2, 10) (1, 5) = ([ 6, 10 ] |> Set.ofSeq) @>

module Cuboids =
    type T = Lines.T * Lines.T * Lines.T

    let intersect (one: T) (other: T) : bool =
        let a, b, c = one
        let d, e, f = other

        Lines.intersect a d
        && Lines.intersect b e
        && Lines.intersect c f

    let divide ((a, b, c): T) ((d, e, f): T) =
        let xSplits = Lines.divide a d
        let ySplits = Lines.divide b e
        let zSplits = Lines.divide c f

        [ for xSegment in xSplits do
              for ySegment in ySplits do
                  for zSegment in zSplits -> xSegment, ySegment, zSegment ]


    let add (one: T) (other: T) : Set<T> =
        if intersect one other |> not then
            [ one; other ] |> Set.ofSeq
        else
            let oneSplits = divide one other
            let otherSplits = divide other one

            (oneSplits @ otherSplits) |> Set.ofSeq

    let subtract (one: T) (other: T) : Set<T> =
        if intersect one other |> not then
            Set.singleton one
        else
            let oneSplits = divide one other |> Set.ofSeq
            let otherSplits = divide other one |> Set.ofSeq

            Set.difference oneSplits otherSplits

    let volume (((xmin, xmax), (ymin, ymax), (zmin, zmax)): T) : int64 =
        (1L + int64 xmax - int64 xmin)
        * (1L + int64 ymax - int64 ymin)
        * (1L + int64 zmax - int64 zmin)

    let runTests () =
        test <@ intersect ((0, 10), (0, 10), (0, 10)) ((0, 10), (0, 10), (0, 10)) @>
        test <@ intersect ((0, 10), (0, 10), (0, 10)) ((5, 15), (0, 15), (0, 15)) @>
        test <@ intersect ((0, 10), (0, 10), (0, 10)) ((10, 15), (10, 15), (10, 15)) @>

        test
            <@ intersect ((0, 10), (0, 10), (0, 10)) ((11, 15), (10, 15), (10, 15))
               |> not @>

        test <@ intersect ((0, 10), (0, 10), (0, 10)) ((2, 8), (2, 8), (2, 8)) @>

        test
            <@ add ((0, 10), (0, 10), (0, 10)) ((2, 8), (2, 8), (2, 8))
               |> Seq.length = 27 @>

        test
            <@ subtract ((1, 4), (1, 4), (1, 4)) ((2, 3), (2, 3), (2, 3))
               |> Seq.length = 26 @>

        test <@ volume ((10, 12), (10, 12), (10, 12)) = 27L @>

type Instruction = { command: State; cuboid: Cuboids.T }

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
      cuboid = ((xMin, xMax), (yMin, yMax), (zMin, zMax)) }

let run () =
    printf "Testing..."

    test
        <@ "on x=1..2,y=3..4,z=5..6666" |> parseInstruction = { command = On
                                                                cuboid = ((1, 2), (3, 4), (5, 6666)) } @>

    Lines.runTests ()
    Cuboids.runTests ()
    printfn "...done!"

run ()


let instructions = example |> Seq.map parseInstruction
let first = instructions |> Seq.head
let rest = instructions |> Seq.tail

let totalVolume cuboids = cuboids |> Seq.sumBy Cuboids.volume

let combine cuboids instruction =
    printfn "Applying %A" instruction
    printfn "on cuboids %A" cuboids
    printfn "On count: %d" (cuboids |> totalVolume)

    match instruction.command with
    | On ->
        cuboids
        |> Set.map (fun cuboid -> Cuboids.add cuboid instruction.cuboid)
        |> Set.unionMany
    | Off ->
        cuboids
        |> Set.map (fun cuboid -> Cuboids.subtract cuboid instruction.cuboid)
        |> Set.unionMany


let endState =
    rest
    |> Seq.fold combine (first.cuboid |> Set.singleton)

endState |> List.ofSeq
endState |> totalVolume
