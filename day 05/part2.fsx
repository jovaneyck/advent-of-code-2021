let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

type Point = int * int
type Line = { p1: Point; p2: Point }

let example =
    @"0,9 -> 5,9
    8,0 -> 0,8
    9,4 -> 3,4
    2,2 -> 2,1
    7,0 -> 7,4
    6,4 -> 2,0
    0,9 -> 2,9
    3,4 -> 1,4
    0,0 -> 8,8
    5,5 -> 8,2"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let parseLine (text: string) : Line =
    let parse (point: string) : Point =
        let [| x; y |] = point.Split(",")
        (int x, int y)

    let [| p1; p2 |] = text.Split(" -> ")
    { p1 = parse p1; p2 = parse p2 }

let isCardinal { p1 = (x1, y1); p2 = (x2, y2) } = x1 = x2 || y1 = y2

let pointify ({ p1 = (x1, y1); p2 = (x2, y2) } as line) =
    let [ min_x; max_x ] = [ x1; x2 ] |> List.sort
    let [ min_y; max_y ] = [ y1; y2 ] |> List.sort

    if line |> isCardinal then
        [ for x in min_x .. max_x do
              for y in min_y .. max_y -> (x, y) ]
    else
        let length = max_x - min_x
        let x_direction = if x1 < x2 then 1 else -1
        let y_direction = if y1 < y2 then 1 else -1
        [ for offset in 0 .. length -> (x1 + (x_direction * offset), y1 + (y_direction * offset)) ]

let solve input =
    let lines = input |> Array.map parseLine

    let all_points =
        lines |> Seq.collect pointify |> Seq.countBy id

    let overlaps =
        all_points |> Seq.filter (fun (_, nb) -> nb > 1)

    overlaps |> Seq.length


#time
solve input

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ pointify { p1 = (17, 5); p2 = (14, 5) } = [ (14, 5); (15, 5); (16, 5); (17, 5) ] @>
    test <@ pointify { p1 = (3, 2); p2 = (3, 5) } = [ (3, 2); (3, 3); (3, 4); (3, 5) ] @>
    test <@ pointify { p1 = (1, 1); p2 = (3, 3) } = [ (1, 1); (2, 2); (3, 3) ] @>
    test <@ pointify { p1 = (9, 7); p2 = (7, 9) } = [ (9, 7); (8, 8); (7, 9) ] @>
    test <@ solve example = 12 @>
    printfn "...done!"

run ()
