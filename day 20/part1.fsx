#r "nuget: Unquote"
open Swensen.Unquote

type Algorithm = char []
type Image = Map<int * int, char>

let shrink (i: Image) : Image = i |> Map.filter (fun k v -> v = '#')

let parse (text: string []) : (Algorithm * Image) =
    let algorithm = text.[0] |> Seq.toArray

    let image =
        [ for r, row in (text |> Array.skip 2 |> Array.indexed) do
              for c, pixel in row |> Seq.indexed -> ((r, c), pixel) ]
        |> Map.ofSeq
        |> shrink

    algorithm, image

let neighbourLocations (r, c) =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 0)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]
    |> List.map (fun (dr, dc) -> (r + dr, c + dc))

let pixelAt (image: Image) location =
    image
    |> Map.tryFind location
    |> Option.defaultValue '.'

let toBinary (chars: char list) =
    chars
    |> List.map
        (function
        | '.' -> "0"
        | '#' -> "1"
        | u -> failwithf "Unknown pixel value: <%c>" u)
    |> String.concat ""

let toDecimal (binary: string) = System.Convert.ToInt32(binary, 2)
let apply (algorithm: Algorithm) binary = algorithm.[binary]
let applyUpdate image (loc, pixel) = image |> Map.add loc pixel

let enhancePixelAt algorithm image location =
    location
    |> neighbourLocations
    |> List.map (pixelAt image)
    |> (toBinary >> toDecimal >> (apply algorithm))

let enhance algorithm image =
    let interestingLocations =
        image
        |> Map.keys
        |> Seq.toList
        |> Seq.collect neighbourLocations
        |> Seq.distinct

    interestingLocations
    |> Seq.map (fun loc -> loc, enhancePixelAt algorithm image loc)
    |> Seq.fold applyUpdate image
    |> shrink

let print (image: Image) : string =
    let minRow =
        image |> Map.keys |> Seq.map fst |> Seq.min

    let maxRow =
        image |> Map.keys |> Seq.map fst |> Seq.max

    let minCol =
        image |> Map.keys |> Seq.map snd |> Seq.min

    let maxCol =
        image |> Map.keys |> Seq.map snd |> Seq.max

    [ for row in minRow .. maxRow do
          [ for col in minCol .. maxCol -> pixelAt image (row, col) |> string ]
          |> String.concat "" ]
    |> String.concat "\n"

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

    #..#.
    #....
    ##..#
    ..#..
    ..###"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let (algorithm, image) = parse input
let enhance' = enhance algorithm
let enhancedOnce = image |> enhance'
let enhancedTwice = enhancedOnce |> enhance'

let part1 =
    enhancedTwice
    |> Map.filter (fun k v -> v = '#')
    |> Map.count
//wrong guesses: 5366

image |> print |> printfn "%s"

let run () =
    printf "Testing..."
    test <@ toBinary [ '.'; '#'; '.' ] = "010" @>
    printfn "...done!"

run ()
