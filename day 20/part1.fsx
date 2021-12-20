#r "nuget: Unquote"
open Swensen.Unquote

type Algorithm = char []
type Image = Map<int * int, char>

//Bah, this took me a while to figure out
//the example has a subtle difference to my actual input:
//The actual input has a "#" rule for the 000000000 number so every dark pixel in the infinite space lights up
//Accordingly, every 111111111 rule falls back into darkness '.'
//We keep track of the "infiniteness" by tracking the "state of all infinite pixels" explicitly
type State =
    { algorithm: Algorithm
      image: Image
      infinitePixelsState: char }

//let shrink (i: Image) : Image = i |> Map.filter (fun k v -> v = '#')

let parse (text: string []) : State =
    let algorithm = text.[0] |> Seq.toArray

    let image =
        [ for r, row in (text |> Array.skip 2 |> Array.indexed) do
              for c, pixel in row |> Seq.indexed -> ((r, c), pixel) ]
        |> Map.ofSeq
    //|> shrink

    { algorithm = algorithm
      image = image
      infinitePixelsState = '.' }

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

let pixelAt defaultInfinite (image: Image) location =
    image
    |> Map.tryFind location
    |> Option.defaultValue defaultInfinite

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

let enhancePixelAt state location =
    location
    |> neighbourLocations
    |> List.map (pixelAt state.infinitePixelsState state.image)
    |> (toBinary >> toDecimal >> (apply state.algorithm))

let nextInfinite (state: State) =
    if state.algorithm.[0] = '.' then
        '.'
    else
        match state.infinitePixelsState with
        | '.' -> '#'
        | _ -> '.'

let enhance state =
    let interestingLocations =
        state.image
        |> Map.keys
        |> Seq.toList
        |> Seq.collect neighbourLocations
        |> Seq.distinct

    let nextImage =
        interestingLocations
        |> Seq.map (fun loc -> loc, enhancePixelAt state loc)
        |> Seq.fold applyUpdate state.image
    //|> shrink

    { state with
          image = nextImage
          infinitePixelsState = nextInfinite state }

let print (state: State) : string =
    let image = state.image

    let minRow =
        image |> Map.keys |> Seq.map fst |> Seq.min

    let maxRow =
        image |> Map.keys |> Seq.map fst |> Seq.max

    let minCol =
        image |> Map.keys |> Seq.map snd |> Seq.min

    let maxCol =
        image |> Map.keys |> Seq.map snd |> Seq.max

    [ for row in minRow .. maxRow do
          [ for col in minCol .. maxCol ->
                pixelAt state.infinitePixelsState image (row, col)
                |> string ]
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

let state = parse input
let enhancedOnce = state |> enhance
let enhancedTwice = enhancedOnce |> enhance

//enhancedOnce |> print |> printfn "%s"
//enhancedTwice |> print |> printfn "%s"


let part1 =
    enhancedTwice.image
    |> Map.filter (fun k v -> v = '#')
    |> Map.count

let run () =
    printf "Testing..."
    test <@ toBinary [ '.'; '#'; '.' ] = "010" @>
    printfn "...done!"

run ()
