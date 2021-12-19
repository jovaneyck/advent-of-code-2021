#r "nuget: Unquote"
open Swensen.Unquote

let example =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\example.txt"

let input =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"

//Algorithm outline:
//For every scanner:
// * Grab a random beacon
// * Calculate the distance from that beacon to every other beacon into a "distance map"
//I think you can find "sensor overlaps" by just looking at these distance sets

type Vector = int list
type Scanner = { id: int; beacons: Vector list }

let parseBeacon (text: string) : Vector =
    let components = text.Split(",")
    components |> Seq.map int |> Seq.toList

let parseScanner (text: string seq) : Scanner =
    let header = text |> Seq.head

    let id =
        header.Split("---", System.StringSplitOptions.RemoveEmptyEntries).[0]
            .Trim()
            .Split(
            " "
        ).[1]
        |> int

    let beacons =
        text
        |> Seq.tail
        |> Seq.map parseBeacon
        |> List.ofSeq

    { id = id; beacons = beacons }

let parse (input: string) =
    input.Split("\r\n\r\n")
    |> Seq.map (fun scanner -> scanner.Split("\n"))
    |> Seq.map parseScanner
    |> Seq.toList

//Represents distances from the first beacon of a scanner to all the other beacons
type DistanceMap = Set<int> list

let distanceMap (scanner: Scanner) : DistanceMap =
    let distance (one: Vector) (other: Vector) =
        List.zip one other
        |> List.map (fun (a, b) -> abs (a - b))
        |> List.sum

    [ for beacon in scanner.beacons do
          let distances =
              scanner.beacons
              |> List.except [ beacon ]
              |> List.map (distance beacon)
              |> Set.ofList

          distances ]

let run () =
    printf "Testing..."

    let small_single_scanner_input =
        @"--- scanner 1337 ---
        -1,-1,1
        -2,-2,2
        -3,-3,3
        -2,-3,1
        5,6,-4
        8,0,7"
            .Split("\n")
        |> Array.map (fun s -> s.Trim())
        |> String.concat "\n"

    test
        <@ parse small_single_scanner_input = [ { id = 1337
                                                  beacons =
                                                      [ [ -1; -1; 1 ]
                                                        [ -2; -2; 2 ]
                                                        [ -3; -3; 3 ]
                                                        [ -2; -3; 1 ]
                                                        [ 5; 6; -4 ]
                                                        [ 8; 0; 7 ] ] } ] @>

    printfn "...done!"

run ()

let maps =
    parse example
    |> Seq.map (fun s -> s, distanceMap s)
    |> Seq.toList

let (_, map) = maps.[0]

let connecting =
    maps
    |> List.tail
    |> List.find
        (fun (_, m) ->
            m
            |> List.exists
                (fun m ->
                    map
                    |> List.exists (fun map -> (Set.intersect m map) |> Set.count >= 11)))
