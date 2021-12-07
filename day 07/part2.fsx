let input =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"

let example = @"16,1,2,0,4,2,7,1,2,14"

let parse (text: string) =
    text.Split(",") |> Seq.map int |> Seq.toArray

let moveCost a b =
    let distance = (a - b) |> abs
    //Series: sum of first n natural numbers, more info here: https://www.cuemath.com/sum-of-natural-numbers-formula/
    let cost = distance * (distance + 1) / 2
    cost

let solve text =
    let crabPositions = text |> parse
    let first = crabPositions |> Seq.min
    let last = crabPositions |> Seq.max
    let allLocations = [ first .. last ]

    let allPairs =
        [ for location in allLocations do
              [ for nextPosition in crabPositions do
                    (location, nextPosition) ] ]

    allPairs
    |> Seq.map (fun pairs -> pairs |> Seq.sumBy (fun (a, b) -> moveCost a b))
    |> Seq.min

#time //Real: 00:00:00.416, CPU: 00:00:00.406, GC gen0: 21, gen1: 9, gen2: 3
solve input

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ parse example = [| 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 |] @>
    test <@ solve example = 168 @>
    test <@ moveCost 13 13 = 0 @>
    test <@ moveCost 16 5 = 66 @>
    test <@ moveCost 0 5 = 15 @>
    printfn "...done!"

run ()
