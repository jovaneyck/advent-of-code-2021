let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"199
200
208
210
200
207
240
269
260
263"

let isIncrease (n1, n2) = n1 < n2

let solve =
    Seq.map int
    >> Seq.pairwise
    >> Seq.filter isIncrease
    >> Seq.length

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ example.Split("\n") |> solve = 7 @>
    printfn "...done!"

run ()

input |> solve
