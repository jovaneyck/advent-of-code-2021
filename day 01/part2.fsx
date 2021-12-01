let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example = @"199
200
208
210
200
207
240
269
260
263"

let isIncrease (n1,n2) = n1 < n2

let solve input =
    input
    |> Seq.map int 
    |> Seq.windowed 3 
    |> Seq.map Seq.sum
    |> Seq.pairwise
    |> Seq.filter isIncrease 
    |> Seq.length

input |> solve

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
