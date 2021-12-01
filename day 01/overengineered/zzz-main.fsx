#load "Parser.fs"
#load "BusinessLogic.fs"
#load "SlidingWindow.fs"
#load "App.fs"

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

let solve1: string seq -> int = App.solve id

let solve2: string seq -> int =
    App.solve (SlidingWindow.ofSize 3 >> Seq.map Seq.sum)

#r "nuget: Unquote"
open Swensen.Unquote

let runTests () =
    printf "Testing..."
    test <@ example.Split("\n") |> solve1 = 7 @>
    test <@ example.Split("\n") |> solve2 = 5 @>
    printfn "...done!"

runTests ()

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\..\input.txt"

[solve1; solve2] |> List.map (fun f -> f input) |> List.iter (printfn "%d")
