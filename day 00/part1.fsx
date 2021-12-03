let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"".Split("\n") |> Array.map (fun s -> s.Trim())

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
