let input =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"

let example = @"16,1,2,0,4,2,7,1,2,14"

let parse (text: string) =
    text.Split(",") |> Seq.map int |> Seq.toArray


let solve text =
    let positions = text |> parse

    let allPairs =
        [ for (i, position) in positions |> Seq.indexed do
              [ for (j, nextPosition) in positions |> Seq.indexed do
                    if i <> j then (position, nextPosition) ] ]

    allPairs
    |> Seq.map (fun pairs -> pairs |> Seq.sumBy (fun (a, b) -> (a - b) |> abs))
    |> Seq.min

solve input

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ parse example = [| 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 |] @>
    test <@ solve example = 37 @>
    printfn "...done!"

run ()
