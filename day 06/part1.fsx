let input =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"

let example = @"3,4,3,1,2"

let parse (text: string) =
    text.Split(",") |> Seq.map int |> Seq.toList

#r "nuget: Unquote"
open Swensen.Unquote

let nextGeneration generation =
    let nbParents =
        generation |> List.filter ((=) 0) |> List.length

    let next =
        generation
        |> List.map (fun n -> if n = 0 then 6 else n - 1)

    let fullNext = next @ (List.replicate nbParents 8)
    Some(fullNext, fullNext)

let solve text wantedGeneration =
    let gen0 = parse text
    let generations = Seq.unfold nextGeneration gen0

    generations
    |> Seq.item (wantedGeneration - 1)
    |> Seq.length

#time
//solve example 256

let run () =
    printf "Testing..."
    test <@ parse example = [ 3; 4; 3; 1; 2 ] @>
    test <@ solve example 80 = 5934 @>
    printfn "...done!"

run ()

//26,984,457,539
// 2,147,483,647
