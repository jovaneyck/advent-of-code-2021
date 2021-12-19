#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"".Split("\n") |> Array.map (fun s -> s.Trim())

let multiply ma mb =
    let rowsA = ma |> Array2D.length1
    let colsB = mb |> Array2D.length2

    [ for i in 0 .. (rowsA - 1) do
          [ for j in 0 .. (colsB - 1) do
                let vectA = ma.[i, *]
                let vectB = mb.[*, j]

                Seq.zip vectA vectB
                |> Seq.map (fun t -> fst t * snd t)
                |> Seq.sum ] ]
    |> array2D

let rotations v =
    let sin =
        [ 0, 0; 90, 1; 180, 0; 270, -1 ] |> Map.ofSeq

    let cos =
        [ 0, 1; 90, 0; 180, -1; 270, 0 ] |> Map.ofSeq

    let Rx t =
        [ [ 1; 0; 0 ]
          [ 0; cos.[t]; -sin.[t] ]
          [ 0; sin.[t]; cos.[t] ] ]
        |> array2D

    let Ry t =
        [ [ cos.[t]; 0; sin.[t] ]
          [ 0; 1; 0 ]
          [ -sin.[t]; 0; cos.[t] ] ]
        |> array2D

    let Rz t =
        [ [ cos.[t]; -sin.[t]; 0 ]
          [ sin.[t]; cos.[t]; 0 ]
          [ 0; 0; 1 ] ]
        |> array2D

    let rotationmatrices =
        [ 0; 90; 180; 270 ]
        |> List.collect (fun d -> [ Rx; Ry; Rz ] |> List.map (fun m -> m d))
        |> List.distinct //Is it normal we have duplicates for the base rotations?

    rotationmatrices
    |> List.map (fun R -> multiply v R)

rotations (array2D [ [ 8; 0; 7 ] ])

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
