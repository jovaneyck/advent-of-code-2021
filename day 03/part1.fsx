let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"00100
    11110
    10110
    10111
    10101
    01111
    00111
    11100
    10000
    11001
    00010
    01010"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let convertToDecimal binary = System.Convert.ToInt32(binary, 2)


type Tally = { nbOnes: int; nbZeroes: int }

let solve input =
    let aggregate tallies n =
        tallies
        |> Seq.zip n
        |> Seq.map
            (fun (digit, tally) ->
                if digit = '0' then
                    { tally with
                          nbZeroes = tally.nbZeroes + 1 }
                else
                    { tally with nbOnes = tally.nbOnes + 1 })

    let tallies =
        input
        |> Seq.fold aggregate (List.replicate (example.Length) { nbOnes = 0; nbZeroes = 0 })
        |> Seq.toList


    let gamma =
        tallies
        |> List.map (fun t -> if t.nbOnes > t.nbZeroes then 1 else 0)
        |> List.map string
        |> String.concat ""
        |> convertToDecimal

    let epsilon =
        tallies
        |> List.map (fun t -> if t.nbOnes > t.nbZeroes then 0 else 1)
        |> List.map string
        |> String.concat ""
        |> convertToDecimal

    let product = gamma * epsilon
    gamma, epsilon, product

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ solve example = (22, 9, 198) @>
    printfn "...done!"

run ()

solve input