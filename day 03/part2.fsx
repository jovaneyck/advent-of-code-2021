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

let rec filter decider index (digits: string seq) =
    if digits |> Seq.length = 1 then
        digits
    else
        let mostCommonBit =
            digits
            |> Seq.map (Seq.item index)
            |> Seq.countBy id
            |> decider
            |> fst

        let filtered =
            digits
            |> Seq.filter (fun digit -> digit.[index] = mostCommonBit)
            |> Seq.toList

        filter decider (index + 1) filtered

let filterOxygenGeneratorRating =
    filter (Seq.sortDescending >> Seq.maxBy snd)

let filterCO2Rating = filter (Seq.sort >> Seq.minBy snd)

let solve (input: string seq) =
    let oxygenGeneratorRating =
        input
        |> filterOxygenGeneratorRating 0
        |> Seq.head
        |> convertToDecimal

    let co2Rating =
        input
        |> filterCO2Rating 0
        |> Seq.head
        |> convertToDecimal

    oxygenGeneratorRating * co2Rating

solve example
solve input
#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ solve example = 230 @>
    printfn "...done!"

run ()
