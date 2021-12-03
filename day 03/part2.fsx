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

let rec filter index chooser (numbers: string seq) =
    if numbers |> Seq.length = 1 then
        numbers |> Seq.head
    else
        let mostCommonBit =
            numbers
            |> Seq.map (Seq.item index)
            |> Seq.countBy id
            |> chooser

        let filtered =
            numbers
            |> Seq.filter (fun digit -> digit.[index] = mostCommonBit)
            |> Seq.toList

        filter (index + 1) chooser filtered

let filterOxygenGeneratorRating =
    filter 0 (Seq.sortDescending >> Seq.maxBy snd >> fst)

let filterCO2Rating = filter 0 (Seq.sort >> Seq.minBy snd >> fst)

let solve (input: string seq) =
    let oxygenGeneratorRating =
        input
        |> filterOxygenGeneratorRating
        |> convertToDecimal

    let co2Rating =
        input |> filterCO2Rating |> convertToDecimal

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
