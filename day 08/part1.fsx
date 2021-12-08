let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
    edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
    fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
    fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
    aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
    fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
    dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
    bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
    egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
    gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let parse (line: string) =
    let parseWords (t: string) = t.Split(" ")
    let [| signalPatternsString; outputDigitsString |] = line.Split(" | ")

    let signalPatterns =
        signalPatternsString
        |> parseWords
        |> Array.map Set.ofSeq

    let outputDigits =
        outputDigitsString
        |> parseWords
        |> Array.map Set.ofSeq

    (signalPatterns, outputDigits)



let (patterns, output) = example |> Array.map parse |> Seq.head

let one =
    patterns
    |> Seq.find (fun s -> s |> Seq.length = 2)

let seven =
    patterns
    |> Seq.find (fun s -> s |> Seq.length = 3)

let minus a b = Set.difference b a

let segmentA = seven |> minus one
let segmentsCF = one

let four =
    patterns
    |> Seq.find (fun s -> s |> Seq.length = 4)

let segmentsBD = four |> minus segmentsCF

let digitsWithSixSegments =
    patterns
    |> Seq.filter (fun s -> s |> Seq.length = 6)

let nine =
    digitsWithSixSegments
    |> Seq.filter (fun s -> Set.isSuperset s segmentsCF)
    |> Seq.filter (fun s -> Set.isSuperset s segmentsBD)
    |> Seq.head

let segmentG =
    nine
    |> minus segmentsCF
    |> minus segmentsBD
    |> minus segmentA

let eight =
    patterns
    |> Seq.filter (fun s -> s |> Seq.length = 7)
    |> Seq.head

let segmentE = eight |> minus nine

let digitsWithFiveSegments =
    patterns
    |> Seq.filter (fun s -> s |> Seq.length = 5)

let three =
    digitsWithFiveSegments
    |> Seq.filter (fun s -> Set.isSuperset s segmentsCF)
    |> Seq.head

let segmentD =
    three
    |> minus segmentsCF
    |> minus segmentA
    |> minus segmentG

let segmentCountsToCandidates =
    [ (7, [ 8 ])
      (2, [ 1 ])
      (5, [ 2; 3; 5 ])
      (4, [ 4 ])
      (6, [ 0; 6; 9 ])
      (3, [ 7 ]) ]

let easy_nb_segments = [ 2; 3; 4; 7 ]

input
|> Array.collect
    (fun s ->
        s.Split(" | ").[1].Split(" ")
        |> Array.map Seq.length)
|> Array.filter (fun n -> easy_nb_segments |> Seq.contains n)
|> Array.length

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
