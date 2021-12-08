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

let calculateOutput (patterns, output) =
    let ofUniqueLength n segments =
        segments
        |> Seq.find (fun s -> s |> Seq.length = n)

    let ofLength n segments =
        segments
        |> Seq.filter (fun s -> s |> Seq.length = n)

    let minus a b = Set.difference b a

    //Let's figure out the segments for each different digit
    //Easy digits
    let one = patterns |> ofUniqueLength 2
    let four = patterns |> ofUniqueLength 4
    let seven = patterns |> ofUniqueLength 3
    let eight = patterns |> ofUniqueLength 7

    //Complex digits that need some deduction
    let segmentsCF = one
    let segmentsBD = four |> minus segmentsCF

    let digitsWithFiveSegments = patterns |> ofLength 5

    let three =
        digitsWithFiveSegments
        |> Seq.find (fun s -> Set.isSuperset s segmentsCF)

    let five =
        digitsWithFiveSegments
        |> Seq.find (fun d -> Set.isSuperset d segmentsBD)

    let two =
        digitsWithFiveSegments
        |> Seq.except [ three; five ]
        |> Seq.head

    let digitsWithSixSegments = patterns |> ofLength 6

    let six =
        digitsWithSixSegments
        |> Seq.find (fun s -> Set.isSuperset s segmentsCF |> not)

    let nine =
        digitsWithSixSegments
        |> Seq.filter (fun s -> Set.isSuperset s segmentsCF)
        |> Seq.filter (fun s -> Set.isSuperset s segmentsBD)
        |> Seq.head

    let zero =
        digitsWithSixSegments
        |> Seq.except [ nine; six ]
        |> Seq.head

    //YAY! Let's build a lookup table mapping segments to digits now
    let lookup =
        [ (zero, 0)
          (one, 1)
          (two, 2)
          (three, 3)
          (four, 4)
          (five, 5)
          (six, 6)
          (seven, 7)
          (eight, 8)
          (nine, 9) ]
        |> Map.ofSeq

    let outputDigit =
        output
        |> Seq.map (fun digit -> lookup |> Map.find digit)
        |> Seq.map string
        |> String.concat ""
        |> int

    outputDigit

let solve input =
    input
    |> Array.map parse
    |> Array.map calculateOutput
    |> Array.sum

#time //Real: 00:00:00.033, CPU: 00:00:00.031, GC gen0: 2, gen1: 0, gen2: 0
solve input

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ solve example = 61229 @>
    test <@ solve input = 1027483 @>
    printfn "...done!"

run ()
