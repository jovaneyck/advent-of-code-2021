let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"NNCB

    CH -> B
    HH -> N
    CB -> H
    NH -> C
    HB -> C
    HC -> B
    HN -> C
    NN -> C
    BH -> H
    NC -> B
    NB -> B
    BN -> B
    BB -> N
    BC -> B
    CC -> N
    CN -> C"
        .Split([| "\n" |], System.StringSplitOptions.None)
    |> Array.map (fun s -> s.Trim())

type Ruleset = Map<char * char, char>

type Polymer =
    { LetterCounts: Map<char, uint64>
      PairCounts: Map<char * char, uint64> }

let parseRule (line: string) =
    let [| src; dest |] =
        line.Split([| " -> " |], System.StringSplitOptions.None)

    let src1 = src.[0]
    let src2 = src.[1]
    ((src1, src2), dest |> Seq.head)


let parsePolymer (input: string) : Polymer =
    let letterCounts =
        input
        |> Seq.toArray
        |> Seq.countBy id
        |> Seq.map (fun (l, c) -> (l, uint64 c))

    let pairCounts =
        input
        |> Seq.pairwise
        |> Seq.countBy id
        |> Seq.map (fun (l, c) -> (l, uint64 c))

    { LetterCounts = Map.ofSeq letterCounts
      PairCounts = Map.ofSeq pairCounts }

let parse (input: string []) : Polymer * Ruleset =
    let polymer = parsePolymer input.[0]

    let rules =
        input
        |> Array.skip 2
        |> Array.map parseRule
        |> Map.ofArray

    (polymer, rules)

let folder (rules: Ruleset) (polymer: Polymer) paircount =
    let (a, b) as pair, count = paircount
    let newChar = rules |> Map.find pair

    let currentLetterCount =
        polymer.LetterCounts
        |> Map.tryFind newChar
        |> Option.defaultValue 0UL

    let newLetterCounts =
        polymer.LetterCounts
        |> Map.add newChar (currentLetterCount + count)

    let newPaircounts =
        [ (a, newChar), count
          (newChar, b), count ]

    let nextPaircounts =
        List.append
            newPaircounts
            (polymer.PairCounts
             |> Map.remove pair
             |> Map.toList)
        |> List.groupBy fst
        |> List.map (fun (pair, counts) -> pair, counts |> List.sumBy snd)
        |> Map.ofList

    { PairCounts = nextPaircounts
      LetterCounts = newLetterCounts }

let processOne (rules: Ruleset) (t: Polymer) : Polymer =
    t.PairCounts
    |> Map.toSeq
    //TODO: map instead of fold, rebuild maps from individual counts in the end
    |> Seq.fold (folder rules) t

let repeat max rules template =
    let rec repeat' n template =
        printfn "After step %d: %A" n template

        if n = max then
            template
        else
            repeat' (n + 1) (processOne rules template)

    repeat' 0 template

let solve nbSteps input =
    let (t, r) = parse input

    let afterX = repeat nbSteps r t

    let sortedCounts =
        afterX.LetterCounts
        |> Map.toSeq
        |> Seq.sortByDescending snd

    let max = sortedCounts |> Seq.maxBy snd
    let min = sortedCounts |> Seq.minBy snd
    (max |> snd) - (min |> snd)

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ solve 4 example = 1588UL @>
    printfn "...done!"

run ()

let part2 = solve 10 input
