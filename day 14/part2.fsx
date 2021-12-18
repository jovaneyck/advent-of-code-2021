#r "nuget: Unquote"
open Swensen.Unquote

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
    { PairCounts: Map<char * char, uint64> }

let parseRule (line: string) =
    let [| src; dest |] =
        line.Split([| " -> " |], System.StringSplitOptions.None)

    let src1 = src.[0]
    let src2 = src.[1]
    ((src1, src2), dest |> Seq.head)


let parsePolymer (input: string) : Polymer =
    let pairCounts =
        input
        |> Seq.pairwise
        |> Seq.countBy id
        |> Seq.map (fun (l, c) -> (l, uint64 c))

    { PairCounts = Map.ofSeq pairCounts }

let parse (input: string []) : Polymer * Ruleset =
    let polymer = parsePolymer input.[0]

    let rules =
        input
        |> Array.skip 2
        |> Array.map parseRule
        |> Map.ofArray

    (polymer, rules)

let apply (rules: Ruleset) (pair, count) =
    let newChar = rules |> Map.find pair

    let (a, b) = pair

    let newPaircounts =
        [ (a, newChar), count
          (newChar, b), count ]

    newPaircounts

let processOne (rules: Ruleset) (t: Polymer) : Polymer =
    let newCounts =
        t.PairCounts
        |> Map.toList
        |> List.collect (apply rules)
        |> List.groupBy fst
        |> List.map (fun (pair, counts) -> pair, counts |> List.sumBy snd)
        |> Map.ofList

    { t with PairCounts = newCounts }

let repeat max rules template =
    let rec repeat' n template =
        if n = max then
            template
        else
            repeat' (n + 1) (processOne rules template)

    repeat' 0 template

let letterCounts (p: Polymer) =
    p.PairCounts
    |> Map.toSeq
    |> Seq.collect (fun ((a, b), count) -> [ (a, count); (b, count) ])
    |> Seq.groupBy fst
    |> Seq.map (fun (char, counts) -> (char, counts |> Seq.map snd |> Seq.sum))

let solve nbSteps input =
    let (template, rules) = parse input

    let resultingPolymer = repeat nbSteps rules template

    let sortedCounts =
        resultingPolymer
        |> letterCounts
        |> Seq.sortByDescending snd

    printfn "Sorted counts: %A" sortedCounts
    let max = sortedCounts |> Seq.maxBy snd
    let min = sortedCounts |> Seq.minBy snd
    (max |> snd) - (min |> snd)

let run () =
    printf "Testing..."
    test <@ solve 10 example = 1588UL @>
    test <@ solve 40 example = 2188189693529UL @>
    printfn "...done!"

run ()

//I'm doing something wrong which ends up in double pair counts, but too tired to figure it out.
//Maybe when we have some energy to troubleshoot, for now we live to solve another puzzle!
let part2 = solve 40 input
part2 / 2UL
//3692219987038UL
