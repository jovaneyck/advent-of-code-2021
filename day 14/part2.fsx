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
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let parseRule (line: string) =
    let [| src; dest |] = line.Split(" -> ")
    (src |> Seq.toList, dest |> Seq.toList)

let parse (input: string []) =
    let template = input.[0] |> Seq.toList

    let rules =
        input
        |> Array.skip 2
        |> Array.map parseRule
        |> Map.ofArray

    (template, rules)

let apply rules (pair: char list) =
    match rules |> Map.tryFind pair with
    | None -> pair
    | Some result ->
        let [ a; b ] = pair
        [ a ] @ result @ [ b ]

let processOne rules t =
    let insertedPairs =
        t
        |> List.pairwise
        |> List.map (fun (a, b) -> apply rules [ a; b ])

    insertedPairs
    |> List.map
        (fun sublist ->
            sublist
            |> List.removeAt ((sublist |> List.length) - 1))
    |> List.collect id
    |> (fun t -> t @ [ insertedPairs |> List.last |> List.last ])

let rec repeat n rules template =
    printf "%d-" n

    let sortedCounts =
        template
        |> List.countBy id
        |> List.sortByDescending snd

    let max = sortedCounts |> List.maxBy snd |> snd
    let min = sortedCounts |> List.minBy snd |> snd
    let difference = (max - min)

    printf "%d-" difference

    template
    |> List.countBy id
    |> List.sortBy fst
    |> List.iter (fun (character, count) -> printf "%c:%d;" character count)

    printfn ""

    if n = 20 then
        template
    else
        repeat (n + 1) rules (processOne rules template)

let solve input =
    let (t, r) = parse input

    let afterX = repeat 0 r t

    let sortedCounts =
        afterX
        |> List.countBy id
        |> List.sortByDescending snd

    let max = sortedCounts |> List.maxBy snd |> snd
    let min = sortedCounts |> List.minBy snd |> snd
    max - min

solve example

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
