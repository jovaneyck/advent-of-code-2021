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
    (src |> Seq.toArray, dest |> Seq.toArray)

let parse (input: string []) =
    let template = input.[0] |> Seq.toArray

    let rules =
        input
        |> Array.skip 2
        |> Array.map parseRule
        |> Map.ofArray

    (template, rules)

let apply rules (pair: char[]) =
    match rules |> Map.tryFind pair with
    | None -> [||]
    | Some result -> result
let processOne rules (t : char[]) =
    Array.concat [
        [|for i in 0..((t |> Seq.length)-2) do
            let a = t.[i]
            let b = t.[i+1]
            yield a
            yield! (apply rules [|a; b|])
        |] 
        t |> Array.last |> Array.singleton
    ]

let repeat max rules template =
    let rec repeat n template =
        printf "%d-" n

        //let sortedCounts =
        //    template
        //    |> List.countBy id
        //    |> List.sortByDescending snd

        //let max = sortedCounts |> List.maxBy snd |> snd
        //let min = sortedCounts |> List.minBy snd |> snd
        //let difference = (max - min)

        //printf "%d-" difference

        //template
        //|> List.countBy id
        //|> List.sortBy fst
        //|> List.iter (fun (character, count) -> printf "%c:%d;" character count)

        printfn ""

        if n = max then
            template
        else
            repeat (n + 1) (processOne rules template)

    repeat 0 template

let solve nbSteps input =
    let (t, r) = parse input

    let afterX = repeat nbSteps r t

    let sortedCounts =
        afterX
        |> Seq.countBy id
        |> Seq.sortByDescending snd

    let max = sortedCounts |> Seq.maxBy snd |> snd
    let min = sortedCounts |> Seq.minBy snd |> snd
    max - min

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ solve 40 input = 1588 @>
    printfn "...done!"

run ()
