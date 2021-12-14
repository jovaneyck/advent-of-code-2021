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
        .Split([|"\n"|], System.StringSplitOptions.None)
    |> Array.map (fun s -> s.Trim())

let parseRule (line: string) =
    let [| src; dest |] = line.Split([|" -> "|], System.StringSplitOptions.None)
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
    let max = (t |> Seq.length)-2
    
    [|for i in 0..max do
        let a = t.[i]
        let b = t.[i+1]
        yield a
        yield! (apply rules [|a; b|])
        if i = max then yield b        
    |] 

let repeat max rules template =
    let rec repeat' n template =
        printfn "%s" (template |> Seq.map string |> String.concat "")

        //printf "%d-" n

        //let sortedCounts =
        //    template
        //    |> Array.countBy id
        //    |> Array.sortByDescending snd

        //let maxCount = sortedCounts |> Array.maxBy snd
        //let minCount = sortedCounts |> Array.minBy snd
        //let difference = (snd maxCount - (snd minCount))
        //printf "%A-" (fst minCount, fst maxCount)

        //printf "%d-" difference

        //template
        //|> Array.countBy id
        //|> Array.sortBy fst
        //|> Array.iter (fun (character, count) -> printf "%c:%d;" character count)

        //printfn ""

        if n = max then
            template
        else
            repeat' (n + 1) (processOne rules template)

    repeat' 0 template

let solve nbSteps input =
    let (t, r) = parse input

    let afterX = repeat nbSteps r t

    let sortedCounts =
        afterX
        |> Seq.countBy id
        |> Seq.sortByDescending snd

    let max = sortedCounts |> Seq.maxBy snd
    let min = sortedCounts |> Seq.minBy snd
    (max |> snd) - (min |> snd)

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ solve 10 example = 1588 @>
    printfn "...done!"

run ()

let part2 =  solve 20 input
