let input =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"

let example = @"3,4,3,1,2"

type Timer = Timer of int
type Fishcount = Fishcount of int64
type CountedTimer = Timer * Fishcount
type State = Map<Timer, Fishcount>

let reducer (t, Fishcount one) (_, Fishcount other) : CountedTimer = (t, Fishcount(one + other))
let tickDown (Timer t) = if t = 0 then Timer 6 else Timer(t - 1)

let parse (text: string) : State =
    let timers =
        text.Split(",") |> Seq.map int |> Seq.toList

    timers
    |> Seq.groupBy id
    |> Seq.map (fun (timer, fishes) -> Timer timer, Fishcount(fishes |> Seq.length |> int64))
    |> Map.ofSeq

let nextGeneration (generation: State) =
    let nbParents =
        generation
        |> Map.tryFind (Timer 0)
        |> Option.defaultValue (Fishcount 0)

    let next =
        generation
        |> Map.toList
        |> List.map (fun (timer, count) -> (tickDown timer, count))
        //6 can enter the fray in multiple ways, so we need to add those together. First way: 7-fish turning 6, second way: 0-fish turning 6
        |> List.groupBy (fun (timer, _) -> timer)
        |> List.map (fun (_, timedFishcounts) -> timedFishcounts |> List.reduce reducer)

    let fullNext =
        if nbParents = Fishcount 0 then
            next |> Map.ofSeq
        else
            (Timer 8, nbParents) :: next |> Map.ofSeq

    Some(fullNext, fullNext)

let solve text wantedGeneration =
    let gen0 = parse text
    let generations = Seq.unfold nextGeneration gen0

    generations
    |> Seq.item (wantedGeneration - 1)
    |> Map.toSeq
    |> Seq.sumBy (fun (_, Fishcount c) -> c)

#time
//solve input 256

#r "nuget: Unquote"

open Swensen.Unquote

let run () =
    printf "Testing..."

    test
        <@ parse example = ([ (Timer 1, Fishcount 1)
                              (Timer 2, Fishcount 1)
                              (Timer 3, Fishcount 2)
                              (Timer 4, Fishcount 1) ]
                            |> Map.ofSeq) @>

    test <@ solve example 80 = 5934 @>
    test <@ solve example 256 = 1589590444365L @>
    printfn "...done!"

run ()
