let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

    22 13 17 11  0
     8  2 23  4 24
    21  9 14 16  7
     6 10  3 18  5
     1 12 20 15 19

     3 15  0  2 22
     9 18 13 17  5
    19  8  7 25 23
    20 11 10 24  4
    14 21 16 12  6

    14 21 17 24  4
    10 16 15  9 19
    18  8 23 26 20
    22 11 13  6  5
     2  0 12  3  7"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

type Position = { row: int; col: int }
type Number = { value: int; marked: bool }

let rec parseBoards (boardLines: string []) =
    printfn "Hey!"
    printfn "%A" boardLines

    if boardLines |> Seq.isEmpty then
        []
    else
        let board =
            [ for (row, line) in boardLines |> Array.take 5 |> Array.indexed do
                  let columns =
                      line.Split(" ")
                      |> Array.except [ "" ]
                      |> Array.map int
                      |> Array.indexed

                  for (column, number) in columns -> ({ row = row; col = column }, { value = number; marked = false }) ]
            |> Map.ofSeq

        let rest =
            boardLines
            |> Array.skip (
                if boardLines |> Seq.length > 5 then
                    6
                else
                    5
            )

        board :: (parseBoards rest)

let parse (text: string []) =
    let numbers =
        text.[0].Split(",")
        |> Array.map int
        |> Array.toList

    let boards = parseBoards (text |> Array.skip 2)
    numbers, boards

let mark numberToMark board =
    board
    |> Map.map
        (fun _ number ->
            if number.value = numberToMark then
                { number with marked = true }
            else
                number)

let isWinner board =
    let rows =
        [ for row in 0 .. 4 ->
              board
              |> Map.toList
              |> List.filter (fun (pos, _) -> pos.row = row) ]

    let cols =
        [ for col in 0 .. 4 ->
              board
              |> Map.toList
              |> List.filter (fun (pos, _) -> pos.col = col) ]


    let vectors = List.append rows cols

    let hasWinner =
        vectors
        |> List.exists (fun v -> v |> List.forall (fun (pos, nb) -> nb.marked))

    hasWinner

let rec findWinningBoard numbers boards =
    let (n :: ns) = numbers
    let marked = boards |> List.map (mark n)
    let winner = marked |> List.tryFind isWinner

    match winner with
    | None -> findWinningBoard ns marked
    | Some winner -> n, winner

let solve text =
    let numbers, boards = parse text
    let lastnumber, winner = findWinningBoard numbers boards

    let sumOfUnmarkedNumbers =
        winner
        |> Map.values
        |> Seq.filter (fun n -> n.marked |> not)
        |> Seq.map (fun n -> n.value)
        |> Seq.sum

    lastnumber * sumOfUnmarkedNumbers

solve input

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."

    test
        <@ mark
            3
            ([ ({ row = 1; col = 2 }, { value = 3; marked = false }) ]
             |> Map.ofSeq) = ([ ({ row = 1; col = 2 }, { value = 3; marked = true }) ]
                              |> Map.ofSeq) @>

    test
        <@ mark
            6
            ([ ({ row = 1; col = 2 }, { value = 3; marked = false }) ]
             |> Map.ofSeq) = ([ ({ row = 1; col = 2 }, { value = 3; marked = false }) ]
                              |> Map.ofSeq) @>

    printfn "...done!"

run ()
