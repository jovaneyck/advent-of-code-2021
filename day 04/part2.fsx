let input =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"
    |> fun s -> s.Split("\r\n\r\n")

let example =
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

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
        .Split("\n\n")

type Number = { value: int; marked: bool }

let parseBoard (text: string) =
    [ for line in text.Split("\n") do
          [ for number in
                line
                    .Trim()
                    .Split(" ", System.StringSplitOptions.RemoveEmptyEntries) -> { value = int number; marked = false } ] ]
    |> array2D

let parse (text: string []) =
    let blocks = text

    let numbers =
        blocks
        |> Seq.head
        |> fun s -> s.Split(",")
        |> Array.map int
        |> Array.toList

    let boards =
        blocks
        |> Seq.tail
        |> Seq.map parseBoard
        |> Seq.toList

    numbers, boards

let mark numberToMark board =
    board
    |> Array2D.map
        (fun number ->
            if number.value = numberToMark then
                { number with marked = true }
            else
                number)

let isWinner (board: Number [,]) =
    let rows =
        [ for i in 0 .. ((board |> Array2D.length1) - 1) -> board.[i, *] ]

    let cols =
        [ for i in 0 .. ((board |> Array2D.length1) - 1) -> board.[*, i] ]

    let vectors = List.append rows cols

    let hasWinner =
        vectors
        |> Seq.exists (fun v -> v |> Seq.forall (fun nb -> nb.marked))

    hasWinner

let printBoard (board: Number [,]) =
    [ for r in 0 .. ((board |> Array2D.length1) - 1) do
          [ for c in board.[r, *] ->
                if c.marked then
                    sprintf "*%d" c.value
                else
                    sprintf "%d" c.value ]
          |> String.concat "\t\t" ]
    |> String.concat "\n"

let rec findLastWinningBoard numbers boards winners : int * Number [,] =
    //printfn "numbers: %A" numbers
    //printfn "boards:"

    //boards
    //|> Seq.map printBoard
    //|> Seq.iter (printfn "%s\n")

    match numbers with
    | [] -> winners |> Seq.last
    | n :: ns ->
        let marked = boards |> List.map (mark n)
        let newWinners = marked |> List.filter isWinner
        let newWinnersWithLastNumber = newWinners |> List.map (fun w -> n, w)

        findLastWinningBoard ns (marked |> List.except newWinners) (winners @ newWinnersWithLastNumber)

let solve text =
    let numbers, boards = parse text
    let lastnumber, winner = findLastWinningBoard numbers boards []

    let sumOfUnmarkedNumbers =
        winner
        |> Seq.cast<Number>
        |> Seq.filter (fun n -> n.marked |> not)
        |> Seq.map (fun n -> n.value)
        |> Seq.sum

    lastnumber * sumOfUnmarkedNumbers

#r "nuget: Unquote"
open Swensen.Unquote

let unmarkedBoardContaining =
    let toUnmarkedNumber n = { value = n; marked = false }
    array2D >> Array2D.map toUnmarkedNumber

let markedBoardContaining =
    let toUnmarkedNumber (n, m) = { value = n; marked = m }
    array2D >> Array2D.map toUnmarkedNumber

let run () =
    printf "Testing..."

    test
        <@ parse [| "1,2,3"
                    "1  0\n3  4"
                    "0 5\n6  7" |] = ([ 1; 2; 3 ],
                                      [ unmarkedBoardContaining [ [ 1; 0 ]
                                                                  [ 3; 4 ] ]
                                        unmarkedBoardContaining [ [ 0; 5 ]
                                                                  [ 6; 7 ] ] ]) @>

    test
        <@ (unmarkedBoardContaining [ [ 1; 2 ]
                                      [ 3; 4 ] ]
            |> mark 3) = (markedBoardContaining [ [ (1, false); (2, false) ]
                                                  [ (3, true); (4, false) ] ]) @>

    test
        <@ (unmarkedBoardContaining [ [ 1; 2 ]
                                      [ 3; 4 ] ]
            |> mark 5) = unmarkedBoardContaining [ [ 1; 2 ]
                                                   [ 3; 4 ] ] @>

    test
        <@ markedBoardContaining [ [ (1, false); (2, false) ]
                                   [ (3, false); (4, false) ] ]
           |> isWinner
           |> not @>

    test
        <@ markedBoardContaining [ [ (1, false); (2, true) ]
                                   [ (3, true); (4, false) ] ]
           |> isWinner
           |> not @>

    test
        <@ markedBoardContaining [ [ (1, true); (2, false) ]
                                   [ (3, false); (4, true) ] ]
           |> isWinner
           |> not @>

    test
        <@ markedBoardContaining [ [ (1, true); (2, true) ]
                                   [ (3, false); (4, false) ] ]
           |> isWinner @>

    test
        <@ markedBoardContaining [ [ (1, false); (2, false) ]
                                   [ (3, true); (4, true) ] ]
           |> isWinner @>

    test
        <@ markedBoardContaining [ [ (1, true); (2, false) ]
                                   [ (3, true); (4, false) ] ]
           |> isWinner @>

    test
        <@ markedBoardContaining [ [ (1, false); (2, true) ]
                                   [ (3, false); (4, true) ] ]
           |> isWinner @>

    test <@ solve example = 1924 @>

    printfn "...done!"

run ()

solve input
