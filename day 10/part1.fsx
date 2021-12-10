let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"[({(<(())[]>[[{[]{<()<>>
    [(()[<>])]({[<{<<[]>>(
    {([(<{}[<>[]}>{[]{[(<()>
    (((({<>}<{<{<>}{[]{[]{}
    [[<[([]))<([[{}[[()]]]
    [{[{({}]{}}([{[{{{}}([]
    {<[[]]>}<{[{[{[]{()[[[]
    [<(<(<(<{}))><([]([]()
    <{([([[(<>()){}]>(<<{{
    <{([{{}}[<[[[<>{}]]]>[]]"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

let parse line : char option =
    let rec parse' stack line =
        let onClosingBrace stack xs closingBrace openingBrace =
            match stack with
            | opening :: ss when opening = openingBrace -> parse' ss xs
            | _ :: _ -> Some(closingBrace)
            | [] -> None //Incomplete?

        match line with
        | x :: xs ->
            match x with
            | '('
            | '['
            | '{'
            | '<' -> parse' (x :: stack) xs
            | ')' -> onClosingBrace stack xs ')' '('
            | ']' -> onClosingBrace stack xs ']' '['
            | '}' -> onClosingBrace stack xs '}' '{'
            | '>' -> onClosingBrace stack xs '>' '<'
            | unknown -> failwithf "Unknown character parsed: %A" unknown
        | _ -> None //Incomplete line alert!

    parse' [] line

let points =
    [ (')', 3)
      (']', 57)
      ('}', 1197)
      ('>', 25137) ]
    |> Map.ofSeq

input
|> Array.map Seq.toList
|> Array.choose parse
|> Array.map (fun corrupt -> points |> Map.find corrupt)
|> Array.sum

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
