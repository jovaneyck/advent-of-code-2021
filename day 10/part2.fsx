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

let findRemainingOpeningBraces line : (char list) option =
    let rec parse' stack line =
        let onClosingBrace stack xs closingBrace openingBrace =
            match stack with
            | opening :: ss when opening = openingBrace -> parse' ss xs
            | _ :: _ -> None
            | [] -> Some stack

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
        | _ -> Some stack

    parse' [] line

let flipParentheses stack =
    stack
    |> List.map
        (function
        | '(' -> ')'
        | '[' -> ']'
        | '{' -> '}'
        | '<' -> '>')

let scores =
    [ (')', 1L)
      (']', 2L)
      ('}', 3L)
      ('>', 4L) ]
    |> Map.ofSeq

let score closingBrackets =
    closingBrackets
    |> Seq.fold (fun acc b -> (acc * 5L) + (scores |> Map.find b)) 0L

let scored =
    input
    |> Array.map Seq.toList
    |> Array.choose findRemainingOpeningBraces
    |> Array.map flipParentheses
    |> Array.map score

let middle =
    scored
    |> Seq.sort
    |> Seq.item ((scored |> Seq.length) / 2)

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ 1 + 1 = 2 @>
    printfn "...done!"

run ()
