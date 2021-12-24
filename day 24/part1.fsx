#r "nuget: Unquote"
open Swensen.Unquote

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example_three_times_larger =
    @"inp z
    inp x
    mul z 3
    eql z x"
        .Split("\n")
    |> Array.map (fun s -> s.Trim())

type VariableName = VariableName of string

type Value =
    | Literal of int64
    | Variable of VariableName

type Instruction =
    | Inp of VariableName
    | Add of VariableName * Value
    | Mul of VariableName * Value
    | Div of VariableName * Value
    | Mod of VariableName * Value
    | Eql of VariableName * Value

type Program = Instruction list
type ProgramState = (int64 * int64 * int64 * int64)
type Input = int64 list

type State =
    { programState: ProgramState
      input: Input }

let getProgramState (s: State) = s.programState

let parseInstruction (i: string) : Instruction =
    let parseValue (v: string) : Value =
        match System.Int32.TryParse(v) with
        | (true, parsed) -> Literal parsed
        | (false, _) -> v |> VariableName |> Variable

    let tokens = i.Split(" ")

    let parseArgs () =
        let first = tokens.[1] |> VariableName
        let second = tokens.[2] |> parseValue
        (first, second)

    match tokens.[0] with
    | "inp" -> tokens.[1] |> VariableName |> Inp
    | "add" -> parseArgs () |> Add
    | "mul" -> parseArgs () |> Mul
    | "div" -> parseArgs () |> Div
    | "mod" -> parseArgs () |> Mod
    | "eql" -> parseArgs () |> Eql

let parse (text: string seq) : Program =
    text |> Seq.map parseInstruction |> Seq.toList

let write variableName a (w, x, y, z) =
    match variableName with
    | VariableName "w" -> (a, x, y, z)
    | VariableName "x" -> (w, a, y, z)
    | VariableName "y" -> (w, x, a, z)
    | VariableName "z" -> (w, x, y, a)

let read variableName (w, x, y, z) =
    match variableName with
    | VariableName "w" -> w
    | VariableName "x" -> x
    | VariableName "y" -> y
    | VariableName "z" -> z

let readValue value programState =
    match value with
    | Literal v -> v
    | Variable name -> read name programState

let runInstruction (state: State) (i: Instruction) =
    match i with
    | Inp v ->
        let next = state.input |> List.head

        { state with
              input = state.input |> List.tail
              programState = state.programState |> write v next }
    | Add (a, b) ->
        let valA = state.programState |> read a
        let valB = state.programState |> readValue b
        let result = valA + valB

        { state with
              programState = state.programState |> write a result }
    | Div (a, b) ->
        let valA = state.programState |> read a
        let valB = state.programState |> readValue b
        let result = valA / valB

        { state with
              programState = state.programState |> write a result }
    | Eql (a, b) ->
        let valA = state.programState |> read a
        let valB = state.programState |> readValue b
        let result = if valA = valB then 1L else 0L

        { state with
              programState = state.programState |> write a result }
    | Mod (a, b) ->
        let valA = state.programState |> read a
        let valB = state.programState |> readValue b
        let result = valA % valB

        { state with
              programState = state.programState |> write a result }
    | Mul (a, b) ->
        let valA = state.programState |> read a
        let valB = state.programState |> readValue b
        let result = valA * valB

        { state with
              programState = state.programState |> write a result }

let runProgram input program =
    let init =
        { input = input
          programState = (0L, 0L, 0L, 0L) }

    program |> List.fold runInstruction init

let run () =
    printf "Testing..."
    test <@ parseInstruction "inp 33" = ("33" |> VariableName |> Inp) @>
    test <@ "add z w" |> parseInstruction = Add(VariableName "z", Variable(VariableName "w")) @>
    test <@ "add z 25" |> parseInstruction = Add(VariableName "z", Literal 25) @>

    test
        <@ [ "inp z" ] |> parse |> runProgram [ 25; 26 ] = { programState = (0L, 0L, 0L, 25L)
                                                             input = [ 26L ] } @>

    test
        <@ [ "inp x"; "inp y"; "add x y" ]
           |> parse
           |> runProgram [ 25; 26 ] = { programState = (0L, 51L, 26L, 0L)
                                        input = [] } @>

    test
        <@ [ "inp x"; "add x 3" ]
           |> parse
           |> runProgram [ 25 ] = { programState = (0L, 28L, 0L, 0L)
                                    input = [] } @>

    test
        <@ example_three_times_larger
           |> parse
           |> runProgram [ 10; 30 ]
           |> getProgramState
           |> read (VariableName "z") = 1L @>

    test
        <@ example_three_times_larger
           |> parse
           |> runProgram [ 10; 31 ]
           |> getProgramState
           |> read (VariableName "z") = 0L @>

    printfn "...done!"

run ()

let monad = input |> parse

let digits = [ 9L .. (-1L) .. 1L ]

let numbers =
    seq {
        for one in digits do
            for two in digits do
                for three in digits do
                    for four in digits do
                        for five in digits do
                            for six in digits do
                                for seven in digits do
                                    for eight in digits do
                                        for nine in digits do
                                            for ten in digits do
                                                for eleven in digits do
                                                    for twelve in digits do
                                                        for thirteen in digits do
                                                            for fourteen in digits ->
                                                                [ one
                                                                  two
                                                                  three
                                                                  four
                                                                  five
                                                                  six
                                                                  seven
                                                                  eight
                                                                  nine
                                                                  ten
                                                                  eleven
                                                                  twelve
                                                                  thirteen
                                                                  fourteen ]
    }

let result =
    numbers
    |> Seq.find
        (fun number ->
            printfn "%A" number
            let result = runProgram number monad

            result
            |> getProgramState
            |> read (VariableName "z") = 0)
