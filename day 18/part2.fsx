#r "nuget: FParsec"
#r "nuget: FsCheck"
#r "nuget: Unquote"

open Swensen.Unquote
open FParsec
open FsCheck

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

//Snailnumber === binary tree
type Depth = int

type Number =
    | Pair of (Number * Number) * Depth
    | Value of int * Depth

let rec print: Number -> string =
    function
    | Value (v, _) -> string v
    | Pair ((l, r), _) -> sprintf "[%s,%s]" (print l) (print r)

let depth: Number -> int =
    function
    | Value (_, d) -> d
    | Pair (_, d) -> d

module Parser =
    type PNumber =
        | PPair of PNumber * PNumber
        | PValue of int

    // Grammar
    // Number -> 0..9
    // Number -> [Number,Number]
    type Parser<'t> = Parser<'t, unit>

    let pSnailNumber, pSnailNumbersRef =
        createParserForwardedToRef<PNumber, unit> ()

    let pValue: Parser<PNumber> = pint32 |>> PValue

    let pPair: Parser<PNumber> =
        between (pstring "[") (pstring "]") (sepBy pSnailNumber (pstring ","))
        |>> (fun xs -> PPair(xs.[0], xs.[1]))

    pSnailNumbersRef := pValue <|> pPair

    let annotateDepth (n: PNumber) =
        let rec annotateDepth depth (n: PNumber) =
            match n with
            | PValue v -> Value(v, depth)
            | PPair (a, b) ->
                let annotatedA = annotateDepth (depth + 1) a
                let annotatedB = annotateDepth (depth + 1) b
                Pair((annotatedA, annotatedB), depth)

        annotateDepth 0 n

    let runParser p textToParse =
        match run p textToParse with
        | Success (result, _, _) -> result
        | failure -> failwithf "%A" failure

    let parse textToParse =
        runParser pSnailNumber textToParse
        |> annotateDepth

module Zipper =
    //LYAHFGG! chapter 14 introduces "zippers" to traverse datastructures + maintaining an overview of the context
    //A zipper represents
    // * a node in a snailnumber tree
    // * and the path explaining how we got there
    //This allows us to reconstruct the tree when we need to from wherever we find ourselves in the tree
    type Breadcrumb =
        | Left of Number
        | Right of Number

    type Path = Breadcrumb list
    type Zipper = (Number * Path)

    let up (z: Zipper) : Zipper option =
        let n, path = z

        match path with
        | [] -> None
        | Left r :: ps -> Some(Pair((n, r), (depth n) - 1), ps)
        | Right l :: ps -> Some(Pair((l, n), (depth n) - 1), ps)

    let rec top (z: Zipper) : Zipper =
        match z with
        | _, [] -> z
        | _ ->
            let (Some upper) = up z
            top upper

    let left (z: Zipper) : Zipper option =
        let n, path = z

        match n with
        | Value _ -> None
        | Pair ((l, r), d) -> Some(l, (Left r :: path))

    let right (z: Zipper) : Zipper option =
        let n, path = z

        match n with
        | Value _ -> None
        | Pair ((l, r), _) -> Some(r, (Right l :: path))

    let rec bottomLeft (z: Zipper) : Zipper option =
        match z |> left with
        | Some v -> bottomLeft v
        | None -> Some z

    let rec bottomRight (z: Zipper) : Zipper option =
        match z |> right with
        | Some v -> bottomRight v
        | None -> Some z

    let rec tryLeftSibling (z: Zipper) : Zipper option =
        let parent = z |> up

        match parent with
        | None -> None
        | Some parent ->
            match parent |> left with
            | None -> None
            | Some sibling ->
                if sibling <> z then
                    bottomRight sibling
                else
                    tryLeftSibling parent

    let rec tryRightSibling (z: Zipper) : Zipper option =
        let parent = z |> up

        match parent with
        | None -> None
        | Some parent ->
            match parent |> right with
            | None -> None
            | Some sibling ->
                if sibling <> z then
                    bottomLeft sibling
                else
                    tryRightSibling parent

    let toNumber z : Number =
        let n, _ = top z
        n

    let ofNumber (n: Number) : Zipper = n, []

    let rec tryFind (predicate: Number -> bool) (zipper: Zipper) : Zipper option =
        let number, path = zipper

        if predicate number then
            Some(number, path)
        else //No direct match, we need to explore the tree
            match number with
            | Value _ -> None
            | _ ->
                match left zipper with
                | Some l ->
                    match tryFind predicate l with
                    | Some hit -> Some hit
                    | None ->
                        match right zipper with
                        | None -> None
                        | Some r ->
                            match tryFind predicate r with
                            | Some hit -> Some hit
                            | None -> None
                | None ->
                    match right zipper with
                    | None -> None
                    | Some r ->
                        match tryFind predicate r with
                        | Some hit -> Some hit
                        | None -> None

    let apply f (z: Zipper) =
        let (node, path) = z
        (f node, path)

let needsExplode: Number -> bool =
    function
    | Pair ((Value _, Value _), depth) when depth >= 4 -> true
    | _ -> false

let explode explosion =
    match explosion with
    | ((Pair ((Value (leftSplosion, _), Value (rightSplosion, _)), _), _)) ->

        let zeroValued =
            explosion
            |> Zipper.apply (fun n -> (Value(0, (depth n))))

        let leftApplied =
            match zeroValued |> Zipper.tryLeftSibling with
            | Some lefty ->
                let (Some applied) =
                    lefty
                    |> Zipper.apply
                        (function
                        | Value (v, d) -> Value(v + leftSplosion, d))
                    |> Zipper.tryRightSibling

                applied
            | None -> zeroValued

        let rightApplied =
            match leftApplied |> Zipper.tryRightSibling with
            | None -> leftApplied
            | Some righty ->
                righty
                |> Zipper.apply
                    (function
                    | Value (v, d) -> Value(v + rightSplosion, d))

        rightApplied
    | _ -> failwith "Never expected to explode this thing"

let needsSplit (n: Number) =
    match n with
    | Value (v, _) when v >= 10 -> true
    | _ -> false

let split (n: Number) =
    match n with
    | Pair _ -> failwith "Never expected to split a pair, something is wrong"
    | Value (v, d) -> Pair((Value(v / 2, d + 1), Value((v + 1) / 2, d + 1)), d)

let handleSplit (z: Zipper.Zipper) = z |> Zipper.apply split

let rec reduce (z: Zipper.Zipper) =
    match z |> Zipper.tryFind needsExplode with
    | Some explosion -> explosion |> explode |> Zipper.top |> reduce
    | None -> z |> Zipper.top |> reduceSplits

and reduceSplits (z: Zipper.Zipper) =
    let nextSplit = z |> Zipper.tryFind needsSplit

    match nextSplit with
    | None -> z
    | Some s -> s |> handleSplit |> Zipper.top |> reduce

let add (n: Number) (n2: Number) : Number =
    let rec bury (n: Number) =
        match n with
        | Value (v, d) -> Value(v, d + 1)
        | Pair ((l, r), d) -> Pair((bury l, bury r), d + 1)

    Pair((bury n, bury n2), depth n)
    |> Zipper.ofNumber
    |> reduce
    |> Zipper.toNumber

let sum numbers = numbers |> Seq.reduce add


let rec magnitude (n: Number) : uint64 =
    match n with
    | Value (v, _) -> uint64 v
    | Pair ((l, r), _) -> 3UL * (magnitude l) + 2UL * (magnitude r)

[<AutoOpen>]
module Tests =
    let check name prop =
        Check.One(
            name,
            { FsCheck.Config.Default with
                  QuietOnSuccess = true },
            prop
        )

    let tSingleExplode number =
        number
        |> Parser.parse
        |> Zipper.ofNumber
        |> Zipper.tryFind needsExplode
        |> Option.get
        |> explode
        |> Zipper.toNumber
        |> print

    let run () =
        printf "Testing..."
        test <@ Parser.parse "123" = Value(123, 0) @>
        test <@ Parser.parse "[1,2]" = Pair((Value(1, 1), Value(2, 1)), 0) @>

        test
            <@ Parser.parse "[1,[3,4]]" = Pair(
                (Value(1, 1), Pair((Value(3, 2), Value(4, 2)), 1)),
                0
            ) @>

        test
            <@ Parser.parse "[[3,4],[[5,6],7]]" = Pair(
                (Pair((Value(3, 2), Value(4, 2)), 1),
                 Pair((Pair((Value(5, 3), Value(6, 3)), 2), Value(7, 2)), 1)),
                0
            ) @>

        check
            "Traversing a zipper leaves the snailnumber intact"
            (fun (n: Parser.PNumber) ->
                let definitePair =
                    Parser.PPair(n, n) |> Parser.annotateDepth

                let walkedAround =
                    definitePair
                    |> Zipper.ofNumber
                    |> Zipper.left
                    |> Option.get
                    |> Zipper.up
                    |> Option.get
                    |> Zipper.right
                    |> Option.get
                    |> Zipper.toNumber

                definitePair = walkedAround)

        test <@ tSingleExplode "[[[[[9,8],1],2],3],4]" = "[[[[0,9],2],3],4]" @>
        test <@ tSingleExplode "[7,[6,[5,[4,[3,2]]]]]" = "[7,[6,[5,[7,0]]]]" @>
        test <@ tSingleExplode "[[6,[5,[4,[3,2]]]],1]" = "[[6,[5,[7,0]]],3]" @>

        test
            <@ tSingleExplode "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" = "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" @>

        test
            <@ tSingleExplode "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]" = "[[3,[2,[8,0]]],[9,[5,[7,0]]]]" @>

        test <@ split (Value(10, 1337)) = Pair((Value(5, 1338), Value(5, 1338)), 1337) @>
        test <@ split (Value(11, 1337)) = Pair((Value(5, 1338), Value(6, 1338)), 1337) @>
        test <@ split (Value(12, 666)) = Pair((Value(6, 667), Value(6, 667)), 666) @>

        test
            <@ "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"
               |> Parser.parse
               |> Zipper.ofNumber
               |> reduce
               |> Zipper.toNumber
               |> print = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" @>

        test
            <@ [ "[1,1]"; "[2,2]"; "[3,3]"; "[4,4]" ]
               |> Seq.map Parser.parse
               |> sum
               |> print = "[[[[1,1],[2,2]],[3,3]],[4,4]]" @>

        test
            <@ [ "[1,1]"
                 "[2,2]"
                 "[3,3]"
                 "[4,4]"
                 "[5,5]" ]
               |> Seq.map Parser.parse
               |> sum
               |> print = "[[[[3,0],[5,3]],[4,4]],[5,5]]" @>

        test
            <@ [ "[1,1]"
                 "[2,2]"
                 "[3,3]"
                 "[4,4]"
                 "[5,5]"
                 "[6,6]" ]
               |> Seq.map Parser.parse
               |> sum
               |> print = "[[[[5,0],[7,4]],[5,5]],[6,6]]" @>

        test
            <@ [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
                 "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
                 "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
                 "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
                 "[7,[5,[[3,8],[1,4]]]]"
                 "[[2,[2,2]],[8,[8,1]]]"
                 "[2,9]"
                 "[1,[[[9,3],9],[[9,0],[0,7]]]]"
                 "[[[5,[7,4]],7],1]"
                 "[[[[4,2],2],6],[8,7]]" ]
               |> Seq.map Parser.parse
               |> sum
               |> print = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" @>

        printfn "...done!"

Tests.run ()

let combos numbers =
    [ for i in 0 .. (numbers |> Array.length) - 1 do
          for j in i .. (numbers |> Array.length) - 1 -> numbers.[i], numbers.[j] ]

input
|> Array.map Parser.parse
|> combos
|> List.map (fun (one, other) -> add one other)
|> List.map magnitude
|> List.max
