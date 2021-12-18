#r "nuget: FParsec"
#r "nuget: FsCheck"
#r "nuget: Unquote"

open Swensen.Unquote
open FParsec
open FsCheck

let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"".Split("\n") |> Array.map (fun s -> s.Trim())

//Basically a binary tree
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

module Parsers =
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
    //LYAHFGG chapter 14 introduces "zippers" to traverse datastructures + maintaining an overview of the context
    //A zipper represents
    // * a node in a snailnumber tree
    // * and the path explaining how we got
    //This allows us to reconstruct the tree when we need to from a current node
    type Breadcrumb =
        | Left of Number
        | Right of Number

    type Path = Breadcrumb list
    type Zipper = (Number * Path)

    let up (z: Zipper) : Zipper =
        let n, path = z

        match path with
        | [] -> failwith "should we crash on an impossible up?"
        | Left r :: ps -> (Pair((n, r), (depth n) - 1), ps)
        | Right l :: ps -> (Pair((l, n), (depth n) - 1), ps)

    let rec top (z: Zipper) : Zipper =
        match z with
        | _, [] -> z
        | _ -> top (up z)

    let left (z: Zipper) : Zipper =
        let n, path = z

        match n with
        | Value _ -> failwith "Cannot descend a leaf node"
        | Pair ((l, r), d) -> l, (Left r :: path)

    let right (z: Zipper) : Zipper =
        let n, path = z

        match n with
        | Value _ -> failwith "Cannot descend a leaf node"
        | Pair ((l, r), _) -> r, (Right l :: path)

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
                match tryFind predicate (left zipper) with
                | Some hit -> Some hit
                | None ->
                    match tryFind predicate (right zipper) with
                    | Some hit -> Some hit
                    | None -> None

    let apply f (z: Zipper) =
        let (node, path) = z
        (f node, path)

let explodes: Number -> bool =
    function
    | Pair ((Value _, Value _), 4) -> true
    | _ -> false

let (Some explosion) =
    (Parsers.parse "[[6,[5,[4,[3,2]]]],1]")
    |> Zipper.ofNumber
    |> Zipper.tryFind explodes

let ((Pair ((Value (leftSplosion, _), Value (rightSplosion, _)), _), path)) = explosion
(leftSplosion, rightSplosion, path)

let rec tryFindLeftSibling (z: Zipper.Zipper) : Zipper.Zipper =
    let parent = z |> Zipper.up
    let sibling = parent |> Zipper.left

    if sibling <> z then
        sibling
    else
        tryFindLeftSibling parent

let rec tryFindRightSibling (z: Zipper.Zipper) : Zipper.Zipper =
    let parent = z |> Zipper.up
    let sibling = parent |> Zipper.right

    if sibling <> z then
        sibling
    else
        tryFindRightSibling parent

"[1,2]" |> Parsers.parse

//TODO: to do the exploding, we need to go up
//but if we're in a left subtree we cannot just go up 1 and left 1
//we need to move up recursively until we can go to an unexplored left??

//explosion
//|> Zipper.apply (fun n -> Value(0, depth n))

let check name prop =
    Check.One(
        name,
        { FsCheck.Config.Default with
              QuietOnSuccess = true },
        prop
    )

let run () =
    printf "Testing..."
    test <@ Parsers.parse "123" = Value(123, 0) @>
    test <@ Parsers.parse "[1,2]" = Pair((Value(1, 1), Value(2, 1)), 0) @>

    test
        <@ Parsers.parse "[1,[3,4]]" = Pair((Value(1, 1), Pair((Value(3, 2), Value(4, 2)), 1)), 0) @>

    test
        <@ Parsers.parse "[[3,4],[[5,6],7]]" = Pair(
            (Pair((Value(3, 2), Value(4, 2)), 1),
             Pair((Pair((Value(5, 3), Value(6, 3)), 2), Value(7, 2)), 1)),
            0
        ) @>

    check
        "Traversing a zipper leaves the snailnumber intact"
        (fun (n: Parsers.PNumber) ->
            let definitePair =
                Parsers.PPair(n, n) |> Parsers.annotateDepth

            let walkedAround =
                definitePair
                |> Zipper.ofNumber
                |> Zipper.left
                |> Zipper.up
                |> Zipper.right
                |> Zipper.toNumber

            definitePair = walkedAround)

    printfn "...done!"

run ()
