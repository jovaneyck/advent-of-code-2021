let input =
    System.IO.File.ReadAllLines $"{__SOURCE_DIRECTORY__}\input.txt"

let example =
    @"".Split("\n") |> Array.map (fun s -> s.Trim())

type ParsedPacket =
    | LiteralValue of {| version: int; number: int64 |}
    | Operator of
        {| version: int
           operator: int
           packets: ParsedPacket list |}

type Mode =
    | TotalLengthInBits of int
    | NumberOfPackets of int

let substring startIndex length (s: string) = s.Substring(startIndex, length)

let splitAt length (s: string) =
    s.Substring(0, length), s.Substring(length)

let binToDec (number: string) = System.Convert.ToInt32(number, 2)

let rec groups text =
    let group, remainder = text |> splitAt 5
    let prefix, g = group |> splitAt 1

    if prefix = "0" then
        g, remainder
    else
        let recgroup, rremainder = groups remainder
        g + recgroup, rremainder

let parseLiteralValuePacket text =
    let group, remainder = text |> groups
    group |> binToDec, remainder

let parseMode operatorPacket : Mode * string =
    match operatorPacket |> splitAt 1 with
    | "0", remainder ->
        let lengthText, rremainder = remainder |> splitAt 15
        TotalLengthInBits(lengthText |> binToDec), rremainder
    | "1", remainder ->
        let lengthText, rremainder = remainder |> splitAt 11
        NumberOfPackets(lengthText |> binToDec), rremainder
    | u, rem -> failwithf "Unknown operator packet length type ID %s, with remainder string: %A" u rem

let totalLengthOperatorPacket =
    "00000000000110111101000101001010010001001000000000"

let rec parsePacket text =
    let versionText, remainder = text |> splitAt 3
    let version = versionText |> binToDec
    let packetTypeIdText, rremainder = remainder |> splitAt 3
    let packetTypeId = packetTypeIdText |> binToDec

    if packetTypeId = 4 then
        let literalValue, rrremainder = parseLiteralValuePacket rremainder

        LiteralValue
            {| version = version
               number = literalValue |},
        rrremainder
    else
        let subpackets, rrremainder = parseOperatorPacket rremainder

        Operator
            {| version = version
               operator = packetTypeId
               packets = subpackets |},
        rrremainder

and parseOperatorPacket text =
    let mode, remainder = parseMode totalLengthOperatorPacket

    match mode with
    | TotalLengthInBits total ->
        let subpacketsText, rremainder = remainder |> splitAt total
        parseSubpacketsTotalLength total subpacketsText
    | NumberOfPackets number -> [], ""

and parseSubpacketsTotalLength length text =
    if length = 0 then
        [], text
    else
        [], text

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ "011111100101" |> binToDec = 2021 @>
    test <@ groups "101111111000101000" = ("011111100101", "000") @>
    test <@ parsePacket "110100101111111000101000" = (LiteralValue {| number = 2021L; version = 6 |}, "000") @>

    test
        <@ parseMode "00000000000110111101000101001010010001001000000000" = (TotalLengthInBits 27,
                                                                             "1101000101001010010001001000000000") @>

    printfn "...done!"

run ()
