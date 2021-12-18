let input =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"


type ParsedPacket =
    | LiteralValue of {| version: int64; number: int64 |}
    | Operator of
        {| version: int64
           operator: int64
           packets: ParsedPacket list |}

type Mode =
    | TotalLengthInBits of int64
    | NumberOfPackets of int64

let splitAt (length: int64) (s: string) =
    s.Substring(0, int length), s.Substring(int length)

let binToDec (number: string) = System.Convert.ToInt64(number, 2)

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
    | u, rem ->
        failwithf "Unknown operator packet length type ID %s, with remainder string: %A" u rem

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
    let mode, remainder = parseMode text

    match mode with
    | TotalLengthInBits total ->
        let subpacketsText, rremainder = remainder |> splitAt total
        parseSubpacketsTotalLength subpacketsText, rremainder
    | NumberOfPackets number -> parseSubPacketsNumberPackets number remainder

and parseSubpacketsTotalLength text =
    if text = "" then
        []
    else
        let packet, remainder = parsePacket text
        packet :: (parseSubpacketsTotalLength remainder)

and parseSubPacketsNumberPackets nbPackets text =
    if nbPackets = 0 then
        [], text
    else
        let packet, remainder = parsePacket text

        let recPackets, rremainder =
            parseSubPacketsNumberPackets (nbPackets - 1L) remainder

        packet :: recPackets, rremainder

let hexToBinary (text: string) : string =
    let lookup =
        [ '0', "0000"
          '1', "0001"
          '2', "0010"
          '3', "0011"
          '4', "0100"
          '5', "0101"
          '6', "0110"
          '7', "0111"
          '8', "1000"
          '9', "1001"
          'A', "1010"
          'B', "1011"
          'C', "1100"
          'D', "1101"
          'E', "1110"
          'F', "1111" ]
        |> Map.ofSeq

    text
    |> Seq.map (fun hexChar -> lookup |> Map.find hexChar)
    |> String.concat ""

let rec addVersions acc packet =
    match packet with
    | LiteralValue v -> acc + v.version
    | Operator o ->
        let sub = o.packets |> List.fold addVersions 0L
        acc + o.version + sub

let solve text =
    let packet, _ = text |> hexToBinary |> parsePacket

    packet |> (addVersions 0)

//solve input

#r "nuget: Unquote"
open Swensen.Unquote

let run () =
    printf "Testing..."
    test <@ "D2FE28" |> hexToBinary = "110100101111111000101000" @>
    test <@ "011111100101" |> binToDec = 2021 @>
    test <@ groups "101111111000101000" = ("011111100101", "000") @>

    test
        <@ parsePacket "110100101111111000101000" |> fst = LiteralValue
                                                               {| number = 2021L; version = 6 |} @>

    test
        <@ parseMode "00000000000110111101000101001010010001001000000000" = (TotalLengthInBits 27,
                                                                             "1101000101001010010001001000000000") @>


    test
        <@ parsePacket "00111000000000000110111101000101001010010001001000000000"
           |> fst = Operator
                        {| operator = 6
                           packets =
                               [ LiteralValue {| number = 10L; version = 6 |}
                                 LiteralValue {| number = 20L; version = 2 |} ]
                           version = 1 |} @>

    test
        <@ parsePacket "11101110000000001101010000001100100000100011000001100000"
           |> fst = Operator
                        {| operator = 3
                           packets =
                               [ LiteralValue {| number = 1L; version = 2 |}
                                 LiteralValue {| number = 2L; version = 4 |}
                                 LiteralValue {| number = 3L; version = 1 |} ]
                           version = 7 |} @>

    test <@ solve "8A004A801A8002F478" = 16L @>
    test <@ solve "620080001611562C8802118E34" = 12L @>
    test <@ solve "C0015000016115A2E0802F182340" = 23L @>
    test <@ solve "A0016C880162017C3686B18A3D4780" = 31L @>

    printfn "...done!"

run ()
