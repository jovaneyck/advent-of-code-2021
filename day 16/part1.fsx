let input =
    System.IO.File.ReadAllText $"{__SOURCE_DIRECTORY__}\input.txt"


type OperatorType =
    | Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo

type ParsedPacket =
    | LiteralValue of {| version: int64; number: int64 |}
    | Operator of
        {| version: int64
           operator: OperatorType
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

let parseOperator =
    function
    | 0L -> Sum
    | 1L -> Product
    | 2L -> Minimum
    | 3L -> Maximum
    | 5L -> GreaterThan
    | 6L -> LessThan
    | 7L -> EqualTo

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
               operator = parseOperator packetTypeId
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

let rec apply operator packets =
    match operator with
    | Sum -> packets |> List.map evaluate |> List.reduce (+)
    | Product -> packets |> List.map evaluate |> List.reduce (*)
    | Minimum -> packets |> List.map evaluate |> List.min
    | Maximum -> packets |> List.map evaluate |> List.max
    | GreaterThan ->
        let [ a; b ] = packets |> List.map evaluate
        if a > b then 1L else 0L
    | LessThan ->
        let [ a; b ] = packets |> List.map evaluate
        if a < b then 1L else 0L
    | EqualTo ->
        let [ a; b ] = packets |> List.map evaluate
        if a = b then 1L else 0L

and evaluate packet =
    match packet with
    | LiteralValue lv -> lv.number
    | Operator op -> apply op.operator op.packets

let solve text =
    let packet, _ = text |> hexToBinary |> parsePacket

    packet |> evaluate

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
                        {| operator = LessThan
                           packets =
                               [ LiteralValue {| number = 10L; version = 6 |}
                                 LiteralValue {| number = 20L; version = 2 |} ]
                           version = 1 |} @>

    test
        <@ parsePacket "11101110000000001101010000001100100000100011000001100000"
           |> fst = Operator
                        {| operator = Maximum
                           packets =
                               [ LiteralValue {| number = 1L; version = 2 |}
                                 LiteralValue {| number = 2L; version = 4 |}
                                 LiteralValue {| number = 3L; version = 1 |} ]
                           version = 7 |} @>

    test <@ solve "C200B40A82" = 3L @>
    test <@ solve "04005AC33890" = 54L @>
    test <@ solve "880086C3E88112" = 7L @>
    test <@ solve "CE00C43D881120" = 9L @>
    test <@ solve "D8005AC2A8F0" = 1L @>
    test <@ solve "F600BC2D8F" = 0L @>
    test <@ solve "9C005AC2F8F0" = 0L @>
    test <@ solve "9C0141080250320F1802104A08" = 1L @>

    printfn "...done!"

run ()
