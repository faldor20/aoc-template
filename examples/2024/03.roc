app [main!] {
    pf: platform "../../../basic-cli/platform/main.roc",
    aoc: "../../package/main.roc",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import pf.Stdout
import pf.File
import pf.Utc
import parser.Parser
import parser.String

import aoc.AoC {
    stdin!: \{} -> File.readBytes! "examples/input/2024_03.txt",
    stdout!: Stdout.write!,
    time!: \{} -> Utc.now! {} |> Utc.toNanosSinceEpoch,
}

main! = \{} ->
    AoC.solve! {
        year: 2024,
        day: 2,
        title: "Red-Nosed Reports ",
        part1: part1f,
        part2: part2f,
    }

# ========================
#    Helper functions
# ========================

unzip = \list ->
    list
    |> List.walk
        ([], [])
        \(aList, bList), (a, b) ->
            (
                aList |> List.append (a),
                bList |> List.append (b),
            )

# ========================
#    Input parsing
# ========================

parseLine : _ -> Result _ _
parseLine = \x ->
    list = x |> Str.splitOn " "
    list |> List.mapTry (Str.toI16)

parseInput = \input ->
    input
    |> Str.splitOn "\n"
    |> List.keepOks parseLine

# ========================
#    Solutions
# ========================
slidingPairs : List a, (a, a -> b) -> List b
slidingPairs = \list, fn ->
    loop = \lst, res ->
        when lst is
            [a, b, ..] ->
                loop
                    (lst |> List.dropFirst 1)
                    (res |> List.append (fn a b))

            [_] -> res
            [] -> res
    loop list (List.withCapacity (List.len list - 1))

resultFromBool = \a -> if a then Ok {} else Err WasFalse

isDigit0to9 : U8 -> Bool
isDigit0to9 = \b -> b >= '0' && b <= '9'

import parser.Parser exposing [skip, const, keep]
import parser.String exposing [string, digits]

## Runs the eater and until parsers again and again until the parsing succeeds or we run out of input
retryTillSuccess = \after, eater, until ->
    Parser.buildPrimitiveParser \input ->
        this =
            after
            |> skip eater
            |> until
        loop = \inp ->
            if inp |> List.len == 0 then
                Err (ParsingFailure "Ran out of content")
            else
                when this |> Parser.parsePartial inp is
                    Err (ParsingFailure _) -> loop (inp |> List.dropFirst 1)
                    Ok { val: val, input: rest } -> Ok { val: val, input: rest }

        loop input

## Runs the parser again and again until the parsing succeeds or we run out of input
retryTillEnd = \until ->
    Parser.buildPrimitiveParser \input ->
        loop = \inp ->
            if inp |> List.len == 0 then
                Err (ParsingFailure "Ran out of content")
            else
                when until |> Parser.parsePartial inp is
                    Err (ParsingFailure _) -> loop (inp |> List.dropFirst 1)
                    ok -> ok

        loop input

digits3max =
    const (\a -> a)
    |> keep (Parser.chompWhile \a -> a >= '0' && a <= '9')
    |> Parser.map \a ->
        if (a |> List.len) < 4 && (a |> List.len) > 0 then
            a |> Str.fromUtf8 |> Result.try Str.toU64 |> Result.mapErr \_ -> "not a number"
        else
            Err "too many digits"
    |> Parser.flatten



parseCmd =
    Parser.oneOf [
        const (\n1 -> \n2 -> Mul n1 n2)
        |> skip (string "mul(")
        |> keep digits3max
        |> skip (string ",")
        |> keep digits3max
        |> skip (string ")"),
        const (Dont)
        |> skip (string "don't()"),
        const (Do)
        |> skip (string "do()"),
    ]

parser = \input ->
    parseCmd
    |> retryTillEnd
    |> Parser.oneOrMore
    |> String.parseStrPartial input
    |> Result.map .val

part1f : Str -> Result Str Str
part1f = \input ->
    parsed = try (parser input |> Result.mapErr \a -> " parsing failed $(a |> Inspect.toStr)")
    parsed
    |> List.walk
        { total: 0 }
        (\{ total }, cmd ->
            when cmd is
                Mul a b -> { total: total + a * b }
                _ -> { total }
        )
    |> .total
    |> Inspect.toStr
    |> Ok

part2f : Str -> Result Str Str
part2f = \input ->
    parsed = try (parser input |> Result.mapErr \a -> " parsing failed $(a |> Inspect.toStr)")
    parsed
    |> List.walk
        { do: Bool.true, total: 0 }
        (\{ do, total }, cmd ->
            when cmd is
                Do -> { do: Bool.true, total }
                Dont -> { do: Bool.false, total }
                Mul a b -> if do then { do, total: total + a * b } else { do, total }
        )
    |> .total
    |> Inspect.toStr
    |> Ok

