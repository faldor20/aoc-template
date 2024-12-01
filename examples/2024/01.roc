app [main!] {
    pf: platform "../../../basic-cli/platform/main.roc",
    aoc: "../../package/main.roc",
}

import pf.Stdout
import pf.File
import pf.Utc
import aoc.AoC {
    stdin!: \{} -> File.readBytes! "examples/input/2024_01.txt",
    stdout!: Stdout.write!,
    time!: \{} -> Utc.now! {} |> Utc.toMillisSinceEpoch,
}

main! = \{} ->
    AoC.solve! {
        year: 2020,
        day: 1,
        title: "Report Repair",
        part1: part1f,
        part2: part2f,
    }

unzip = \list ->
    list
    |> List.walk
        ([], [])
        (\(aList, bList), (a, b) ->
            (
                aList |> List.append (a),
                bList |> List.append (b),
            )
        )

parseLine : _ -> Result _ _
parseLine = \x ->
    { before, after } = Str.splitFirst? x "   "
    a = Str.toI32? before
    b = Str.toI32? after
    Ok (a, b)

parseInput = \input ->
    input
    |> Str.splitOn "\n"
    |> List.keepOks parseLine
    |> unzip

part1f : Str -> Result Str Str
part1f = \input ->
    (fst, snd) = parseInput input

    fst2 = fst |> List.sortDesc
    snd2 = snd |> List.sortDesc

    diffList = List.map2 fst2 snd2 Num.absDiff
    totalDifference = diffList |> List.sum

    totalDifference |> Num.toStr |> Ok

part2f : Str -> Result Str Str
part2f = \input ->
    (fst, snd) = parseInput input
    startDict = fst |> List.map (\a -> (a, 0)) |> Dict.fromList
    endDict =
        snd |> List.walk startDict \dict, id ->
         dict |> Dict.update id \maybeVal -> 
             Result.map maybeVal \count -> count + 1
    sum =
        fst |> List.walk 0 \state, id ->
            count = (endDict |> Dict.get id |> Result.withDefault 0)
            state + (id * count)

    Ok (Num.toStr sum)

