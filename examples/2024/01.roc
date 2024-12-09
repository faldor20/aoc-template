app [main!] {
    pf: platform"../../../basic-cli/platform/main.roc",
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

tagTest= \list -> 
    list|>List.walkUntil 0 \state, item->
        if item >10 then
         Continue (item-1)
        else
            Break 0

Bbbtest:[Bottt Str]

aast:Bbbtest-> _
aast= \a->
    {}  
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
    { before, after } = Str.splitFirst? x "   "
    a = Str.toI32? before
    b = Str.toI32? after
    Ok (a, b)

parseInput = \input ->
    input
    |> Str.splitOn "\n"
    |> List.keepOks parseLine
    |> unzip

# ========================
#    Solutions
# ========================

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
    endDict =
        snd |> List.walk (Dict.empty{}) \dict, id ->
         dict |> Dict.update id \maybeVal -> 
             when maybeVal is 
             Err _-> Ok 1
             Ok count->Ok (count + 1)
    sum =
        fst |> List.walk 0 \state, id ->
            count = (endDict |> Dict.get id |> Result.withDefault 0)
            state + (id * count)

    Ok (Num.toStr sum)

