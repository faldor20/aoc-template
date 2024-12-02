app [main!] {
    pf: platform "../../../basic-cli/platform/main.roc",
    aoc: "../../package/main.roc",
}

import pf.Stdout
import pf.File
import pf.Utc
import aoc.AoC {
    stdin!: \{} -> File.readBytes! "examples/input/2024_02.txt",
    stdout!: Stdout.write!,
    time!: \{} -> Utc.now! {} |> Utc.toMillisSinceEpoch,
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
# solver = \lines, unsafeThreshold ->
#     lines
#     |> List.keepOks \lineInp ->
#         solveLine = \line, canRemove ->
#             state = { lastLast: -10000, last: -1, decreasing: None, hadUnsafe: 0 }
#             line
#             |> List.walkTry state \stateVar, num ->
#                 checkSafe = \last, curr, wasDecreasing ->
#                     diff = Num.absDiff last curr
#                     decreasing = Some (Num.isLt curr last)
#                     changedDirection = wasDecreasing != None && (decreasing != wasDecreasing)

#                     (diff > 3 || diff < 1 || changedDirection, decreasing)
#                 if stateVar.last == -1 then
#                     Ok { stateVar & last: num }
#                 else
#                     this:{lastLast:Num _,hadUnsafe:_}_ ->_
#                     this = \st ->
#                         { lastLast, last, decreasing: wasDecreasing, hadUnsafe } = st
#                         (isSafe, decreasing) = checkSafe last num wasDecreasing
#                         if isSafe then
#                             if hadUnsafe + 1 > unsafeThreshold then
#                                 Err Unsafe
#                             else
#                                 when canRemove is
#                                     This ->
#                                         Ok { st & hadUnsafe: hadUnsafe + 1 }
#                                     Last ->
#                                         this { st& last: lastLast, hadUnsafe: hadUnsafe + 1 }
#                         else
#                             Ok { lastLast: last, last: num, decreasing, hadUnsafe }
#                     this stateVar
#         solveLine lineInp This |> Result.onErr \_ ->
#             solveLine lineInp Last|>Result.map(\x->
#                 dbg lineInp
#                 x)
checkSafe = \last, curr, wasDecreasing ->
    diff = Num.absDiff last curr
    decreasing = Some (Num.isLt curr last)
    changedDirection = wasDecreasing != None && (decreasing != wasDecreasing)

    (diff > 3 || diff < 1 || changedDirection, decreasing)
solver = \lines, unsafeThreshold ->
    lines
    |> List.keepOks \lineInp ->
        state = { last: -1, decreasing: None, hadUnsafe: 0 }
        helper = \items, st ->
            res =
                when items is
                    [] -> Ok {}
                    [num, .. as rest] ->
                        if st.last == -1 then
                            helper rest { st & last: num, decreasing: None }
                        else
                            { last, decreasing: wasDecreasing, hadUnsafe } = st
                            (notSafe, decreasing) = checkSafe last num wasDecreasing
                            if notSafe then
                                Err Unsafe
                            else
                                helper rest { last: num, decreasing, hadUnsafe }
            res
            |> Result.onErr \_ ->
                # If we have already tried to recover from an unsafe issue don't try again
                if st.hadUnsafe + 1 > unsafeThreshold then
                    Err Unsafe
                else
                # If we have already tried to recover from an unsafe issue don't try again
                    helper (items |> List.dropFirst 1) { st & hadUnsafe: st.hadUnsafe + 1 }
        helper lineInp state

part1f : Str -> Result Str Str
part1f = \input ->
    lines = parseInput input

    safeList = lines |> solver 0

    safeList |> List.len |> Num.toStr |> Ok

part2f : Str -> Result Str Str
part2f = \input ->
    lines = parseInput input

    safeList = lines |> solver 1

    safeList |> List.len |> Num.toStr |> Ok

