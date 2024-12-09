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

# Checks if a sequence of levels is valid or not in a fairly efficent manner
# Will walk forward checking for safe levels untill it finds an unsfae level which will cause an error
# In the case of an error we will try to continue skipping the current level
# Becasue this is recursive if an error occurs and recovery doesn't work it will bubble up to the previous iteration and we will try to recover from there
# eg: 1 2 3 2 4
# (1 2 4 3) 4 6 -we process these levels and then hit an error (3)
# (1 2 4 4) 6 -we skip the current value (3) but we then have another error value (4)
# (1 2 3 4 6) -we move back one iteration, skip the previous value  (4) and then continue to the end
# In the worst case we would walk all the way back to the start. Although we could cap it at 3 steps back, which is the most we would need to find a solution if one exists
solver = \lines, unsafeThreshold ->
    state = { last: -1, decreasing: None, hadUnsafe: 0 }
    solveLine = \items, st ->
        { last, decreasing: wasDecreasing, hadUnsafe } = st
        attempt =
            when items is
                [] -> Ok {}
                [num, .. as rest] ->
                    # Handle first level
                    if last == -1 then
                        solveLine rest { st & last: num, decreasing: None }
                    else
                        (notSafe, decreasing) = checkSafe last num wasDecreasing
                        if notSafe then
                            Err Unsafe
                        else
                            solveLine rest { last: num, decreasing, hadUnsafe }
        attempt
        |> Result.onErr \_ ->
            # If we have already tried to recover from an unsafe issue don't try again
            if hadUnsafe + 1 > unsafeThreshold then
                Err Unsafe
            else
                # We will try to recover from the unsafe state by skipping the current level and then walking forward again.
                solveLine (items |> List.dropFirst 1) { st & hadUnsafe: hadUnsafe + 1 }

    lines
    |> List.keepOks \lineInp -> solveLine lineInp state
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

solverSimple = \lines ->

    solveLevel = \levels ->
        diffs = levels |> slidingPairs \a, b ->  b-a
        direction = if diffs |> List.first |> Result.withDefault 0 |> Num.isPositive then Num.isPositive else Num.isNegative
        diffs |> List.all (\a -> direction(a) && Num.abs(a) <= 3 && a != 0)

    permutations = \line ->
        List.range { start: At 0, end: Before (List.len line) }
        |> List.map \idx -> line |> List.dropAt idx

    lines
    |> List.countIf\a -> a |> permutations |> List.any solveLevel

part1f : Str -> Result Str Str
part1f = \input ->
    lines = parseInput input

    safeList = lines |> solver 0

    safeList |> List.len |> Num.toStr |> Ok

part2f : Str -> Result Str Str
part2f = \input ->
    lines = parseInput input

    # safeList = lines |> solver 1|>List.len
    safeList = lines |> solverSimple

    # expect safeList==safeListCheck
    

    safeList |> Num.toStr |> Ok

