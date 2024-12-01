module { stdin!, stdout!, time! } -> [Solution, solve!]

Solution err : {
    year : U64,
    day : U64,
    title : Str,
    part1 : Str -> Result Str err,
    part2 : Str -> Result Str err,
} where err implements Inspect

solve! : Solution err => Result {} _
solve! = \{ year, day, title, part1, part2 } ->
    try
        stdout!
        (
            Str.joinWith
                [
                    green "--- ADVENT OF CODE ",
                    green "$(Num.toStr year)-$(Num.toStr day): $(title)",
                    green " ---\n\n",
                    blue "INPUT:\n",
                    "Reading input from STDIN...\n\n",
                ]
                ""
        )

    startRead = time! {}

    input : Str
    input =
        try
            (
                inp = try stdin! {}
                inp
                |> \bytes ->
                    Str.fromUtf8 bytes
                    |> Result.mapErr \_ -> InvalidUtf8Input
            )

    endRead = time! {}

    startPart1 = time! {}

    solutionPart1 : Result Str _
    solutionPart1 = part1 input

    endPart1 = time! {}

    part1Task =
        when solutionPart1 is
            Ok str -> stdout! (Str.joinWith [blue "PART 1:\n", "$(str)\n\n"] "")
            Err err ->
                stdout!
                    (
                        Str.joinWith
                            [
                                red "PART 1 ",
                                red "ERROR:\n",
                                "$(Inspect.toStr err)\n\n",
                            ]
                            ""
                    )

    startPart2 = time! {}

    solutionPart2 : Result Str _
    solutionPart2 = part2 input

    endPart2 = time! {}

    part2Task =
        when solutionPart2 is
            Ok str -> stdout! (Str.joinWith [blue "PART 2:\n", "$(str)\n\n"] "")
            Err err ->
                stdout!
                    (
                        Str.joinWith
                            [
                                red "PART 2 ",
                                red "ERROR:\n",
                                "$(Inspect.toStr err)\n\n",
                            ]
                            ""
                    )

    readMillis = if (endRead - startRead) < 1 then "<1" else Num.toStr (endRead - startRead)
    part1Millis = if (endPart1 - startPart1) < 1 then "<1" else Num.toStr (endPart1 - startPart1)
    part2Millis = if (endPart2 - startPart2) < 1 then "<1" else Num.toStr (endPart2 - startPart2)
    try
        stdout!
        (
            Str.joinWith
                [
                    "reding took: $(readMillis)ms",
                    "part1 took: $(part1Millis)ms",
                    "part2 took: $(part2Millis)ms",
                ]
                "\n"
        )

    Ok {}

output = { solve! }

blue = \str -> "\u(001b)[0;34m$(str)\u(001b)[0m"
green = \str -> "\u(001b)[0;32m$(str)\u(001b)[0m"
red = \str -> "\u(001b)[0;31m$(str)\u(001b)[0m"
