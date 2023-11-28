app "AoC"
    packages {
        pf: "https://github.com/roc-lang/basic-cli/releases/download/0.6.2/c7T4Hp8bAdWz3r9ZrhboBzibCjJag8d0IP_ljb42yVc.tar.br",
    }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Tty,
        pf.Task.{ Task },
        ANSI.{ Color, Input },
        pf.Utc.{ Utc },
        # App,
    ]
    provides [main, debugScreen] to pf

ScreenSize : { width : I32, height : I32 }
Position : { row : I32, col : I32 }

Model : {
    screen : ScreenSize,
    cursor : Position,
    puzzles : List Str,
    prevDraw : Utc,
    currDraw : Utc,
    inputs : List Input,
}

init : Model
init = {
    cursor: { row: 3, col: 3 },
    screen: { width: 0, height: 0 },
    puzzles: [
        "2022 Day 1: Calorie Counting",
        "2022 Day 2: Rock Paper Scissors",
        "2022 Day 3: Rucksack Reorganization",
    ],
    prevDraw: Utc.fromMillisSinceEpoch 0,
    currDraw: Utc.fromMillisSinceEpoch 0,
    inputs: List.withCapacity 1000,
}

render : Model -> List DrawFn
render = \state ->
    [
        homeScreen state,
        debugScreen state,
    ]
    |> List.join

main : Task {} *
main = runTask |> Task.onErr \_ -> Stdout.line "ERROR Something went wrong"

runTask : Task {} []
runTask =

    # Enable TTY Raw mode
    {} <- Tty.enableRawMode |> Task.await

    # Run App Loop
    _ <- Task.loop init runLoop |> Task.await

    # Restore TTY Mode
    {} <- Tty.disableRawMode |> Task.await

    # Exit
    Task.ok {}

# TODO ADD PUZZLE STUFF BACK IN
# when App.solvePuzzle { year: 2022, day: 1, puzzle: Part1 } is
#     Ok answer ->
#         header = Color.fg "Advent of Code Solution" Green
#         year = Color.fg "\(Num.toStr 2022)" Green
#         day = Color.fg "\(Num.toStr 1)" Green
#         part = Color.fg "1" Green
#         time = Color.fg "245ms" Green

#         """

#         --- \(header)
#         year: \(year)
#         day: \(day)
#         part: \(part)
#         time: \(time)
#         answer:

#         \(answer)
#         ---
#         """
#         |> Stdout.line

#     Err NotImplemented ->
#         [
#             Color.fg "Advent of Code" Green,
#             ":",
#             Color.fg "\(Num.toStr 2022)-\(Num.toStr 1)-Part 1" Blue,
#             ":",
#             Color.fg "NOT IMPLEMENTED" Red,
#         ]
#         |> Str.joinWith ""
#         |> Stdout.line

#     Err (Error msg) ->
#         [
#             Color.fg "Advent of Code" Green,
#             ":",
#             Color.fg "\(Num.toStr 2022)-\(Num.toStr 1)-Part 1" Blue,
#             ":",
#             Color.fg "ERROR \(msg)" Red,
#         ]
#         |> Str.joinWith ""
#         |> Stdout.line

runLoop : Model -> Task [Step Model, Done Model] []
runLoop = \prevState ->

    # Get the time for this draw
    now <- Utc.now |> Task.await

    # Update screen size (in case it was resized since last draw)
    terminalSize <- getTerminalSize |> Task.await

    # Update State for this draw
    state = { prevState & screen: terminalSize, prevDraw: prevState.currDraw, currDraw: now }

    # Draw the screen
    drawFns = render state
    {} <- drawScreen state drawFns |> Task.await

    # Get user input
    input <- Stdin.bytes |> Task.map ANSI.parseRawStdin |> Task.await

    # Parse input into command
    command =
        when input is
            KeyPress Up -> MoveCursor Up
            KeyPress Down -> MoveCursor Down
            KeyPress Left -> MoveCursor Left
            KeyPress Right -> MoveCursor Right
            KeyPress Escape -> Exit
            CtrlC -> Exit
            Unsupported bytes -> Crash bytes
            KeyPress _ -> Nothing

    # Update state too keep a history of inputs
    stateWithInput = { state & inputs: List.append state.inputs input }

    # Handle input
    when command is
        Nothing -> Task.ok (Step stateWithInput)
        MoveCursor direction -> Task.ok (Step (updateCursor stateWithInput direction))
        Exit -> Task.ok (Done stateWithInput)
        Crash _ -> Task.ok (Step stateWithInput)

DrawFn : Position, Position -> Result Pixel {}
Pixel : { char : Str, fg : Color, bg : Color }

# Loop through each pixel in screen and build up a single string to write to stdout
drawScreen : {cursor: Position, screen : ScreenSize}*, List DrawFn -> Task {} []
drawScreen = \{cursor, screen}, drawFns ->
    pixels =
        row <- List.range { start: At 0, end: Before screen.height } |> List.map
        col <- List.range { start: At 0, end: Before screen.width } |> List.map

        List.walkUntil
            drawFns
            { char: " ", fg: Default, bg: Default }
            \defaultPixel, drawFn ->
                when drawFn cursor { row, col } is
                    Ok pixel -> Break pixel
                    Err _ -> Continue defaultPixel

    pixels
    |> joinAllPixels
    |> Stdout.write

joinAllPixels : List (List Pixel) -> Str
joinAllPixels = \rows ->
    List.walkWithIndex
        rows
        {
            char: " ",
            fg: Default,
            bg: Default,
            lines: List.withCapacity (List.len rows),
        }
        joinPixelRow
    |> .lines
    |> Str.joinWith ""

joinPixelRow : { char : Str, fg : Color, bg : Color, lines : List Str }, List Pixel, Nat -> { char : Str, fg : Color, bg : Color, lines : List Str }
joinPixelRow = \{ char, fg, bg, lines }, pixelRow, row ->

    { rowStrs, prev } =
        List.walk
            pixelRow
            { rowStrs: List.withCapacity (List.len pixelRow), prev: { char, fg, bg } }
            joinPixels

    line =
        rowStrs
        |> Str.joinWith "" # Set cursor at the start of line we want to draw
        |> Str.withPrefix (ANSI.toStr (SetCursor { row: Num.toI32 (row + 1), col: 0 }))

    { char: " ", fg: prev.fg, bg: prev.bg, lines: List.append lines line }

joinPixels : { rowStrs : List Str, prev : Pixel }, Pixel -> { rowStrs : List Str, prev : Pixel }
joinPixels = \{ rowStrs, prev }, curr ->
    pixelStr =
        # Prepend an ASCII escape ONLY if there is a change between pixels
        curr.char
        |> \str -> if curr.fg != prev.fg then Str.concat (ANSI.toStr (SetFgColor curr.fg)) str else str
        |> \str -> if curr.bg != prev.bg then Str.concat (ANSI.toStr (SetBgColor curr.bg)) str else str

    { rowStrs: List.append rowStrs pixelStr, prev: curr }

drawBox : { r : I32, c : I32, w : I32, h : I32, fg ? Color, bg ? Color, char ? Str } -> DrawFn
drawBox = \{ r, c, w, h, fg ? Gray, bg ? Default, char ? "#" } -> \_, { row, col } ->

        startRow = r
        endRow = (r + h)
        startCol = c
        endCol = (c + w)

        if row == r && (col >= startCol && col < endCol) then
            Ok { char, fg, bg } # TOP BORDER
        else if row == (r + h - 1) && (col >= startCol && col < endCol) then
            Ok { char, fg, bg } # BOTTOM BORDER
        else if col == c && (row >= startRow && row < endRow) then
            Ok { char, fg, bg } # LEFT BORDER
        else if col == (c + w - 1) && (row >= startRow && row < endRow) then
            Ok { char, fg, bg } # RIGHT BORDER
        else
            Err {}

drawVLine : { r : I32, c : I32, len : I32, fg ? Color, bg ? Color, char ? Str } -> DrawFn
drawVLine = \{ r, c, len, fg ? Default, bg ? Default, char ? "|" } -> \_, { row, col } ->
        if col == c && (row >= r && row < (r + len)) then
            Ok { char, fg, bg }
        else
            Err {}

drawHLine : { r : I32, c : I32, len : I32, fg ? Color, bg ? Color, char ? Str } -> DrawFn
drawHLine = \{ r, c, len, fg ? Default, bg ? Default, char ? "-" } -> \_, { row, col } ->
        if row == r && (col >= c && col < (c + len)) then
            Ok { char, fg, bg }
        else
            Err {}

drawCursor : { fg ? Color, bg ? Color, char ? Str } -> DrawFn
drawCursor = \{ fg ? Default, bg ? Gray, char ? " " } -> \cursor, { row, col } ->
        if
            (row == cursor.row) && (col == cursor.col)
        then
            Ok { char, fg, bg }
        else
            Err {}

drawText : Str, { r : I32, c : I32, fg ? Color, bg ? Color } -> DrawFn
drawText = \text, { r, c, fg ? Default, bg ? Default } -> \_, pixel ->
        bytes = Str.toUtf8 text
        len = text |> Str.toUtf8 |> List.len |> Num.toI32
        if pixel.row == r && pixel.col >= c && pixel.col < (c + len) then
            bytes
            |> List.get (Num.toNat (pixel.col - c))
            |> Result.try \b -> Str.fromUtf8 [b]
            |> Result.map \char -> { char, fg, bg }
            |> Result.mapErr \_ -> {}
        else
            Err {}

updateCursor : Model, [Up, Down, Left, Right] -> Model
updateCursor = \state, direction ->
    when direction is
        Up ->
            { state &
                cursor: {
                    row: ((state.cursor.row + state.screen.height - 1) % state.screen.height),
                    col: state.cursor.col,
                },
            }

        Down ->
            { state &
                cursor: {
                    row: ((state.cursor.row + 1) % state.screen.height),
                    col: state.cursor.col,
                },
            }

        Left ->
            { state &
                cursor: {
                    row: state.cursor.row,
                    col: ((state.cursor.col + state.screen.width - 1) % state.screen.width),
                },
            }

        Right ->
            { state &
                cursor: {
                    row: state.cursor.row,
                    col: ((state.cursor.col + 1) % state.screen.width),
                },
            }

parseCursorPosition : List U8 -> Position
parseCursorPosition = \bytes ->
    { val: row, rest: afterFirst } = takeNumber { val: 0, rest: List.dropFirst bytes 2 }
    { val: col } = takeNumber { val: 0, rest: List.dropFirst afterFirst 1 }

    { row, col }

# test "ESC[33;1R"
expect parseCursorPosition [27, 91, 51, 51, 59, 49, 82] == { col: 1, row: 33 }

takeNumber : { val : I32, rest : List U8 } -> { val : I32, rest : List U8 }
takeNumber = \in ->
    when in.rest is
        [a, ..] if a == '0' -> takeNumber { val: in.val * 10 + 0, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '1' -> takeNumber { val: in.val * 10 + 1, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '2' -> takeNumber { val: in.val * 10 + 2, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '3' -> takeNumber { val: in.val * 10 + 3, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '4' -> takeNumber { val: in.val * 10 + 4, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '5' -> takeNumber { val: in.val * 10 + 5, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '6' -> takeNumber { val: in.val * 10 + 6, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '7' -> takeNumber { val: in.val * 10 + 7, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '8' -> takeNumber { val: in.val * 10 + 8, rest: List.dropFirst in.rest 1 }
        [a, ..] if a == '9' -> takeNumber { val: in.val * 10 + 9, rest: List.dropFirst in.rest 1 }
        _ -> in

expect takeNumber { val: 0, rest: [27, 91, 51, 51, 59, 49, 82] } == { val: 0, rest: [27, 91, 51, 51, 59, 49, 82] }
expect takeNumber { val: 0, rest: [51, 51, 59, 49, 82] } == { val: 33, rest: [59, 49, 82] }
expect takeNumber { val: 0, rest: [49, 82] } == { val: 1, rest: [82] }

getTerminalSize : Task ScreenSize []
getTerminalSize =
    [
        SetCursor { row: 999, col: 999 },
        GetCursor,
    ]
    |> List.map ANSI.toStr
    |> Str.joinWith ""
    |> Stdout.write
    |> Task.await \{} -> Stdin.bytes
    |> Task.map parseCursorPosition
    |> Task.map \{ row, col } -> { width: col, height: row }

homeScreen : Model -> List DrawFn
homeScreen = \state ->
    [
        [
            drawCursor { bg: Green },
            drawText " Advent of Code Solutions" { r: 1, c: 1, fg: Green },
            drawText " ENTER TO RUN, ESCAPE TO QUIT" { r: 2, c: 1, fg: Gray },
            drawBox { r: 0, c: 0, w: state.screen.width, h: state.screen.height },
        ],
        (
            List.mapWithIndex state.puzzles \puzzleStr, idx ->
                row = 3 + (Num.toI32 idx)
                if (state.cursor.row == row) then
                    # Selected puzzle
                    drawText " - \(puzzleStr)" { r: row, c: 2, fg: Green }
                else
                    drawText " - \(puzzleStr)" { r: row, c: 2, fg: Black }
        ),
    ]
    |> List.join

debugScreen : Model -> List DrawFn
debugScreen = \state ->
    cursorStr = "CURSOR R\(Num.toStr state.cursor.row), C\(Num.toStr state.cursor.col)"
    screenStr = "SCREEN H\(Num.toStr state.screen.height), W\(Num.toStr state.screen.width)"
    inputDelatStr = "DELTA \(Num.toStr (Utc.deltaAsMillis state.prevDraw state.currDraw)) millis"
    lastInput = 
        state.inputs 
        |> List.last 
        |> Result.map ANSI.inputToStr  
        |> Result.map \str -> "INPUT \(str)"
        |> Result.withDefault "NO INPUT YET"

    [
        drawText lastInput { r: state.screen.height - 5, c: 1, fg: Magenta },
        drawText inputDelatStr { r: state.screen.height - 4, c: 1, fg: Magenta },
        drawText cursorStr { r: state.screen.height - 3, c: 1, fg: Magenta },
        drawText screenStr { r: state.screen.height - 2, c: 1, fg: Magenta },
        drawVLine { r: 1, c: state.screen.width // 2, len: state.screen.height, fg: Gray },
        drawHLine { c: 1, r: state.screen.height // 2, len: state.screen.width, fg: Gray },
    ]