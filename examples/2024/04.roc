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
import Helpers

import aoc.AoC {
    stdin!: \{} -> File.readBytes! "examples/input/2024_04.txt",
    stdout!: Stdout.write!,
    time!: \{} -> Utc.now! {} |> Utc.toNanosSinceEpoch,
}

main! = \{} ->
    AoC.solve! {
        year: 2024,
        day: 4,
        title: "Red-Nosed Reports ",
        part1: part1f,
        part2: part2f,
    }
# ========================
#    Input parsing
# ========================
getI = \list, i ->
    when list |> List.get i is
        Ok a -> a
        Err _ -> crash "out of bounds"

getOffset : I64, I64, I64, I64 -> I64
getOffset = \start, lineLen, x, y ->
    start + x + (y * lineLen)

expect
    inp =
        """
        1234567
        abcdefg
        """
        |> Str.replaceEach "\n" ""
        |> Str.toUtf8
    idx = getOffset 2 7 2 1 |> Num.toU64
    val = inp |> getI idx
    expectedV = 'e'
    val == expectedV

sampleInp =
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAXM
    ASAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """

cellsStarts : List (I64, I64)
cellsStarts = [(1, 0), (1, -1), (-1, 0), (-1, -1), (0, -1), (-1, 1), (0, 1), (1, 1)]

#generates all the possible serch paths eg: [(0,1),(0,2),(0,3)]
makeCells = \cells ->
    cells
    |> List.map \(x, y) ->
        List.range { start: At 1, end: At 3 }
        |> List.map \idx ->
            (x * idx, y * idx)

expected = ['X', 'M', 'A', 'S']
checkCell = \myCell, lineLen, input, startIdx ->
    currLine = Num.divTrunc startIdx lineLen
    myCell
    |> List.walkUntil { i: 1, res: 0 } \{ i }, (x, y) ->
        idx = getOffset startIdx lineLen x y
        thisLine = Num.divTrunc idx lineLen
        hasWrapped = (currLine + y != thisLine)
        if idx |> Num.isNegative || hasWrapped then
            Break { i, res: 0 }
        else
            char = input |> List.get (idx |> Num.toU64)
            when char is
                Ok char2 ->
                    if char2 == (expected |> getI i) then
                        Continue { i: i + 1, res: 1 }
                    else
                        Break { i: i + 1, res: 0 }

                Err _ -> Break { i, res: 0 }

    |> .res

parseInp=\input->
    data_ = input |> Str.toUtf8
    lineLen = (data_ |> List.findFirstIndex (\a -> a == '\n') |> Result.withDefault 0 |> Num.toI64)
    data = data_ |> List.keepIf \a -> a != '\n'
    (data,lineLen)
part1f : Str -> Result Str Str
part1f = \input ->
    cells = makeCells cellsStarts

    (data,lineLen)=parseInp input    
    len = List.len data
    loop = \i, count ->
        if i < len then
            if data |> getI i == 'X' then
                found =
                    cells
                    |> List.walk 0 \thisCount, cell ->
                        thisCount + (cell |> checkCell lineLen data (i |> Num.toI64))
                loop (i + 1) (count + found)
            else
                loop (i + 1) count
        else
            count

    result = loop 0 0
    Ok (result |> Num.toStr)

# cellsStarts = [(1, 0), (1, -1), (-1, 0), (-1, -1), (0, -1), (-1, 1), (0, 1), (1, 1)]

rotate=\lst-> 
    lst
    |>List.append (lst|>getI 0)
    |>List.dropFirst 1

getCellContent= \myCell, lineLen, input, startIdx ->
    currLine = Num.divTrunc startIdx lineLen
    out=
        myCell|>List.walkUntil {i:0,res:[]} \{i,res},(x,y)-> 
            idx = getOffset startIdx lineLen x y
            thisLine = Num.divTrunc idx lineLen
            hasWrapped = (currLine + y != thisLine)
            if idx |> Num.isNegative || hasWrapped then
                Break { i, res: [] }
            else
                char = input |> List.get (idx |> Num.toU64)
                when char is
                    Ok char2 ->
                        if !(char2 == 'M' || char2 == 'S') then
                            Break {i,res:[]}
                        else
                            Continue {i,res:res|>List.append char2 }

                    Err _ -> Break { i, res: [] }
    if out.res ==[] then Err No else Ok out.res



part2f : Str -> Result Str Str
part2f = \input ->
    (data,lineLen)=parseInp input

    xCell= [ (1, -1), (-1, -1), (-1, 1), (1, 1)]
    a=
        ['M','M','S','S']
    b=a|>rotate
    c=b|>rotate
    d=c|>rotate
    checks=[a,b,c,d]

    res=
        data|>List.mapWithIndex \v,i-> 
            if v =='A'then
                xCell
                |>getCellContent lineLen data  (i|>Num.toI64)
                |>Result.map \chars->if checks|>List.any \x->x==chars then 1 else 0
                |>Result.withDefault 0
            else 0

    res|>List.sum|>Num.toStr|>Ok




expect
    actual = part1f sampleInp
    expects = Ok "18"
    expects == actual

expect
    actual = part2f sampleInp
    expects = Ok "9"
    expects == actual

