interface AoC
    exposes [Solution]
    imports []

Solution : {
    year : U64,
    day : U64,
    title : Str,
    part1 : {} -> Result Str [NotImplemented, Error Str],
    part2 : {} -> Result Str [NotImplemented, Error Str],
}