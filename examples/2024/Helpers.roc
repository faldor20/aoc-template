module [unzip, slidingPairs,resultFromBool,isDigit0to9]
unzip = \list ->
    list
    |> List.walk
        ([], [])
        \(aList, bList), (a, b) ->
            (
                aList |> List.append (a),
                bList |> List.append (b),
            )

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
