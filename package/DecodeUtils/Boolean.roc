module [decodeBool]

decodeBool : List U8 -> { result : Result Bool [TooShort], rest : List U8 }
decodeBool = \bytes ->
    when bytes is
        ['f', 'a', 'l', 's', 'e', .. as rest] -> { result: Ok Bool.false, rest }
        ['t', 'r', 'u', 'e', .. as rest] -> { result: Ok Bool.true, rest }
        _ -> { result: Err TooShort, rest: bytes }

# Test decode of Bool
expect
    actual = "true\n" |> Str.toUtf8 |> decodeBool
    expected = Ok Bool.true
    actual.result == expected

# Test decode of Bool
expect
    actual = "false ]\n" |> Str.toUtf8 |> decodeBool
    expected = Ok Bool.false
    actual.result == expected
