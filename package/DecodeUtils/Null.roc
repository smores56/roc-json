module [nullChars, nullToEmpty, emptyToNull]

nullChars = "null" |> Str.toUtf8

## Returns `Null` if the input starts with "null"
## If makeNullEmpty is true Null{bytes} will be empty
nullToEmpty : List U8, Bool -> [Null _, NotNull]
nullToEmpty = \bytes, makeNullEmpty ->
    when bytes is
        ['n', 'u', 'l', 'l', .. as rest] ->
            if makeNullEmpty then
                Null { bytes: [], rest }
            else
                Null { bytes: nullChars, rest }

        _ -> NotNull

emptyToNull : List U8, Bool -> List U8
emptyToNull = \bytes, makeEmptyNull ->
    if bytes == [] && makeEmptyNull then
        nullChars
    else
        bytes
