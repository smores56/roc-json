module [
    openBracket,
    closingBracket,
    anything,
    comma,
    tryDecode,
    parseExactChar,
    eatWhitespace,
]

openBracket : List U8 -> DecodeResult {}
openBracket = \bytes -> parseExactChar bytes '['

closingBracket : List U8 -> DecodeResult {}
closingBracket = \bytes -> parseExactChar bytes ']'

anything : List U8 -> DecodeResult {}
anything = \bytes -> { result: Err TooShort, rest: bytes }

comma : List U8 -> DecodeResult {}
comma = \bytes -> parseExactChar bytes ','

tryDecode : DecodeResult a, ({ val : a, rest : List U8 } -> DecodeResult b) -> DecodeResult b
tryDecode = \{ result, rest }, mapper ->
    when result is
        Ok val -> mapper { val, rest }
        Err e -> { result: Err e, rest }

parseExactChar : List U8, U8 -> DecodeResult {}
parseExactChar = \bytes, char ->
    when List.get bytes 0 is
        Ok c ->
            if
                c == char
            then
                { result: Ok {}, rest: (List.split bytes 1).others }
            else
                { result: Err TooShort, rest: bytes }

        Err _ -> { result: Err TooShort, rest: bytes }

isWhitespace = \b ->
    when b is
        ' ' | '\n' | '\r' | '\t' -> Bool.true
        _ -> Bool.false

expect
    input = ['1', 'a', ' ', '\n', 0x0d, 0x09]
    actual = List.map input isWhitespace
    expected = [Bool.false, Bool.false, Bool.true, Bool.true, Bool.true, Bool.true]

    actual == expected

eatWhitespace : List U8 -> List U8
eatWhitespace = \bytes ->
    when bytes is
        [a, ..] if isWhitespace a -> eatWhitespace (List.dropFirst bytes 1)
        _ -> bytes

expect eatWhitespace (Str.toUtf8 "") == (Str.toUtf8 "")
expect eatWhitespace (Str.toUtf8 "ABC    ") == (Str.toUtf8 "ABC    ")
expect eatWhitespace (Str.toUtf8 "  \nABC    ") == (Str.toUtf8 "ABC    ")
