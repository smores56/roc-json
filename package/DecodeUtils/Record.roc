module [decodeList]

import DecodeUtils.String exposing [decodeString]

# JSON OBJECTS -----------------------------------------------------------------

decodeRecord = \initialState, stepField, finalizer -> Decode.custom \bytes, @Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull } ->

        # Recursively build up record from object field:value pairs
        decodeFields = \recordState, bytesBeforeField ->

            # Decode the JSON string field name
            { result: objectNameResult, rest: bytesAfterField } =
                Decode.decodeWith bytesBeforeField decodeString utf8

            # Count the bytes until the field value
            countBytesBeforeValue =
                when List.walkUntil bytesAfterField (BeforeColon 0) objectHelp is
                    AfterColon n -> n
                    _ -> 0

            valueBytes = List.dropFirst bytesAfterField countBytesBeforeValue

            when objectNameResult is
                Err TooShort ->
                    # Invalid object, unable to decode field name or find colon ':'
                    # after field and before the value
                    { result: Err TooShort, rest: bytes }

                Ok objectName ->
                    # Decode the json value
                    { val: updatedRecord, rest: bytesAfterValue } <-
                        (
                            fieldName =
                                fromObjectNameUsingMap objectName fieldNameMapping

                            # Retrieve value decoder for the current field
                            when (stepField recordState fieldName, skipMissingProperties) is
                                (Skip, shouldSkip) if shouldSkip == Bool.true ->
                                    # Count the bytes until the field value
                                    countBytesBeforeNextField =
                                        when List.walkUntil valueBytes (FieldValue 0) skipFieldHelp is
                                            FieldValueEnd n -> n
                                            _ -> 0

                                    dropedValueBytes = List.dropFirst valueBytes countBytesBeforeNextField

                                    { result: Ok recordState, rest: dropedValueBytes }

                                (Skip, _) ->
                                    { result: Ok recordState, rest: valueBytes }

                                (Keep valueDecoder, _) ->
                                    # Decode the value using the decoder from the recordState
                                    decodePotentialNull valueBytes valueDecoder (@Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull })
                        )
                        |> tryDecode

                    # Check if another field or '}' for end of object
                    when List.walkUntil bytesAfterValue (AfterObjectValue 0) objectHelp is
                        ObjectFieldNameStart n ->
                            rest = List.dropFirst bytesAfterValue n

                            # Decode the next field and value
                            decodeFields updatedRecord rest

                        AfterClosingBrace n ->
                            rest = List.dropFirst bytesAfterValue n

                            # Build final record from decoded fields and values
                            when finalizer updatedRecord utf8 is
                                ## This step is where i can implement my special decoding of options
                                Ok val -> { result: Ok val, rest }
                                Err e ->
                                    { result: Err e, rest }

                        _ ->
                            # Invalid object
                            { result: Err TooShort, rest: bytesAfterValue }

        countBytesBeforeFirstField =
            when List.walkUntil bytes (BeforeOpeningBrace 0) objectHelp is
                ObjectFieldNameStart n -> n
                _ -> 0

        if countBytesBeforeFirstField == 0 then
            # Invalid object, expected opening brace '{' followed by a field
            { result: Err TooShort, rest: bytes }
        else
            bytesBeforeFirstField = List.dropFirst bytes countBytesBeforeFirstField

            # Begin decoding field:value pairs
            decodeFields initialState bytesBeforeFirstField

skipFieldHelp : SkipValueState, U8 -> [Break SkipValueState, Continue SkipValueState]
skipFieldHelp = \state, byte ->
    when (state, byte) is
        (FieldValue n, b) if b == '}' -> Break (FieldValueEnd n)
        (FieldValue n, b) if b == '[' -> Continue (InsideAnArray { index: (n + 1), nesting: 0 })
        (FieldValue n, b) if b == '{' -> Continue (InsideAnObject { index: (n + 1), nesting: 0 })
        (FieldValue n, b) if b == '"' -> Continue (InsideAString (n + 1))
        (FieldValue n, b) if b == ',' -> Break (FieldValueEnd (n))
        (FieldValue n, _) -> Continue (FieldValue (n + 1))
        # strings
        (InsideAString n, b) if b == '\\' -> Continue (Escaped (n + 1))
        (Escaped n, _) -> Continue (InsideAString (n + 1))
        (InsideAString n, b) if b == '"' -> Continue (FieldValue (n + 1))
        (InsideAString n, _) -> Continue (InsideAString (n + 1))
        # arrays
        (InsideAnArray { index, nesting }, b) if b == '"' -> Continue (StringInArray { index: index + 1, nesting })
        (InsideAnArray { index, nesting }, b) if b == '[' -> Continue (InsideAnArray { index: index + 1, nesting: nesting + 1 })
        (InsideAnArray { index, nesting }, b) if nesting == 0 && b == ']' -> Continue (FieldValue (index + 1))
        (InsideAnArray { index, nesting }, b) if b == ']' -> Continue (InsideAnArray { index: index + 1, nesting: nesting - 1 })
        (InsideAnArray { index, nesting }, _) -> Continue (InsideAnArray { index: index + 1, nesting })
        # arrays escape strings
        (StringInArray { index, nesting }, b) if b == '\\' -> Continue (EcapdedStringInArray { index: index + 1, nesting })
        (EcapdedStringInArray { index, nesting }, _) -> Continue (StringInArray { index: index + 1, nesting })
        (StringInArray { index, nesting }, b) if b == '"' -> Continue (InsideAnArray { index: index + 1, nesting })
        (StringInArray { index, nesting }, _) -> Continue (StringInArray { index: index + 1, nesting })
        # objects
        (InsideAnObject { index, nesting }, b) if b == '"' -> Continue (StringInObject { index: index + 1, nesting })
        (InsideAnObject { index, nesting }, b) if b == '{' -> Continue (InsideAnObject { index: index + 1, nesting: nesting + 1 })
        (InsideAnObject { index, nesting }, b) if nesting == 0 && b == '}' -> Continue (FieldValue (index + 1))
        (InsideAnObject { index, nesting }, b) if b == '}' -> Continue (InsideAnObject { index: index + 1, nesting: nesting - 1 })
        (InsideAnObject { index, nesting }, _) -> Continue (InsideAnObject { index: index + 1, nesting })
        # objects escape strings
        (StringInObject { index, nesting }, b) if b == '\\' -> Continue (EncodedStringInObject { index: index + 1, nesting })
        (EncodedStringInObject { index, nesting }, _) -> Continue (StringInObject { index: index + 1, nesting })
        (StringInObject { index, nesting }, b) if b == '"' -> Continue (InsideAnObject { index: index + 1, nesting })
        (StringInObject { index, nesting }, _) -> Continue (StringInObject { index: index + 1, nesting })
        _ -> Break InvalidObject

SkipValueState : [
    FieldValue U64,
    FieldValueEnd U64,
    InsideAString U64,
    InsideAnObject { index : U64, nesting : U64 },
    StringInObject { index : U64, nesting : U64 },
    EncodedStringInObject { index : U64, nesting : U64 },
    InsideAnArray { index : U64, nesting : U64 },
    StringInArray { index : U64, nesting : U64 },
    EcapdedStringInArray { index : U64, nesting : U64 },
    Escaped U64,
    InvalidObject,
]

# Test decode of partial record
expect
    input = Str.toUtf8 "{\"extraField\":2, \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record in list additional field last
expect
    input = Str.toUtf8 "[{\"ownerName\": \"Farmer Joe\", \"extraField\":2}]"
    actual : DecodeResult (List { ownerName : Str })
    actual = decodeRecord actual

    expected = Ok [{ ownerName: "Farmer Joe" }]

    result = actual.result
    result == expected

# Test decode of partial record in record partial field last
expect
    input = Str.toUtf8 "{\"value\": {\"ownerName\": \"Farmer Joe\",\"extraField\":2}}"
    actual : DecodeResult { value : { ownerName : Str } }
    actual = decodeRecord actual

    expected = Ok { value: { ownerName: "Farmer Joe" } }

    result = actual.result
    result == expected

# Test decode of partial record in partial record additional fields last
expect
    input = Str.toUtf8 "{\"value\": {\"ownerName\": \"Farmer Joe\", \"extraField\":2}, \"extraField\":2}"
    actual : DecodeResult { value : { ownerName : Str } }
    actual = decodeRecord actual

    expected = Ok { value: { ownerName: "Farmer Joe" } }

    result = actual.result
    result == expected

# Test decode of partial record with multiple additional fields
expect
    input = Str.toUtf8 "{\"extraField\":2, \"ownerName\": \"Farmer Joe\", \"extraField2\":2 }"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with string value
expect
    input = Str.toUtf8 "{\"extraField\": \"abc\", \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with string value with a comma
expect
    input = Str.toUtf8 "{\"extraField\": \"a,bc\", \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with string value with an escaped "
expect
    input = Str.toUtf8 "{\"extraField\": \"a\\\"bc\", \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with an array
expect
    input = Str.toUtf8 "{\"extraField\": [1,2,3], \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with a nested array
expect
    input = Str.toUtf8 "{\"extraField\": [1,[4,5,[[9],6,7]],3], \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with a nested array with strings inside
expect
    input = Str.toUtf8 "{\"extraField\": [\"a\", [\"bc]]]def\"]], \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with a nested array with escaped strings inside
expect
    input = Str.toUtf8 "{\"extraField\": [\"a\", [\"b\\cdef\"]], \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with an object
expect
    input = Str.toUtf8 "{\"extraField\": { \"fieldA\": 6 }, \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with a nested object
expect
    input = Str.toUtf8 "{\"extraField\": { \"fieldA\": 6, \"nested\": { \"nestField\": \"abcd\" } }, \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with a nested object and string
expect
    input = Str.toUtf8 "{\"extraField\": { \"fieldA\": 6, \"nested\": { \"nestField\": \"ab}}}}}cd\" } }, \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

# Test decode of partial record with a nested object and string ending with an escaped char
expect
    input = Str.toUtf8 "{\"extraField\": { \"fieldA\": 6, \"nested\": { \"nestField\": \"ab\\cd\" } }, \"ownerName\": \"Farmer Joe\"}"
    actual : DecodeResult { ownerName : Str }
    actual = decodeRecord actual

    expected = Ok { ownerName: "Farmer Joe" }

    result = actual.result
    result == expected

objectHelp : ObjectState, U8 -> [Break ObjectState, Continue ObjectState]
objectHelp = \state, byte ->
    when (state, byte) is
        (BeforeOpeningBrace n, b) if isWhitespace b -> Continue (BeforeOpeningBrace (n + 1))
        (BeforeOpeningBrace n, b) if b == '{' -> Continue (AfterOpeningBrace (n + 1))
        (AfterOpeningBrace n, b) if isWhitespace b -> Continue (AfterOpeningBrace (n + 1))
        (AfterOpeningBrace n, b) if b == '"' -> Break (ObjectFieldNameStart n)
        (BeforeColon n, b) if isWhitespace b -> Continue (BeforeColon (n + 1))
        (BeforeColon n, b) if b == ':' -> Continue (AfterColon (n + 1))
        (AfterColon n, b) if isWhitespace b -> Continue (AfterColon (n + 1))
        (AfterColon n, _) -> Break (AfterColon n)
        (AfterObjectValue n, b) if isWhitespace b -> Continue (AfterObjectValue (n + 1))
        (AfterObjectValue n, b) if b == ',' -> Continue (AfterComma (n + 1))
        (AfterObjectValue n, b) if b == '}' -> Continue (AfterClosingBrace (n + 1))
        (AfterComma n, b) if isWhitespace b -> Continue (AfterComma (n + 1))
        (AfterComma n, b) if b == '"' -> Break (ObjectFieldNameStart n)
        (AfterClosingBrace n, b) if isWhitespace b -> Continue (AfterClosingBrace (n + 1))
        (AfterClosingBrace n, _) -> Break (AfterClosingBrace n)
        _ -> Break InvalidObject

ObjectState : [
    BeforeOpeningBrace U64,
    AfterOpeningBrace U64,
    ObjectFieldNameStart U64,
    BeforeColon U64,
    AfterColon U64,
    AfterObjectValue U64,
    AfterComma U64,
    AfterClosingBrace U64,
    InvalidObject,
]

# Test decode of record with two strings ignoring whitespace
expect
    input = Str.toUtf8 " {\n\"FruitCount\"\t:2\n, \"OwnerName\": \"Farmer Joe\" } "
    decoder = utf8With { fieldNameMapping: PascalCase }
    actual = decodeRecord actual
    expected = Ok { fruitCount: 2, ownerName: "Farmer Joe" }

    actual.result == expected

# Test decode of record with an array of strings and a boolean field
expect
    input = Str.toUtf8 "{\"fruit-flavours\": [\"Apples\",\"Bananas\",\"Pears\"], \"is-fresh\": true }"
    decoder = utf8With { fieldNameMapping: KebabCase }
    actual = decodeRecord actual
    expected = Ok { fruitFlavours: ["Apples", "Bananas", "Pears"], isFresh: Bool.true }

    actual.result == expected

# Test decode of record with a string and number field
expect
    input = Str.toUtf8 "{\"first_segment\":\"ab\",\"second_segment\":10}"
    decoder = utf8With { fieldNameMapping: SnakeCase }
    actual = decodeRecord actual
    expected = Ok { firstSegment: "ab", secondSegment: 10u8 }

    actual.result == expected

# Test decode of record of a record
expect
    input = Str.toUtf8 "{\"OUTER\":{\"INNER\":\"a\"},\"OTHER\":{\"ONE\":\"b\",\"TWO\":10}}"
    decoder = utf8With { fieldNameMapping: Custom fromYellingCase }
    actual = decodeRecord actual
    expected = Ok { outer: { inner: "a" }, other: { one: "b", two: 10u8 } }

    actual.result == expected

# Complex example from IETF RFC 8259 (2017)
complexExampleJson = Str.toUtf8 "{\"Image\":{\"Animated\":false,\"Height\":600,\"Ids\":[116,943,234,38793],\"Thumbnail\":{\"Height\":125,\"Url\":\"http:\\/\\/www.example.com\\/image\\/481989943\",\"Width\":100},\"Title\":\"View from 15th Floor\",\"Width\":800}}"
complexExampleRecord = {
    image: {
        width: 800,
        height: 600,
        title: "View from 15th Floor",
        thumbnail: {
            url: "http://www.example.com/image/481989943",
            height: 125,
            width: 100,
        },
        animated: Bool.false,
        ids: [116, 943, 234, 38793],
    },
}

# Test decode of Complex Example
expect
    input = complexExampleJson
    decoder = utf8With { fieldNameMapping: PascalCase }
    actual = Decode.fromBytes input decoder
    expected = Ok complexExampleRecord

    actual == expected

# Test encode of Complex Example
expect
    input = complexExampleRecord
    encoder = utf8With { fieldNameMapping: PascalCase }
    actual = Encode.toBytes input encoder
    expected = complexExampleJson

    actual == expected
