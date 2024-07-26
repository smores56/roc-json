## JSON is a data format that is easy for humans to read and write. It is
## commonly used to exhange data between two systems such as a server and a
## client (e.g. web browser).
##
## This module implements functionality to serialise and de-serialise Roc types
## to and from JSON data. Using the `Encode` and `Decode` builtins this process
## can be achieved without the need to write custom encoder and decoder functions
## to parse UTF-8 strings.
##
## Here is a basic example which shows how to parse a JSON record into a Roc
## type named `Language` which includes a `name` field. The JSON string is
## decoded and then the field is encoded back into a UTF-8 string.
##
## ```
## Language : {
##     name : Str,
## }
##
## jsonStr = Str.toUtf8 "{\"name\":\"Röc Lang\"}"
##
## result : Result Language _
## result =
##     jsonStr
##     |> Decode.fromBytes Json.utf8 # returns `Ok {name : "Röc Lang"}`
##
## name =
##     decodedValue <- Result.map result
##
##     Encode.toBytes decodedValue.name Json.utf8
##
## expect name == Ok (Str.toUtf8 "\"Röc Lang\"")
## ```
module [
    Json,
    utf8,
    utf8With,
    encodeAsNullOption,
]

import Casing exposing [FieldNameMapping]
import DecodeUtils.Array exposing [decodeList]
import DecodeUtils.String exposing [decodeString]
import DecodeUtils.Record exposing [decodeRecord]
import DecodeUtils.Boolean exposing [decodeBool]

## An opaque type with the `Encode.EncoderFormatting` and
## `DecoderFormatting` abilities.
Json := { fieldNameMapping : FieldNameMapping, skipMissingProperties : Bool, nullDecodeAsEmpty : Bool, emptyEncodeAsNull : EncodeAsNull }
    implements [
        Encode.EncoderFormatting {
            u8: encodeU8,
            u16: encodeU16,
            u32: encodeU32,
            u64: encodeU64,
            u128: encodeU128,
            i8: encodeI8,
            i16: encodeI16,
            i32: encodeI32,
            i64: encodeI64,
            i128: encodeI128,
            f32: encodeF32,
            f64: encodeF64,
            dec: encodeDec,
            bool: encodeBool,
            string: encodeString,
            # list: encodeList,
            # record: encodeRecord,
            # tuple: encodeTuple,
            tag: encodeTag,
        },
        DecoderFormatting {
            u8: decodeU8,
            u16: decodeU16,
            u32: decodeU32,
            u64: decodeU64,
            u128: decodeU128,
            i8: decodeI8,
            i16: decodeI16,
            i32: decodeI32,
            i64: decodeI64,
            i128: decodeI128,
            f32: decodeF32,
            f64: decodeF64,
            dec: decodeDec,
            # bool: decodeBool,
            string: decodeString,
            # list: decodeList,
            # record: decodeRecord,
            tuple: decodeTuple,
        },
    ]

## Returns a JSON `Encode.Encoder` and `Decoder`
utf8 = @Json { fieldNameMapping: Default, skipMissingProperties: Bool.true, nullDecodeAsEmpty: Bool.true, emptyEncodeAsNull: defaultEncodeAsNull }

## Returns a JSON `Encode.Encoder` and `Decoder` with configuration options
##
## **skipMissingProperties** - if `True` the decoder will skip additional properties
## in the json that are not present in the model. (Default: `True`)
##
## **nullDecodeAsEmpty** - if `True` the decoder will convert `null` to an empty byte array.
## This makes `{"email":null,"name":"bob"}` decode the same as `{"name":"bob"}`. (Default: `True`)
##
## **emptyEncodeAsNull** - if `True` encoders that return `[]` will result in a `null` in the
## json. If `False` when an encoder returns `[]` the record field, or list/tuple element, will be ommitted.
## eg: `{email:@Option None, name:"bob"}` encodes to `{"email":null, "name":"bob"}` instead of `{"name":"bob"}` (Default: `True`)

utf8With : { fieldNameMapping ? FieldNameMapping, skipMissingProperties ? Bool, nullDecodeAsEmpty ? Bool, emptyEncodeAsNull ? EncodeAsNull } -> Json
utf8With = \{ fieldNameMapping ? Default, skipMissingProperties ? Bool.true, nullDecodeAsEmpty ? Bool.true, emptyEncodeAsNull ? defaultEncodeAsNull } ->
    @Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull }

# TODO: move to DecodeUtils.Null maybe
EncodeAsNull : {
    list : Bool,
    tuple : Bool,
    record : Bool,
}

encodeAsNullOption : { list ? Bool, tuple ? Bool, record ? Bool } -> EncodeAsNull
encodeAsNullOption = \{ list ? Bool.false, tuple ? Bool.true, record ? Bool.true } -> {
    list,
    tuple,
    record,
}
defaultEncodeAsNull = {
    list: Bool.false,
    tuple: Bool.true,
    record: Bool.true,
}

# TODO encode as JSON numbers as base 10 decimal digits
# e.g. the REPL `Num.toStr 12e42f64` gives
# "12000000000000000000000000000000000000000000" : Str
# which should be encoded as "12e42" : Str
numToBytes = \n ->
    n |> Num.toStr |> Str.toUtf8

encodeU8 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeU16 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeU32 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeU64 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeU128 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI8 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI16 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI32 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI64 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeI128 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeF32 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeF64 = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeDec = \n ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (numToBytes n)

encodeBool = \b ->
    Encode.custom \bytes, @Json {} ->
        if b then
            List.concat bytes (Str.toUtf8 "true")
        else
            List.concat bytes (Str.toUtf8 "false")

# Test encode boolean
expect
    input = [Bool.true, Bool.false]
    actual = Encode.toBytes input utf8
    expected = Str.toUtf8 "[true,false]"

    actual == expected

encodeString = \str ->
    Encode.custom \bytes, @Json {} ->
        List.concat bytes (encodeStrBytes str)

# TODO add support for unicode escapes (including 2,3,4 byte code points)
# these should be encoded using a 12-byte sequence encoding the UTF-16 surrogate
# pair. For example a string containing only G clef character U+1D11E is
# represented as "\\uD834\\uDD1E" (note "\\" here is a single reverse solidus)
encodeStrBytes = \str ->
    bytes = Str.toUtf8 str

    initialState = { bytePos: 0, status: NoEscapesFound }

    firstPassState =
        List.walkUntil bytes initialState \{ bytePos, status }, b ->
            when b is
                0x22 -> Break { bytePos, status: FoundEscape } # U+0022 Quotation mark
                0x5c -> Break { bytePos, status: FoundEscape } # U+005c Reverse solidus
                0x2f -> Break { bytePos, status: FoundEscape } # U+002f Solidus
                0x08 -> Break { bytePos, status: FoundEscape } # U+0008 Backspace
                0x0c -> Break { bytePos, status: FoundEscape } # U+000c Form feed
                0x0a -> Break { bytePos, status: FoundEscape } # U+000a Line feed
                0x0d -> Break { bytePos, status: FoundEscape } # U+000d Carriage return
                0x09 -> Break { bytePos, status: FoundEscape } # U+0009 Tab
                _ -> Continue { bytePos: bytePos + 1, status }

    when firstPassState.status is
        NoEscapesFound ->
            (List.len bytes)
            + 2
            |> List.withCapacity
            |> List.concat ['"']
            |> List.concat bytes
            |> List.concat ['"']

        FoundEscape ->
            { before: bytesBeforeEscape, others: bytesWithEscapes } =
                List.split bytes firstPassState.bytePos

            # Reserve List with 120% capacity for escaped bytes to reduce
            # allocations, include starting quote, and bytes up to first escape
            initial =
                List.len bytes
                |> Num.mul 120
                |> Num.divCeil 100
                |> List.withCapacity
                |> List.concat ['"']
                |> List.concat bytesBeforeEscape

            # Walk the remaining bytes and include escape '\' as required
            # add closing quote
            List.walk bytesWithEscapes initial \encodedBytes, byte ->
                List.concat encodedBytes (escapedByteToJson byte)
            |> List.concat ['"']

# Prepend an "\" escape byte
escapedByteToJson : U8 -> List U8
escapedByteToJson = \b ->
    when b is
        0x22 -> [0x5c, 0x22] # U+0022 Quotation mark
        0x5c -> [0x5c, 0x5c] # U+005c Reverse solidus
        0x2f -> [0x5c, 0x2f] # U+002f Solidus
        0x08 -> [0x5c, 'b'] # U+0008 Backspace
        0x0c -> [0x5c, 'f'] # U+000c Form feed
        0x0a -> [0x5c, 'n'] # U+000a Line feed
        0x0d -> [0x5c, 'r'] # U+000d Carriage return
        0x09 -> [0x5c, 'r'] # U+0009 Tab
        _ -> [b]

expect escapedByteToJson '\n' == ['\\', 'n']
expect escapedByteToJson '\\' == ['\\', '\\']
expect escapedByteToJson '"' == ['\\', '"']

# Test encode small string
expect
    input = "G'day"
    actual = Encode.toBytes input utf8
    expected = Str.toUtf8 "\"G'day\""

    actual == expected

# Test encode large string
expect
    input = "the quick brown fox jumps over the lazy dog"
    actual = Encode.toBytes input utf8
    expected = Str.toUtf8 "\"the quick brown fox jumps over the lazy dog\""

    actual == expected

# Test encode with escapes e.g. "\r" encodes to "\\r"
expect
    input = "the quick brown fox jumps over the lazy doga\r\nbc\\\"xz"
    actual = Encode.toBytes input utf8
    expected = Str.toUtf8 "\"the quick brown fox jumps over the lazy doga\\r\\nbc\\\\\\\"xz\""

    actual == expected

encodeList = \lst, encodeElem ->
    Encode.custom \bytes, @Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull } ->
        writeList = \{ buffer, elemsLeft }, elem ->
            beforeBufferLen = buffer |> List.len

            bufferWithElem =
                elemBytes =
                    Encode.appendWith [] (encodeElem elem) (@Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull })
                    |> emptyToNull emptyEncodeAsNull.list
                buffer |> List.concat elemBytes

            # If our encoder returned [] we just skip the elem
            emptyEncode = bufferWithElem |> List.len == beforeBufferLen
            if emptyEncode then
                { buffer: bufferWithElem, elemsLeft: elemsLeft - 1 }
            else
                bufferWithSuffix =
                    if elemsLeft > 1 then
                        List.append bufferWithElem (Num.toU8 ',')
                    else
                        bufferWithElem

                { buffer: bufferWithSuffix, elemsLeft: elemsLeft - 1 }

        head = List.append bytes (Num.toU8 '[')
        { buffer: withList } = List.walk lst { buffer: head, elemsLeft: List.len lst } writeList

        List.append withList (Num.toU8 ']')

# Test encode list of floats
expect
    input : List F64
    input = [-1, 0.00001, 1e12, 2.0e-2, 0.0003, 43]
    actual = Encode.toBytes input utf8
    expected = Str.toUtf8 "[-1,0.00001,1000000000000,0.02,0.0003,43]"

    actual == expected

encodeRecord = \fields ->
    Encode.custom \bytes, @Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull } ->
        writeRecord = \{ buffer, fieldsLeft }, { key, value } ->

            fieldValue =
                []
                |> Encode.appendWith value (@Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull })
                |> emptyToNull emptyEncodeAsNull.record

            # If our encoder returned [] we just skip the field

            emptyEncode = fieldValue == []
            if emptyEncode then
                { buffer, fieldsLeft: fieldsLeft - 1 }
            else
                fieldName = toObjectNameUsingMap key fieldNameMapping
                bufferWithKeyValue =
                    List.append buffer (Num.toU8 '"')
                    |> List.concat (Str.toUtf8 fieldName)
                    |> List.append (Num.toU8 '"')
                    |> List.append (Num.toU8 ':') # Note we need to encode using the json config here
                    |> List.concat fieldValue

                bufferWithSuffix =
                    if fieldsLeft > 1 then
                        List.append bufferWithKeyValue (Num.toU8 ',')
                    else
                        bufferWithKeyValue

                { buffer: bufferWithSuffix, fieldsLeft: fieldsLeft - 1 }

        bytesHead = List.append bytes (Num.toU8 '{')
        { buffer: bytesWithRecord } = List.walk fields { buffer: bytesHead, fieldsLeft: List.len fields } writeRecord

        List.append bytesWithRecord (Num.toU8 '}')

# Test encode for a record with two strings ignoring whitespace
expect
    input = { fruitCount: 2, ownerName: "Farmer Joe" }
    encoder = utf8With { fieldNameMapping: PascalCase }
    actual = Encode.toBytes input encoder
    expected = Str.toUtf8 "{\"FruitCount\":2,\"OwnerName\":\"Farmer Joe\"}"

    actual == expected

# Test encode of record with an array of strings and a boolean field
expect
    input = { fruitFlavours: ["Apples", "Bananas", "Pears"], isFresh: Bool.true }
    encoder = utf8With { fieldNameMapping: KebabCase }
    actual = Encode.toBytes input encoder
    expected = Str.toUtf8 "{\"fruit-flavours\":[\"Apples\",\"Bananas\",\"Pears\"],\"is-fresh\":true}"

    actual == expected

# Test encode of record with a string and number field
expect
    input = { firstSegment: "ab", secondSegment: 10u8 }
    encoder = utf8With { fieldNameMapping: SnakeCase }
    actual = Encode.toBytes input encoder
    expected = Str.toUtf8 "{\"first_segment\":\"ab\",\"second_segment\":10}"

    actual == expected

# Test encode of record of a record
expect
    input = { outer: { inner: "a" }, other: { one: "b", two: 10u8 } }
    encoder = utf8With { fieldNameMapping: Custom toYellingCase }
    actual = Encode.toBytes input encoder
    expected = Str.toUtf8 "{\"OTHER\":{\"ONE\":\"b\",\"TWO\":10},\"OUTER\":{\"INNER\":\"a\"}}"

    actual == expected

encodeTuple = \elems ->
    Encode.custom \bytes, @Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull } ->
        writeTuple = \{ buffer, elemsLeft }, elemEncoder ->
            beforeBufferLen = buffer |> List.len

            bufferWithElem =
                elemBytes =
                    Encode.appendWith [] (elemEncoder) (@Json { fieldNameMapping, skipMissingProperties, nullDecodeAsEmpty, emptyEncodeAsNull })
                    |> emptyToNull emptyEncodeAsNull.tuple
                buffer |> List.concat elemBytes
            # If our encoder returned [] we just skip the elem
            emptyEncode = bufferWithElem |> List.len == beforeBufferLen
            if emptyEncode then
                { buffer: bufferWithElem, elemsLeft: elemsLeft - 1 }
            else
                bufferWithSuffix =
                    if elemsLeft > 1 then
                        List.append bufferWithElem (Num.toU8 ',')
                    else
                        bufferWithElem

                { buffer: bufferWithSuffix, elemsLeft: elemsLeft - 1 }

        bytesHead = List.append bytes (Num.toU8 '[')
        { buffer: bytesWithRecord } = List.walk elems { buffer: bytesHead, elemsLeft: List.len elems } writeTuple

        List.append bytesWithRecord (Num.toU8 ']')

# Test encode of tuple
expect
    input = ("The Answer is", 42)
    actual = Encode.toBytes input utf8
    expected = Str.toUtf8 "[\"The Answer is\",42]"

    actual == expected

encodeTag = \name, payload ->
    Encode.custom \bytes, @Json jsonFmt ->
        # Idea: encode `A v1 v2` as `{"A": [v1, v2]}`
        writePayload = \{ buffer, itemsLeft }, encoder ->
            bufferWithValue = Encode.appendWith buffer encoder (@Json jsonFmt)
            bufferWithSuffix =
                if itemsLeft > 1 then
                    List.append bufferWithValue (Num.toU8 ',')
                else
                    bufferWithValue

            { buffer: bufferWithSuffix, itemsLeft: itemsLeft - 1 }

        bytesHead =
            List.append bytes (Num.toU8 '{')
            |> List.append (Num.toU8 '"')
            |> List.concat (Str.toUtf8 name)
            |> List.append (Num.toU8 '"')
            |> List.append (Num.toU8 ':')
            |> List.append (Num.toU8 '[')

        { buffer: bytesWithPayload } = List.walk payload { buffer: bytesHead, itemsLeft: List.len payload } writePayload

        List.append bytesWithPayload (Num.toU8 ']')
        |> List.append (Num.toU8 '}')

# Test encode of tag
expect
    input = TheAnswer "is" 42
    encoder = utf8With { fieldNameMapping: KebabCase }
    actual = Encode.toBytes input encoder
    expected = Str.toUtf8 "{\"TheAnswer\":[\"is\",42]}"

    actual == expected

decodeNumberFromStr : List U8, (Str -> Result num err) -> DecodeResult num
decodeNumberFromStr = \bytes, converter ->
    { taken, rest } = takeJsonNumber bytes

    result =
        taken
        |> Str.fromUtf8
        |> Result.try converter
        |> Result.mapErr \_ -> TooShort

    { result, rest }

decodeU8 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toU8

# Test decode of U8
expect
    actual = Str.toUtf8 "255" |> Decode.fromBytes utf8
    actual == Ok 255u8

decodeU16 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toU16

# Test decode of U16
expect
    actual = Str.toUtf8 "65535" |> Decode.fromBytes utf8
    actual == Ok 65_535u16

decodeU32 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toU32

# Test decode of U32
expect
    actual = Str.toUtf8 "4000000000" |> Decode.fromBytes utf8
    actual == Ok 4_000_000_000u32

decodeU64 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toU64

# Test decode of U64
expect
    actual = Str.toUtf8 "18446744073709551614" |> Decode.fromBytes utf8
    actual == Ok 18_446_744_073_709_551_614u64

decodeU128 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toU128

# Test decode of U128
expect
    actual = Str.toUtf8 "1234567" |> Decode.fromBytesPartial utf8
    actual.result == Ok 1234567u128

decodeI8 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toI8

# Test decode of I8
expect
    actual = Str.toUtf8 "-125" |> Decode.fromBytesPartial utf8
    actual.result == Ok -125i8

decodeI16 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toI16

# Test decode of I16
expect
    actual = Str.toUtf8 "-32768" |> Decode.fromBytesPartial utf8
    actual.result == Ok -32_768i16

decodeI32 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toI32

# Test decode of I32
expect
    actual = Str.toUtf8 "-2147483648" |> Decode.fromBytesPartial utf8
    actual.result == Ok -2_147_483_648i32

decodeI64 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toI64

# Test decode of I64
expect
    actual = Str.toUtf8 "-9223372036854775808" |> Decode.fromBytesPartial utf8
    actual.result == Ok -9_223_372_036_854_775_808i64

decodeI128 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toI128

decodeF32 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toF32

# Test decode of F32
expect
    actual : DecodeResult F32
    actual = Str.toUtf8 "12.34e-5" |> Decode.fromBytesPartial utf8
    numStr = actual.result |> Result.map Num.toStr

    Result.withDefault numStr "" == "0.00012339999375399202"

decodeF64 = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toF64

# Test decode of F64
expect
    actual : DecodeResult F64
    actual = Str.toUtf8 "12.34e-5" |> Decode.fromBytesPartial utf8
    numStr = actual.result |> Result.map Num.toStr

    Result.withDefault numStr "" == "0.0001234"

decodeDec = Decode.custom \bytes, @Json {} ->
    decodeNumberFromStr Str.toDec

# Test decode of Dec
expect
    actual : DecodeResult Dec
    actual = Str.toUtf8 "12.0034" |> Decode.fromBytesPartial utf8

    actual.result == Ok 12.0034dec

decodeTuple = \initialState, stepElem, finalizer -> Decode.custom \initialBytes, jsonFmt ->
        # NB: the stepper function must be passed explicitly until #2894 is resolved.
        decodeElems = \stepper, state, index, bytes ->
            { val: newState, rest: beforeCommaOrBreak } <- tryDecode
                    (
                        when stepper state index is
                            TooLong ->
                                { rest: beforeCommaOrBreak } <- bytes |> anything |> tryDecode
                                { result: Ok state, rest: beforeCommaOrBreak }

                            Next decoder ->
                                decodePotentialNull (eatWhitespace bytes) decoder jsonFmt

                    )

            { result: commaResult, rest: nextBytes } = comma beforeCommaOrBreak

            when commaResult is
                Ok {} -> decodeElems stepElem newState (index + 1) nextBytes
                Err _ -> { result: Ok newState, rest: nextBytes }

        { rest: afterBracketBytes } <- initialBytes |> openBracket |> tryDecode

        { val: endStateResult, rest: beforeClosingBracketBytes } <- decodeElems stepElem initialState 0 (eatWhitespace afterBracketBytes) |> tryDecode

        { rest: afterTupleBytes } <-
            (eatWhitespace beforeClosingBracketBytes)
            |> closingBracket
            |> tryDecode

        when finalizer endStateResult is
            Ok val -> { result: Ok val, rest: afterTupleBytes }
            Err e -> { result: Err e, rest: afterTupleBytes }

# Test decode of tuple
expect
    input = Str.toUtf8 "[\"The Answer is\",42]"
    actual = Decode.fromBytesPartial input utf8

    actual.result == Ok ("The Answer is", 42)

# Test decode with whitespace
expect
    input = Str.toUtf8 "[ 123,\t456\n]"
    actual = Decode.fromBytesPartial input utf8
    expected = Ok (123, 456)

    actual.result == expected

# TODO: remove this, or maybe move it?
# Test decode of a null
expect
    input = Str.toUtf8 "null"

    actual : DecodeResult Str
    actual = Decode.fromBytesPartial input utf8

    Result.isErr actual.result

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

# TODO: move to Testing.roc
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

## If the field value is "null" we may want to make it the same as the field simply not being there for decoding simplicity
decodePotentialNull = \bytes, decoder, @Json jsonFmt ->
    when nullToEmpty bytes jsonFmt.nullDecodeAsEmpty is
        Null { bytes: nullBytes, rest: nullRest } ->
            decode = Decode.decodeWith (nullBytes) decoder (@Json jsonFmt)
            # We have to replace the rest because if the null was converted to empty the decoder would return an empty rest
            { result: decode.result, rest: nullRest }

        NotNull ->
            Decode.decodeWith bytes decoder (@Json jsonFmt)
