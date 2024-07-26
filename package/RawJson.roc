module [
    isNull,
    asStr,
    asInt,
    asFloat,
    asBool,
    asList,
    asObject,
    decode,
    RawJson,
]

import DecodeUtils exposing [parseExactChar, eatWhitespace, tryDecode]
import DecodeUtils.String exposing [decodeString]
import DecodeUtils.Number exposing [takeJsonNumber]

RawJson : [
    Null,
    Str Str,
    Int I64,
    Float F64,
    Bool Bool,
    List (List RawJson),
    Object (Dict Str RawJson),
]

isNull : RawJson -> Bool
isNull = \json ->
    # need to match since RawJson doesn't implement EQ, since F64 doesn't
    when json is
        Null -> Bool.true
        _ -> Bool.false

asStr : RawJson -> Result Str [NotAStr]
asStr = \json ->
    when json is
        Str s -> Ok s
        _ -> Err NotAStr

asInt : RawJson -> Result I64 [NotAnInt]
asInt = \json ->
    when json is
        Int i -> Ok i
        _ -> Err NotAnInt

asFloat : RawJson -> Result F64 [NotAFloat]
asFloat = \json ->
    when json is
        Float f -> Ok f
        _ -> Err NotAFloat

asBool : RawJson -> Result Bool [NotABool]
asBool = \json ->
    when json is
        Bool b -> Ok b
        _ -> Err NotABool

asList : RawJson -> Result (List RawJson) [NotAList]
asList = \json ->
    when json is
        List l -> Ok l
        _ -> Err NotAList

asObject : RawJson -> Result (Dict Str RawJson) [NotAnObject]
asObject = \json ->
    when json is
        Object o -> Ok o
        _ -> Err NotAnObject

decode : List U8 -> DecodeResult RawJson
decode = \bytes ->
    when bytes is
        [] ->
            { result: Err TooShort, rest: bytes }

        ['n', 'u', 'l', 'l', .. as rest] ->
            { result: Ok Null, rest }

        ['t', 'r', 'u', 'e', .. as rest] ->
            { result: Ok (Bool Bool.true), rest }

        ['f', 'a', 'l', 's', 'e', .. as rest] ->
            { result: Ok (Bool Bool.false), rest }

        ['"', ..] ->
            { result: strResult, rest } = decodeString bytes
            { result: strResult |> Result.map Str, rest }

        ['[', .. as rest] ->
            decodeRestOfList rest

        ['{', .. as rest] ->
            decodeRestOfObject rest

        _ ->
            { taken: numberBytes, rest: afterNumber } = takeJsonNumber bytes
            numberStr =
                numberBytes
                |> Str.fromUtf8
                |> Result.withDefault ""

            when Str.toI64 numberStr is
                Ok int -> { result: Ok (Int int), rest: afterNumber }
                Err _err ->
                    when Str.toF64 numberStr is
                        Ok float -> { result: Ok (Float float), rest: afterNumber }
                        Err _err -> { result: Err TooShort, rest: bytes }

decodeRestOfList : List U8 -> DecodeResult RawJson
decodeRestOfList = \bytes ->
    walkList : List U8 -> DecodeResult (List RawJson)
    walkList = \listBytes ->
        when listBytes is
            [] -> { result: Err TooShort, rest: [] }
            [first, .. as rest] if first == ']' -> { result: Ok [], rest }
            _ ->
                afterWhitespace = eatWhitespace listBytes
                { val: listItem, rest: afterListItem } <- decode afterWhitespace
                    |> tryDecode
                afterItemAndWhitespace = eatWhitespace afterListItem

                afterComma =
                    when parseExactChar afterItemAndWhitespace ',' is
                        { result: Ok {}, rest: afterCommaBytes } -> afterCommaBytes
                        { result: Err _err, rest: _ } -> afterItemAndWhitespace
                afterCommaAndWhitespace = eatWhitespace afterComma

                when walkList afterCommaAndWhitespace is
                    { result: Ok otherItems, rest: afterOtherItems } ->
                        allItems = List.concat [listItem] otherItems
                        { result: Ok allItems, rest: afterOtherItems }

                    { result: Err innerErr, rest: _ } ->
                        { result: Err innerErr, rest: afterComma }

    { result: listData, rest: afterListBytes } = walkList bytes
    { result: listData |> Result.map List, rest: afterListBytes }

decodeRestOfObject : List U8 -> DecodeResult RawJson
decodeRestOfObject = \bytes ->
    walkObject : List U8 -> DecodeResult (Dict Str RawJson)
    walkObject = \objectBytes ->
        when objectBytes is
            [] -> { result: Err TooShort, rest: [] }
            [first, .. as rest] if first == '}' -> { result: Ok (Dict.empty {}), rest }
            _ ->
                afterWhitespace = eatWhitespace objectBytes
                { val: fieldName, rest: afterFieldName } <- decodeString afterWhitespace
                    |> tryDecode
                afterFieldAndWhitespace = eatWhitespace afterFieldName

                { val: {}, rest: afterColon } <- afterFieldAndWhitespace
                    |> parseExactChar ':'
                    |> tryDecode
                afterColonAndWhitespace = eatWhitespace afterColon

                { val: fieldData, rest: afterFieldData } <- decode afterColonAndWhitespace
                    |> tryDecode
                afterFieldDataAndWhitespace = eatWhitespace afterFieldData

                afterComma =
                    when parseExactChar afterFieldAndWhitespace ',' is
                        { result: Ok {}, rest: afterCommaBytes } -> afterCommaBytes
                        { result: Err _err, rest: _ } -> afterFieldDataAndWhitespace
                afterCommaAndWhitespace = eatWhitespace afterComma

                when walkObject afterCommaAndWhitespace is
                    { result: Ok otherFields, rest: afterOtherFields } ->
                        allFields =
                            otherFields
                            |> Dict.insert fieldName fieldData
                        { result: Ok allFields, rest: afterOtherFields }

                    { result: Err innerErr, rest: _ } ->
                        { result: Err innerErr, rest: afterFieldDataAndWhitespace }

    { result: objectData, rest: afterObjectBytes } = walkObject bytes
    { result: objectData |> Result.map Object, rest: afterObjectBytes }

