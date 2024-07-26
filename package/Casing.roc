module [
    toYellingCase,
    fromYellingCase,
    toObjectNameUsingMap,
    fromObjectNameUsingMap,
    FieldNameMapping,

]

## Mapping between Roc record fields and JSON object names
FieldNameMapping : [
    Default, # no transformation
    SnakeCase, # snake_case
    PascalCase, # PascalCase
    KebabCase, # kabab-case
    CamelCase, # camelCase
    Custom (Str -> Str), # provide a custom formatting
]

crashOnBadUtf8Error : Result Str _ -> Str
crashOnBadUtf8Error = \res ->
    when res is
        Ok str -> str
        Err _ -> crash "invalid UTF-8 code units"

toYellingCase = \str ->
    Str.toUtf8 str
    |> List.map toUppercase
    |> Str.fromUtf8
    |> crashOnBadUtf8Error

fromYellingCase = \str ->
    Str.toUtf8 str
    |> List.map toLowercase
    |> Str.fromUtf8
    |> crashOnBadUtf8Error

expect fromYellingCase "YELLING" == "yelling"

fromObjectNameUsingMap : Str, FieldNameMapping -> Str
fromObjectNameUsingMap = \objectName, fieldNameMapping ->
    when fieldNameMapping is
        Default -> objectName
        SnakeCase -> fromSnakeCase objectName
        PascalCase -> fromPascalCase objectName
        KebabCase -> fromKebabCase objectName
        CamelCase -> fromCamelCase objectName
        Custom transformation -> transformation objectName

toObjectNameUsingMap : Str, FieldNameMapping -> Str
toObjectNameUsingMap = \fieldName, fieldNameMapping ->
    when fieldNameMapping is
        Default -> fieldName
        SnakeCase -> toSnakeCase fieldName
        PascalCase -> toPascalCase fieldName
        KebabCase -> toKebabCase fieldName
        CamelCase -> toCamelCase fieldName
        Custom transformation -> transformation fieldName

# Convert a `snake_case` JSON Object name to a Roc Field name
fromSnakeCase = \str ->
    snakeToCamel str

# Convert a `PascalCase` JSON Object name to a Roc Field name
fromPascalCase = \str ->
    pascalToCamel str

# Convert a `kabab-case` JSON Object name to a Roc Field name
fromKebabCase = \str ->
    kebabToCamel str

# Convert a `camelCase` JSON Object name to a Roc Field name
fromCamelCase = \str ->
    # Nothing to change as Roc field names are camelCase by default
    str

# Convert a `camelCase` Roc Field name to a `snake_case` JSON Object name
toSnakeCase = \str ->
    camelToSnake str

# Convert a `camelCase` Roc Field name to a `PascalCase` JSON Object name
toPascalCase = \str ->
    camelToPascal str

# Convert a `camelCase` Roc Field name to a `kabab-case` JSON Object name
toKebabCase = \str ->
    camelToKebeb str

# Convert a `camelCase` Roc Field name to a `camelCase` JSON Object name
toCamelCase = \str ->
    # Nothing to change as Roc field names are camelCase by default
    str

snakeToCamel : Str -> Str
snakeToCamel = \str ->
    segments = Str.split str "_"
    when segments is
        [first, .. as rest] ->
            rest
            |> List.map uppercaseFirst
            |> List.prepend first
            |> Str.joinWith ""

        _ -> str

expect snakeToCamel "snake_case_string" == "snakeCaseString"

pascalToCamel : Str -> Str
pascalToCamel = \str ->
    segments = Str.toUtf8 str
    when segments is
        [a, .. as rest] ->
            first = toLowercase a
            rest |> List.prepend first |> Str.fromUtf8 |> crashOnBadUtf8Error

        _ -> str

expect pascalToCamel "PascalCaseString" == "pascalCaseString"

kebabToCamel : Str -> Str
kebabToCamel = \str ->
    segments = Str.split str "-"
    when segments is
        [first, .. as rest] ->
            rest
            |> List.map uppercaseFirst
            |> List.prepend first
            |> Str.joinWith ""

        _ -> str

expect kebabToCamel "kebab-case-string" == "kebabCaseString"

camelToPascal : Str -> Str
camelToPascal = \str ->
    segments = Str.toUtf8 str
    when segments is
        [a, .. as rest] ->
            first = toUppercase a
            rest |> List.prepend first |> Str.fromUtf8 |> crashOnBadUtf8Error

        _ -> str

expect camelToPascal "someCaseString" == "SomeCaseString"

camelToKebeb : Str -> Str
camelToKebeb = \str ->
    rest = Str.toUtf8 str
    taken = List.withCapacity (List.len rest)

    camelToKebabHelp { taken, rest }
    |> .taken
    |> Str.fromUtf8
    |> crashOnBadUtf8Error

camelToKebabHelp : { taken : List U8, rest : List U8 } -> { taken : List U8, rest : List U8 }
camelToKebabHelp = \{ taken, rest } ->
    when rest is
        [] -> { taken, rest }
        [a, ..] if isUpperCase a ->
            camelToKebabHelp {
                taken: List.concat taken ['-', toLowercase a],
                rest: List.dropFirst rest 1,
            }

        [a, ..] ->
            camelToKebabHelp {
                taken: List.append taken a,
                rest: List.dropFirst rest 1,
            }

expect camelToKebeb "someCaseString" == "some-case-string"

camelToSnake : Str -> Str
camelToSnake = \str ->
    rest = Str.toUtf8 str
    taken = List.withCapacity (List.len rest)

    camelToSnakeHelp { taken, rest }
    |> .taken
    |> Str.fromUtf8
    |> crashOnBadUtf8Error

camelToSnakeHelp : { taken : List U8, rest : List U8 } -> { taken : List U8, rest : List U8 }
camelToSnakeHelp = \{ taken, rest } ->
    when rest is
        [] -> { taken, rest }
        [a, ..] if isUpperCase a ->
            camelToSnakeHelp {
                taken: List.concat taken ['_', toLowercase a],
                rest: List.dropFirst rest 1,
            }

        [a, ..] ->
            camelToSnakeHelp {
                taken: List.append taken a,
                rest: List.dropFirst rest 1,
            }

expect camelToSnake "someCaseString" == "some_case_string"

uppercaseFirst : Str -> Str
uppercaseFirst = \str ->
    segments = Str.toUtf8 str
    when segments is
        [a, .. as rest] ->
            first = toUppercase a
            rest |> List.prepend first |> Str.fromUtf8 |> crashOnBadUtf8Error

        _ -> str

toUppercase : U8 -> U8
toUppercase = \codeunit ->
    if 'a' <= codeunit && codeunit <= 'z' then
        codeunit - (32) # 32 is the difference to the respecive uppercase letters
    else
        codeunit

toLowercase : U8 -> U8
toLowercase = \codeunit ->
    if 'A' <= codeunit && codeunit <= 'Z' then
        codeunit + 32 # 32 is the difference to the respecive lowercase letters
    else
        codeunit

isUpperCase : U8 -> Bool
isUpperCase = \codeunit ->
    'A' <= codeunit && codeunit <= 'Z'

