module Glsl exposing
    ( Attribute
    , Constant
    , Function
    , Type(..)
    , Uniform
    , Varying
    , attribute
    , constant
    , float
    , function
    , mat3
    , mat4
    , program
    , uniform
    , varying
    , vec2
    , vec3
    , vec4
    )


type Function
    = Function String (List Function) (List Constant)


type Type
    = Type String


float : Type
float =
    Type "float"


vec2 : Type
vec2 =
    Type "vec2"


vec3 : Type
vec3 =
    Type "vec3"


vec4 : Type
vec4 =
    Type "vec4"


mat3 : Type
mat3 =
    Type "mat3"


mat4 : Type
mat4 =
    Type "mat4"


type Attribute
    = Attribute Type String


type Uniform
    = Uniform Type String


type Varying
    = Varying Type String


type Constant
    = Constant Type String String


attribute : Type -> String -> Attribute
attribute =
    Attribute


uniform : Type -> String -> Uniform
uniform =
    Uniform


varying : Type -> String -> Varying
varying =
    Varying


constant : Type -> String -> String -> Constant
constant =
    Constant


function : { dependencies : List Function, constants : List Constant } -> String -> Function
function { dependencies, constants } source =
    Function (stripExtraWhitespace source) dependencies constants


functionSource : Function -> String
functionSource (Function source dependencies constants) =
    source


addFunctionSource : String -> List String -> List String
addFunctionSource source accumulated =
    if List.member source accumulated then
        accumulated

    else
        accumulated ++ [ source ]


collectFunctionSources : List Function -> List String -> List String
collectFunctionSources functions accumulated =
    case functions of
        (Function source dependencies constants) :: rest ->
            accumulated
                |> collectFunctionSources dependencies
                |> addFunctionSource source
                |> collectFunctionSources rest

        [] ->
            accumulated


addConstant : Constant -> List String -> List String
addConstant (Constant (Type typeName) name value) accumulated =
    let
        constantLine =
            "const " ++ typeName ++ " " ++ name ++ " = " ++ value ++ ";"
    in
    if List.member constantLine accumulated then
        accumulated

    else
        accumulated ++ [ constantLine ]


collectFunctionConstants : List Function -> List String -> List String
collectFunctionConstants functions accumulated =
    case functions of
        (Function source dependencies constants) :: rest ->
            List.foldl addConstant (collectFunctionConstants dependencies accumulated) constants
                |> collectFunctionConstants rest

        [] ->
            accumulated


program :
    { attributes : List Attribute
    , uniforms : List Uniform
    , varyings : List Varying
    , constants : List Constant
    , functions : List Function
    }
    -> String
    -> String
program { attributes, uniforms, varyings, constants, functions } source =
    let
        allFunctions =
            String.join "\n\n" (collectFunctionSources functions [])

        allConstants =
            String.join "\n" <|
                List.foldl addConstant (collectFunctionConstants functions []) constants
    in
    String.join "\n\n" <|
        List.filter (not << String.isEmpty) <|
            [ "precision mediump float;"
            , attributesBlock attributes
            , uniformsBlock uniforms
            , varyingsBlock varyings
            , allConstants
            , allFunctions
            , stripExtraWhitespace source
            ]


attributesBlock : List Attribute -> String
attributesBlock attributes =
    String.join "\n" (List.map attributeLine attributes)


uniformsBlock : List Uniform -> String
uniformsBlock uniforms =
    String.join "\n" (List.map uniformLine uniforms)


varyingsBlock : List Varying -> String
varyingsBlock varyings =
    String.join "\n" (List.map varyingLine varyings)


attributeLine : Attribute -> String
attributeLine (Attribute (Type typeName) name) =
    "attribute " ++ typeName ++ " " ++ name ++ ";"


uniformLine : Uniform -> String
uniformLine (Uniform (Type typeName) name) =
    "uniform " ++ typeName ++ " " ++ name ++ ";"


varyingLine : Varying -> String
varyingLine (Varying (Type typeName) name) =
    "varying " ++ typeName ++ " " ++ name ++ ";"


indentation : String -> Maybe Int
indentation string =
    if String.isEmpty (String.trim string) then
        Nothing

    else
        Just (String.length string - String.length (String.trimLeft string))


stripExtraWhitespace : String -> String
stripExtraWhitespace code =
    let
        lines =
            String.lines code

        minIndentation =
            List.minimum (List.filterMap indentation lines)
                |> Maybe.withDefault 0
    in
    String.trim (String.join "\n" (List.map (String.dropLeft minIndentation) lines))
