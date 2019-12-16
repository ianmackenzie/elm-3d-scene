module Glsl exposing
    ( Attribute
    , Function
    , Type(..)
    , Uniform
    , Varying
    , attribute
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
    = Function
        { source : String
        , dependencies : List Function
        }


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
    = Attribute String Type


type Uniform
    = Uniform String Type


type Varying
    = Varying String Type


attribute : String -> Type -> Attribute
attribute =
    Attribute


uniform : String -> Type -> Uniform
uniform =
    Uniform


varying : String -> Type -> Varying
varying =
    Varying


function : String -> List Function -> Function
function source dependencies =
    Function
        { source = stripExtraWhitespace source
        , dependencies = dependencies
        }


functionSource : Function -> String
functionSource (Function { source }) =
    source


type alias Types =
    { attributes : List Attribute
    , uniforms : List Uniform
    , varyings : List Varying
    }


program : Types -> String -> List Function -> String
program types source dependencies =
    String.join "\n\n" <|
        List.filter (not << String.isEmpty) <|
            "precision mediump float;"
                :: attributesBlock types.attributes
                :: uniformsBlock types.uniforms
                :: varyingsBlock types.varyings
                :: stripExtraWhitespace source
                :: List.map functionSource dependencies


attributesBlock : List Attribute -> String
attributesBlock attributes =
    String.join "\n" (List.map attributeLine attributes)


attributeLine : Attribute -> String
attributeLine (Attribute name (Type typeName)) =
    "attribute " ++ typeName ++ " " ++ name ++ ";"


uniformsBlock : List Uniform -> String
uniformsBlock uniforms =
    String.join "\n" (List.map uniformLine uniforms)


uniformLine : Uniform -> String
uniformLine (Uniform name (Type typeName)) =
    "uniform " ++ typeName ++ " " ++ name ++ ";"


varyingsBlock : List Varying -> String
varyingsBlock varyings =
    String.join "\n" (List.map varyingLine varyings)


varyingLine : Varying -> String
varyingLine (Varying name (Type typeName)) =
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
