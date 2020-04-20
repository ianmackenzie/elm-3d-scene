module Glsl exposing
    ( function, vertexShader, fragmentShader, generateModule
    , Attribute, Uniform, Varying, Constant, Function, Type, Shader
    , Precision, highp, mediump, lowp
    , float, vec2, vec3, vec4, mat4, sampler2D
    , attribute, uniform, varying, constant
    , ShaderType(..), shaderName, shaderType, shaderSource, setShaderSource
    )

{-|

@docs function, vertexShader, fragmentShader, generateModule

@docs Attribute, Uniform, Varying, Constant, Function, Type, Shader

@docs Precision, highp, mediump, lowp

@docs float, vec2, vec3, vec4, mat4, sampler2D

@docs attribute, uniform, varying, constant

@docs ShaderType, shaderName, shaderType, shaderSource, setShaderSource

-}


type Function
    = Function String (List Function) (List Constant)


type Precision
    = Highp
    | Mediump
    | Lowp


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


mat4 : Type
mat4 =
    Type "mat4"


sampler2D : Type
sampler2D =
    Type "sampler2D"


type Attribute
    = Attribute Precision Type String


type Uniform
    = Uniform Precision Type String


type Varying
    = Varying Precision Type String


type Constant
    = Constant Precision Type String String


highp : Precision
highp =
    Highp


mediump : Precision
mediump =
    Mediump


lowp : Precision
lowp =
    Lowp


type InternalShaderType
    = Vertex (List Attribute)
    | Fragment Precision


type ShaderType
    = VertexShader
    | FragmentShader


type Shader
    = Shader
        { name : String
        , type_ : InternalShaderType
        , uniforms : List Uniform
        , varyings : List Varying
        , source : String
        }


attribute : Precision -> Type -> String -> Attribute
attribute =
    Attribute


uniform : Precision -> Type -> String -> Uniform
uniform =
    Uniform


varying : Precision -> Type -> String -> Varying
varying =
    Varying


constant : Precision -> Type -> String -> String -> Constant
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
addConstant (Constant precision (Type typeName) name value) accumulated =
    let
        constantLine =
            "const " ++ precisionString precision ++ " " ++ typeName ++ " " ++ name ++ " = " ++ value ++ ";"
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


vertexShader :
    String
    ->
        { attributes : List Attribute
        , uniforms : List Uniform
        , varyings : List Varying
        , constants : List Constant
        , functions : List Function
        }
    -> String
    -> Shader
vertexShader name { attributes, uniforms, varyings, constants, functions } source =
    shader name
        (Vertex attributes)
        { uniforms = uniforms
        , varyings = varyings
        , constants = constants
        , functions = functions
        }
        source


fragmentShader :
    String
    ->
        { precision : Precision
        , uniforms : List Uniform
        , varyings : List Varying
        , constants : List Constant
        , functions : List Function
        }
    -> String
    -> Shader
fragmentShader name properties source =
    shader name (Fragment properties.precision) properties source


shader :
    String
    -> InternalShaderType
    ->
        { a
            | uniforms : List Uniform
            , varyings : List Varying
            , constants : List Constant
            , functions : List Function
        }
    -> String
    -> Shader
shader name type_ { uniforms, varyings, constants, functions } source =
    let
        allFunctions =
            String.join "\n\n" (collectFunctionSources functions [])

        allConstants =
            String.join "\n" <|
                List.foldl addConstant (collectFunctionConstants functions []) constants

        glslSource =
            String.join "\n\n" <|
                List.filter (not << String.isEmpty) <|
                    [ shaderPrecisionString type_
                    , attributesBlock type_
                    , uniformsBlock uniforms
                    , varyingsBlock varyings
                    , allConstants
                    , allFunctions
                    , stripExtraWhitespace source
                    ]
    in
    Shader
        { name = name
        , type_ = type_
        , uniforms = uniforms
        , varyings = varyings
        , source = glslSource
        }


precisionString : Precision -> String
precisionString precision =
    case precision of
        Highp ->
            "highp"

        Mediump ->
            "mediump"

        Lowp ->
            "lowp"


shaderPrecisionString : InternalShaderType -> String
shaderPrecisionString type_ =
    case type_ of
        Vertex _ ->
            "precision highp float;"

        Fragment precision ->
            "precision " ++ precisionString precision ++ " float;"


elmSource : Shader -> String
elmSource (Shader { name, type_, uniforms, varyings, source }) =
    String.join "\n"
        [ name ++ " :"
        , indent 1 "WebGL.Shader"
        , indent 2 (attributesSignature type_)
        , indent 2 (uniformsSignature uniforms)
        , indent 2 (varyingsSignature varyings)
        , name ++ " ="
        , indent 1 "[glsl|"
        , indent 2 source
        , indent 1 "|]"
        ]


capitalize : String -> String
capitalize string =
    String.toUpper (String.left 1 string)
        ++ String.dropLeft 1 string


elmField : Type -> String -> String
elmField (Type glslType) name =
    let
        elmType =
            if glslType == "sampler2D" then
                "Texture"

            else
                capitalize glslType
    in
    name ++ " : " ++ elmType


varyingsSignature : List Varying -> String
varyingsSignature varyings =
    case varyings of
        [] ->
            "{}"

        (Varying _ firstType firstName) :: rest ->
            String.join "\n"
                [ "{ " ++ elmField firstType firstName
                , String.join "\n" (List.map varyingField rest)
                , "}"
                ]


varyingField : Varying -> String
varyingField (Varying _ fieldType fieldName) =
    ", " ++ elmField fieldType fieldName


uniformsSignature : List Uniform -> String
uniformsSignature uniforms =
    case uniforms of
        [] ->
            "uniforms"

        (Uniform _ firstType firstName) :: rest ->
            String.join "\n"
                [ "{ uniforms"
                , "    | " ++ elmField firstType firstName
                , String.join "\n" (List.map uniformField rest)
                , "}"
                ]


uniformField : Uniform -> String
uniformField (Uniform _ fieldType fieldName) =
    "    , " ++ elmField fieldType fieldName


attributesSignature : InternalShaderType -> String
attributesSignature type_ =
    case type_ of
        Vertex attributes ->
            case attributes of
                [] ->
                    "attributes"

                (Attribute _ firstType firstName) :: rest ->
                    String.join "\n"
                        [ "{ attributes"
                        , "    | " ++ elmField firstType firstName
                        , String.join "\n" (List.map attributeField rest)
                        , "}"
                        ]

        Fragment _ ->
            "{}"


attributeField : Attribute -> String
attributeField (Attribute _ fieldType fieldName) =
    "    , " ++ elmField fieldType fieldName


indent : Int -> String -> String
indent count text =
    let
        spaces =
            String.repeat count "    "
    in
    String.lines text
        |> List.map ((++) spaces)
        |> String.join "\n"


attributesBlock : InternalShaderType -> String
attributesBlock type_ =
    case type_ of
        Vertex attributes ->
            String.join "\n" (List.map attributeLine attributes)

        Fragment _ ->
            ""


uniformsBlock : List Uniform -> String
uniformsBlock uniforms =
    String.join "\n" (List.map uniformLine uniforms)


varyingsBlock : List Varying -> String
varyingsBlock varyings =
    String.join "\n" (List.map varyingLine varyings)


attributeLine : Attribute -> String
attributeLine (Attribute precision (Type typeName) name) =
    "attribute " ++ precisionString precision ++ " " ++ typeName ++ " " ++ name ++ ";"


uniformLine : Uniform -> String
uniformLine (Uniform precision (Type typeName) name) =
    "uniform " ++ precisionString precision ++ " " ++ typeName ++ " " ++ name ++ ";"


varyingLine : Varying -> String
varyingLine (Varying precision (Type typeName) name) =
    "varying " ++ precisionString precision ++ " " ++ typeName ++ " " ++ name ++ ";"


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


generateModule : String -> List Shader -> String
generateModule moduleName shaders =
    case List.sortBy shaderName shaders of
        [] ->
            ""

        (Shader first) :: rest ->
            String.join "\n"
                [ "module " ++ moduleName ++ " exposing"
                , "    ( " ++ first.name
                , String.join "\n" (List.map exposeLine rest)
                , "    )"
                , ""
                , "import Math.Matrix4 as Matrix4 exposing (Mat4)"
                , "import Math.Vector2 as Vector2 exposing (Vec2)"
                , "import Math.Vector3 as Vector3 exposing (Vec3)"
                , "import Math.Vector4 as Vector4 exposing (Vec4)"
                , "import WebGL"
                , "import WebGL.Texture exposing (Texture)"
                , ""
                , String.join "\n" (List.map elmSource shaders)
                ]


shaderName : Shader -> String
shaderName (Shader { name }) =
    name


exposeLine : Shader -> String
exposeLine givenShader =
    "    , " ++ shaderName givenShader


shaderType : Shader -> ShaderType
shaderType (Shader { type_ }) =
    case type_ of
        Vertex _ ->
            VertexShader

        Fragment precision ->
            FragmentShader


shaderSource : Shader -> String
shaderSource (Shader { source }) =
    source


setShaderSource : String -> Shader -> Shader
setShaderSource newSource (Shader givenShader) =
    Shader { givenShader | source = newSource }
