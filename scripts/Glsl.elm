module Glsl exposing
    ( function, vertexShader, fragmentShader, generateModule
    , Attribute, Uniform, Varying, Constant, Function, Type, Shader
    , float, vec2, vec3, vec4, mat4, sampler2D
    , attribute, uniform, varying, constant
    )

{-|

@docs function, vertexShader, fragmentShader, generateModule

@docs Attribute, Uniform, Varying, Constant, Function, Type, Shader

@docs float, vec2, vec3, vec4, mat4, sampler2D

@docs attribute, uniform, varying, constant

-}


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


mat4 : Type
mat4 =
    Type "mat4"


sampler2D : Type
sampler2D =
    Type "sampler2D"


type Attribute
    = Attribute Type String


type Uniform
    = Uniform Type String


type Varying
    = Varying Type String


type Constant
    = Constant Type String String


type ShaderType
    = Vertex (List Attribute)
    | Fragment


type Shader
    = Shader String String


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
        { uniforms : List Uniform
        , varyings : List Varying
        , constants : List Constant
        , functions : List Function
        }
    -> String
    -> Shader
fragmentShader name properties source =
    shader name Fragment properties source


shader :
    String
    -> ShaderType
    ->
        { uniforms : List Uniform
        , varyings : List Varying
        , constants : List Constant
        , functions : List Function
        }
    -> String
    -> Shader
shader name shaderType { uniforms, varyings, constants, functions } source =
    let
        allFunctions =
            String.join "\n\n" (collectFunctionSources functions [])

        allConstants =
            String.join "\n" <|
                List.foldl addConstant (collectFunctionConstants functions []) constants

        glslSource =
            String.join "\n\n" <|
                List.filter (not << String.isEmpty) <|
                    [ "precision mediump float;"
                    , attributesBlock shaderType
                    , uniformsBlock uniforms
                    , varyingsBlock varyings
                    , allConstants
                    , allFunctions
                    , stripExtraWhitespace source
                    ]

        elmSource =
            String.join "\n"
                [ name ++ " :"
                , indent 1 "WebGL.Shader"
                , indent 2 (attributesSignature shaderType)
                , indent 2 (uniformsSignature uniforms)
                , indent 2 (varyingsSignature varyings)
                , name ++ " ="
                , indent 1 "[glsl|"
                , indent 2 glslSource
                , indent 1 "|]"
                ]
    in
    Shader name elmSource


capitalize : String -> String
capitalize string =
    String.toUpper (String.left 1 string)
        ++ String.dropLeft 1 string


elmField : Type -> String -> String
elmField (Type glslType) name =
    name ++ " : " ++ capitalize glslType


varyingsSignature : List Varying -> String
varyingsSignature varyings =
    case varyings of
        [] ->
            "{}"

        (Varying firstType firstName) :: rest ->
            String.join "\n"
                [ "{ " ++ elmField firstType firstName
                , String.join "\n" (List.map varyingField rest)
                , "}"
                ]


varyingField : Varying -> String
varyingField (Varying fieldType fieldName) =
    ", " ++ elmField fieldType fieldName


uniformsSignature : List Uniform -> String
uniformsSignature uniforms =
    case uniforms of
        [] ->
            "uniforms"

        (Uniform firstType firstName) :: rest ->
            String.join "\n"
                [ "{ uniforms"
                , "    | " ++ elmField firstType firstName
                , String.join "\n" (List.map uniformField rest)
                , "}"
                ]


uniformField : Uniform -> String
uniformField (Uniform fieldType fieldName) =
    "    , " ++ elmField fieldType fieldName


attributesSignature : ShaderType -> String
attributesSignature shaderType =
    case shaderType of
        Vertex attributes ->
            case attributes of
                [] ->
                    "attributes"

                (Attribute firstType firstName) :: rest ->
                    String.join "\n"
                        [ "{ attributes"
                        , "    | " ++ elmField firstType firstName
                        , String.join "\n" (List.map attributeField rest)
                        , "}"
                        ]

        Fragment ->
            "{}"


attributeField : Attribute -> String
attributeField (Attribute fieldType fieldName) =
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


attributesBlock : ShaderType -> String
attributesBlock shaderType =
    case shaderType of
        Vertex attributes ->
            String.join "\n" (List.map attributeLine attributes)

        Fragment ->
            ""


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


generateModule : String -> List Shader -> String
generateModule moduleName shaders =
    case List.sortBy shaderName shaders of
        [] ->
            ""

        (Shader firstName firstSource) :: rest ->
            String.join "\n"
                [ "module " ++ moduleName ++ " exposing"
                , "    ( " ++ firstName
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
                , String.join "\n" (List.map shaderSource shaders)
                ]


exposeLine : Shader -> String
exposeLine givenShader =
    "    , " ++ shaderName givenShader


shaderName : Shader -> String
shaderName (Shader name _) =
    name


shaderSource (Shader _ source) =
    source
