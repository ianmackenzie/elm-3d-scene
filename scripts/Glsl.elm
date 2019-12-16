module Glsl exposing
    ( Function
    , Type(..)
    , function
    , program
    )


type Function
    = Function
        { source : String
        , dependencies : List Function
        }


type Type
    = Float
    | Vec2
    | Vec3
    | Vec4
    | Mat3
    | Mat4
    | Texture


function : String -> List Function -> Function
function source dependencies =
    Function { source = source, dependencies = dependencies }


functionSource : Function -> String
functionSource (Function { source }) =
    source


type alias Types =
    { attributes : List ( String, Type )
    , uniforms : List ( String, Type )
    , varying : List ( String, Type )
    }


program : Types -> String -> List Function -> String
program types source dependencies =
    let
        shaderSource =
            String.join "\n" (source :: List.map functionSource dependencies)
    in
    ""
