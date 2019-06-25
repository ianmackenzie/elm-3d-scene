module Scene3d.Material exposing
    ( Material
    , emissive
    , metal
    , nonmetal
    )

import Color exposing (Color)
import Scene3d.Types as Types


type alias Material =
    Types.Material


flat : Color -> Material
flat color =
    Types.SimpleMaterial Types.FlatColor color


metal : { color : Color, roughness : Float } -> Material
metal { color, roughness } =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    Types.PhysicalMaterial
        { r = red
        , g = green
        , b = blue
        , rg = roughness
        , mt = 1.0
        }


nonmetal : { color : Color, roughness : Float } -> Material
nonmetal { color, roughness } =
    let
        { red, green, blue } =
            Color.toRgba color
    in
    Types.PhysicalMaterial
        { r = red
        , g = green
        , b = blue
        , rg = roughness
        , mt = 0.0
        }


emissive : Color -> Material
emissive color =
    Types.SimpleMaterial Types.EmissiveColor color
