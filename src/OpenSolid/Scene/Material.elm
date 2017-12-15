module OpenSolid.Scene.Material
    exposing
        ( Material
        , emissive
        , metal
        , nonmetal
        )

import Color exposing (Color)
import OpenSolid.Scene.Types as Types


type alias Material =
    Types.Material


flat : Color -> Material
flat color =
    Types.SimpleMaterial Types.FlatColor color


metal : { color : Color, roughness : Float } -> Material
metal { color, roughness } =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Types.PhysicalMaterial
        { r = toFloat red / 255
        , g = toFloat green / 255
        , b = toFloat blue / 255
        , rg = roughness
        , mt = 1.0
        }


nonmetal : { color : Color, roughness : Float } -> Material
nonmetal { color, roughness } =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    Types.PhysicalMaterial
        { r = toFloat red / 255
        , g = toFloat green / 255
        , b = toFloat blue / 255
        , rg = roughness
        , mt = 0.0
        }


emissive : Color -> Material
emissive color =
    Types.SimpleMaterial Types.EmissiveColor color
