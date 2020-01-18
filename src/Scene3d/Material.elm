module Scene3d.Material exposing
    ( Material
    , unlit, diffuse, backlit, metal, nonmetal, pbr
    , Channel, constant, load
    , unlitTexture, diffuseTexture, backlitTexture, texturedMetal, texturedNonmetal, texturedPbr
    , Plain, ForMeshWithNormals, ForMeshWithUvs, ForMeshWithNormalsAndUvs, ForMeshWithTangents
    -- , withNormalMap
    )

{-|

@docs Material


## Simple materials

@docs unlit, diffuse, backlit, metal, nonmetal, pbr


## Textured materials

@docs Channel, constant, load

@docs unlitTexture, diffuseTexture, backlitTexture, texturedMetal, texturedNonmetal, texturedPbr


## Type aliases

@docs Plain, ForMeshWithNormals, ForMeshWithUvs, ForMeshWithNormalsAndUvs, ForMeshWithTangents

-}

import Color exposing (Color)
import Luminance exposing (Luminance)
import Math.Vector3 exposing (Vec3)
import Scene3d.Chromaticity exposing (Chromaticity)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Types as Types exposing (LinearRgb(..))
import Task exposing (Task)
import WebGL.Texture


type alias Material vertexAttributes =
    Types.Material vertexAttributes


unlit : Color -> Material vertexAttributes
unlit givenColor =
    Types.UnlitMaterial (Types.Constant (toVec3 givenColor))


diffuse : Color -> Material { a | normals : () }
diffuse materialColor =
    Types.LambertianMaterial (Types.Constant (ColorConversions.colorToLinearRgb materialColor))


backlit : Color -> Luminance -> Material vertexAttributes
backlit color backlight =
    Types.EmissiveMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb color))
        (Luminance.inNits backlight)


metal : { baseColor : Color, roughness : Float } -> Material { a | normals : () }
metal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = 1 }


nonmetal : { baseColor : Color, roughness : Float } -> Material { a | normals : () }
nonmetal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = 0 }


pbr : { baseColor : Color, roughness : Float, metallic : Float } -> Material { a | normals : () }
pbr { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (Types.Constant (ColorConversions.colorToLinearRgb baseColor))
        (Types.Constant (clamp 0 1 roughness))
        (Types.Constant (clamp 0 1 metallic))


type alias Channel value =
    Types.Channel value


constant : value -> Channel value
constant givenValue =
    Types.Constant givenValue


load : String -> Task WebGL.Texture.Error (Channel value)
load url =
    WebGL.Texture.load url
        |> Task.map
            (\texture ->
                Types.Textured
                    { url = url
                    , options = WebGL.Texture.defaultOptions
                    , data = texture
                    }
            )


map : (a -> b) -> Channel a -> Channel b
map function channel =
    case channel of
        Types.Constant value ->
            Types.Constant (function value)

        Types.Textured texture ->
            Types.Textured texture


toVec3 : Color -> Vec3
toVec3 givenColor =
    let
        ( red, green, blue ) =
            Color.toRGB givenColor
    in
    Math.Vector3.vec3 (red / 255) (green / 255) (blue / 255)


unlitTexture : Channel Color -> Material { a | uvs : () }
unlitTexture colorChannel =
    Types.UnlitMaterial (map toVec3 colorChannel)


diffuseTexture : Channel Color -> Material { a | normals : (), uvs : () }
diffuseTexture colorChannel =
    Types.LambertianMaterial (map ColorConversions.colorToLinearRgb colorChannel)


backlitTexture : Channel Color -> Luminance -> Material { a | uvs : () }
backlitTexture colorChannel backlight =
    Types.EmissiveMaterial
        (map ColorConversions.colorToLinearRgb colorChannel)
        (Luminance.inNits backlight)


texturedMetal :
    { baseColor : Channel Color
    , roughness : Channel Float
    }
    -> Material { a | normals : (), uvs : () }
texturedMetal { baseColor, roughness } =
    texturedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 1
        }


texturedNonmetal :
    { baseColor : Channel Color
    , roughness : Channel Float
    }
    -> Material { a | normals : (), uvs : () }
texturedNonmetal { baseColor, roughness } =
    texturedPbr
        { baseColor = baseColor
        , roughness = roughness
        , metallic = constant 0
        }


texturedPbr :
    { baseColor : Channel Color
    , roughness : Channel Float
    , metallic : Channel Float
    }
    -> Material { a | normals : (), uvs : () }
texturedPbr { baseColor, roughness, metallic } =
    Types.PbrMaterial
        (map ColorConversions.colorToLinearRgb baseColor)
        (map (clamp 0 1) roughness)
        (map (clamp 0 1) metallic)


type alias Plain =
    Material {}


type alias ForMeshWithNormals =
    Material { normals : () }


type alias ForMeshWithUvs =
    Material { uvs : () }


type alias ForMeshWithNormalsAndUvs =
    Material { normals : (), uvs : () }


type alias ForMeshWithTangents =
    Material { normals : (), uvs : (), tangents : () }



-- withNormalMap :
--     Texture
--     -> Material { a | uvs : (), normals : (), tangents : () }
--     -> Material { a | uvs : (), normals : (), tangents : () }
