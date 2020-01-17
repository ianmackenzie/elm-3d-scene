module Scene3d.Material exposing
    ( Material, solidColor
    , Channel, constant, load
    , unlit, diffuse, emissive, metal, nonmetal, pbr
    , Plain, ForMeshWithNormals, ForMeshWithUvs, ForMeshWithNormalsAndUvs, ForMeshWithTangents
    -- , withNormalMap
    )

{-|


## Basics

@docs Material, solidColor

@docs Channel, constant, load

@docs unlit, diffuse, emissive, metal, nonmetal, pbr

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


solidColor : Color -> Material vertexAttributes
solidColor givenColor =
    unlit (constant givenColor)


type alias Channel value vertexAttributes =
    Types.Channel value vertexAttributes


constant : value -> Channel value vertexAttributes
constant givenValue =
    Types.Constant givenValue


load : String -> Task WebGL.Texture.Error (Channel value { a | uvs : () })
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


map : (a -> b) -> Channel a vertexAttributes -> Channel b vertexAttributes
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


unlit : Channel Color vertexAttributes -> Material vertexAttributes
unlit colorChannel =
    Types.UnlitMaterial (map toVec3 colorChannel)


diffuse : Channel Color { a | normals : () } -> Material { a | normals : () }
diffuse colorChannel =
    Types.LambertianMaterial (map ColorConversions.colorToLinearRgb colorChannel)


emissive :
    { color : Channel Color vertexAttributes
    , backlight : Luminance
    }
    -> Material vertexAttributes
emissive { color, backlight } =
    Types.EmissiveMaterial
        (map ColorConversions.colorToLinearRgb color)
        (Luminance.inNits backlight)


metal :
    { baseColor : Channel Color { a | normals : () }
    , roughness : Channel Float { a | normals : () }
    }
    -> Material { a | normals : () }
metal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = constant 1 }


nonmetal :
    { baseColor : Channel Color { a | normals : () }
    , roughness : Channel Float { a | normals : () }
    }
    -> Material { a | normals : () }
nonmetal { baseColor, roughness } =
    pbr { baseColor = baseColor, roughness = roughness, metallic = constant 0 }


pbr :
    { baseColor : Channel Color { a | normals : () }
    , roughness : Channel Float { a | normals : () }
    , metallic : Channel Float { a | normals : () }
    }
    -> Material { a | normals : () }
pbr { baseColor, roughness, metallic } =
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
