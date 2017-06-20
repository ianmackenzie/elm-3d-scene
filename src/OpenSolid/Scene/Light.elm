module OpenSolid.Scene.Light
    exposing
        ( AmbientLookupTexture
        , Light
        , ambient
        , directional
        , loadAmbientLookupTexture
        )

import Color exposing (Color)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Scene.Types as Types
import OpenSolid.WebGL.Color as Color
import OpenSolid.WebGL.Direction3d as Direction3d
import Task exposing (Task)
import WebGL exposing (Texture)
import WebGL.Texture


type alias Light =
    Types.Light


type AmbientLookupTexture
    = AmbientLookupTexture WebGL.Texture


ambient : AmbientLookupTexture -> Color -> Light
ambient (AmbientLookupTexture lookupTexture) color =
    Types.AmbientLight lookupTexture (Color.toVec3 color)


directional : Color -> Direction3d -> Light
directional color direction =
    Types.DirectionalLight
        { color = Color.toVec3 color
        , direction = Direction3d.toVec3 (Direction3d.flip direction)
        }


loadAmbientLookupTexture : String -> Task WebGL.Texture.Error AmbientLookupTexture
loadAmbientLookupTexture url =
    let
        options =
            { magnify = WebGL.Texture.linear
            , minify = WebGL.Texture.linear
            , horizontalWrap = WebGL.Texture.clampToEdge
            , verticalWrap = WebGL.Texture.clampToEdge
            , flipY = True
            }
    in
    WebGL.Texture.loadWith options url |> Task.map AmbientLookupTexture
