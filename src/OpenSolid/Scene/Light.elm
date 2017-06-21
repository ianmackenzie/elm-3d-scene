module OpenSolid.Scene.Light
    exposing
        ( AmbientLookupTexture
        , Light
        , ambient
        , directional
        , loadAmbientLookupTextureFrom
        , point
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Scene.Types as Types
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import Task exposing (Task)
import WebGL exposing (Texture)
import WebGL.Texture


type alias Light =
    Types.Light


type AmbientLookupTexture
    = AmbientLookupTexture WebGL.Texture


ambient : AmbientLookupTexture -> Vec3 -> Light
ambient (AmbientLookupTexture lookupTexture) color =
    Types.AmbientLight
        { color = color
        , lookupTexture = lookupTexture
        }


directional : Direction3d -> Vec3 -> Light
directional direction color =
    Types.DirectionalLight
        { color = color
        , direction = Direction3d.toVec3 (Direction3d.flip direction)
        }


point : Point3d -> Vec3 -> Light
point position color =
    Types.PointLight
        { color = color
        , position = Point3d.toVec3 position
        }


loadAmbientLookupTextureFrom : String -> Task WebGL.Texture.Error AmbientLookupTexture
loadAmbientLookupTextureFrom url =
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
