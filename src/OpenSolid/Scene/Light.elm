module OpenSolid.Scene.Light
    exposing
        ( AmbientLookupTexture
        , Light
        , ambient
        , directional
        , loadAmbientLookupTextureFrom
        , point
        )

import Math.Vector3 exposing (vec3)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Interop.LinearAlgebra.Direction3d as Direction3d
import OpenSolid.Interop.LinearAlgebra.Point3d as Point3d
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Scene.Types as Types
import Task exposing (Task)
import WebGL exposing (Texture)
import WebGL.Texture


type alias Light =
    Types.Light


type AmbientLookupTexture
    = AmbientLookupTexture WebGL.Texture


ambient : AmbientLookupTexture -> ( Float, Float, Float ) -> Light
ambient (AmbientLookupTexture lookupTexture) ( r, g, b ) =
    Types.AmbientLight
        { color = vec3 r g b
        , lookupTexture = lookupTexture
        }


directional : Direction3d -> ( Float, Float, Float ) -> Light
directional direction ( r, g, b ) =
    Types.DirectionalLight
        { color = vec3 r g b
        , direction = Direction3d.toVec3 (Direction3d.flip direction)
        }


point : Point3d -> ( Float, Float, Float ) -> Light
point position ( r, g, b ) =
    Types.PointLight
        { color = vec3 r g b
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
