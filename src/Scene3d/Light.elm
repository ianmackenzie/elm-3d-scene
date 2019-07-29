module Scene3d.Light exposing
    ( AmbientLookupTexture
    , Light
    , ambient
    , directional
    , loadAmbientLookupTextureFrom
    , point
    )

import Direction3d exposing (Direction3d)
import Geometry.Interop.LinearAlgebra.Direction3d as Direction3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Math.Vector3 exposing (vec3)
import Point3d exposing (Point3d)
import Scene3d.Types as Types
import Task exposing (Task)
import WebGL.Texture exposing (Texture)


type alias Light units coordinates =
    Types.Light units coordinates


type AmbientLookupTexture
    = AmbientLookupTexture Texture


ambient : AmbientLookupTexture -> ( Float, Float, Float ) -> Light units coordinates
ambient (AmbientLookupTexture lookupTexture) ( r, g, b ) =
    Types.AmbientLight
        { color = vec3 r g b
        , lookupTexture = lookupTexture
        }


directional : Direction3d coordinates -> ( Float, Float, Float ) -> Light units coordinates
directional direction ( r, g, b ) =
    Types.DirectionalLight
        { color = vec3 r g b
        , direction = Direction3d.toVec3 (Direction3d.reverse direction)
        }


point : Point3d units coordinates -> ( Float, Float, Float ) -> Light units coordinates
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
