module OpenSolid.Scene.Light
    exposing
        ( Light
        , directional
        )

import Color exposing (Color)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Scene.Types as Types
import OpenSolid.WebGL.Color as Color
import OpenSolid.WebGL.Direction3d as Direction3d


type alias Light =
    Types.Light


directional : Color -> Direction3d -> Light
directional color direction =
    Types.DirectionalLight
        { color = Color.toVec3 color
        , direction = Direction3d.toVec3 (Direction3d.flip direction)
        }
