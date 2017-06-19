module OpenSolid.Scene.Light
    exposing
        ( Light
        , directional
        )

import Color exposing (Color)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Scene.Types as Types


type alias Light =
    Types.Light


directional : Color -> Direction3d -> Light
directional =
    Types.DirectionalLight
