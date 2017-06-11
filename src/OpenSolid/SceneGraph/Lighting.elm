module OpenSolid.SceneGraph.Lighting
    exposing
        ( Light
        , Lighting
        , directional
        , single
        )

import Color exposing (Color)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.SceneGraph.Types as Types


type alias Light =
    Types.Light


type alias Lighting =
    Types.Lighting


directional : Color -> Direction3d -> Light
directional =
    Types.DirectionalLight


single : Light -> Lighting
single =
    Types.SingleLight
