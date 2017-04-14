module OpenSolid.SceneGraph.Material
    exposing
        ( Material
        , solid
        )

import OpenSolid.SceneGraph.Internal.Material as Material
import Color exposing (Color)


type alias Material =
    Material.Material


solid : Color -> Material
solid =
    Material.Solid
