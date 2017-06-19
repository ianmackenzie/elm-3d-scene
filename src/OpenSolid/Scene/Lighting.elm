module OpenSolid.Scene.Lighting
    exposing
        ( Lighting
        , singleLight
        )

import OpenSolid.Scene.Light exposing (Light)
import OpenSolid.Scene.Types as Types


type alias Lighting =
    Types.Lighting


singleLight : Light -> Lighting
singleLight =
    Types.SingleLight
