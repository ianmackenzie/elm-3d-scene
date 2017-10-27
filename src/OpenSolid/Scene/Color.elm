module OpenSolid.Scene.Color exposing (toVec3)

import Color exposing (Color)
import Math.Vector3 exposing (Vec3, vec3)


toVec3 : Color -> Vec3
toVec3 color =
    let
        { red, green, blue } =
            Color.toRgb color
    in
    vec3
        (toFloat red / 255.0)
        (toFloat green / 255.0)
        (toFloat blue / 255.0)
