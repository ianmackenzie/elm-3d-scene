module Triangles exposing (main)

{-| This example is very similar to the HelloWorld example but shows how to
render individual triangles.
-}

import Angle exposing (Angle)
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Triangle3d
import Viewpoint3d


main : Html msg
main =
    let
        -- Create an orange triangle using the Triangle3d type from elm-geometry
        triangle1 =
            Scene3d.triangle (Material.color Color.orange) <|
                Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 0 0)
                    (Point3d.meters 1 1 0)

        -- Create a blue triangle the same way
        triangle2 =
            Scene3d.triangle (Material.color Color.blue) <|
                Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 1 0)
                    (Point3d.meters 0 1 0)

        -- Create a camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.meters 0.5 0.5 0
                        , eyePoint = Point3d.meters 3 1 1
                        , upDirection = Direction3d.z
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Scene3d.unlit
        { entities = [ triangle1, triangle2 ]
        , camera = camera
        , clipDepth = Length.meters 0.1
        , dimensions = ( Pixels.int 400, Pixels.int 300 )
        , background = Scene3d.transparentBackground
        }
