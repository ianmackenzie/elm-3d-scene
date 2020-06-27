module Points exposing (main)

{-| This example shows how to render points in 3D as dots with particular radius
in pixels (the dots will have the same radius no matter how near/far away they
are from the camera).
-}

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Html exposing (Html)
import Length exposing (Meters)
import Parameter1d
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d exposing (Viewpoint3d)


main : Html msg
main =
    let
        -- Generate 13 evenly-spaced points by taking 12 steps from (1, 0, -1)
        -- to (-1, 0, 1); see https://package.elm-lang.org/packages/ianmackenzie/elm-1d-parameter/latest/Parameter1d#steps
        -- and https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Point3d#interpolateFrom
        -- for details
        points =
            Parameter1d.steps 12 <|
                Point3d.interpolateFrom
                    (Point3d.meters 1 0 -1)
                    (Point3d.meters -1 0 1)

        -- Convert the points to a list of entities by providing a radius and
        -- color for each point
        pointEntities =
            points
                |> List.map
                    (\point ->
                        Scene3d.point { radius = Pixels.float 5 }
                            (Material.color Color.blue)
                            point
                    )

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 2 6 4
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Scene3d.unlit
        { camera = camera
        , dimensions = ( Pixels.int 300, Pixels.int 300 )
        , background = Scene3d.transparentBackground
        , clipDepth = Length.meters 0.1
        , entities = pointEntities
        }
