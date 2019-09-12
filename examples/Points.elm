module Points exposing (main)

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
import Scene3d.Drawable as Drawable
import Scene3d.Mesh as Mesh exposing (Mesh, Points)
import Viewpoint3d exposing (Viewpoint3d)


main : Html msg
main =
    let
        points =
            Parameter1d.steps 20 <|
                Point3d.interpolateFrom
                    (Point3d.meters 1 0 -1)
                    (Point3d.meters -1 0 1)

        mesh =
            Mesh.points [] points

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = Point3d.meters 2 6 4
                , upDirection = Direction3d.positiveZ
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }
    in
    Scene3d.unlit
        [ Scene3d.pointSize (Pixels.pixels 10) ]
        { camera = camera
        , width = Pixels.pixels 800
        , height = Pixels.pixels 600
        }
        [ Drawable.colored Color.blue mesh ]
