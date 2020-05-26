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
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import Viewpoint3d exposing (Viewpoint3d)


main : Html msg
main =
    let
        mesh =
            Mesh.points { radius = Pixels.pixels 5 } <|
                Parameter1d.steps 12 <|
                    Point3d.interpolateFrom
                        (Point3d.meters 1 0 -1)
                        (Point3d.meters -1 0 1)

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
        , dimensions = ( Pixels.pixels 300, Pixels.pixels 300 )
        , background = Scene3d.transparentBackground
        , clipDepth = Length.meters 0.1
        , entities = [ Scene3d.mesh (Material.color Color.blue) mesh ]
        }
