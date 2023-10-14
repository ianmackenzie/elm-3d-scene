module LineSegments exposing (main)

import Angle exposing (Angle)
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Polyline3d
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh


main : Html msg
main =
    let
        mesh =
            Mesh.polyline <|
                Polyline3d.fromVertices
                    [ Point3d.meters 1 0 0
                    , Point3d.meters 0 0 0
                    , Point3d.meters 0 0 1
                    , Point3d.meters 0 1 1
                    ]

        camera =
            Camera3d.lookAt
                { focalPoint = Point3d.meters 0 0 0.5
                , eyePoint = Point3d.meters 3 3 2
                , upDirection = Direction3d.z
                , fov = Camera3d.angle (Angle.degrees 30)
                , projection = Camera3d.Perspective
                }
    in
    Scene3d.unlit
        { camera = camera
        , clipDepth = Length.meters 0.1
        , dimensions = ( Pixels.pixels 300, Pixels.pixels 300 )
        , background = Scene3d.transparentBackground
        , entities = [ Scene3d.mesh (Material.color Color.blue) mesh ]
        }
