module Rotation exposing (main)

import Angle exposing (Angle)
import Axis3d
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import Triangle3d
import Viewpoint3d


main : Html msg
main =
    let
        triangle1 =
            Triangle3d.from
                (Point3d.meters 0 0 0)
                (Point3d.meters 1 0 0)
                (Point3d.meters 1 1 0)

        triangle2 =
            Triangle3d.from
                (Point3d.meters 0 0 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters 0 1 0)

        mesh1 =
            Mesh.triangles [ triangle1 ]

        mesh2 =
            Mesh.triangles [ triangle2 ]

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.meters 0.5 2 0
                        , eyePoint = Point3d.meters 9 5 4
                        , upDirection = Direction3d.z
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        square =
            Scene3d.group
                [ Scene3d.mesh (Material.color Color.orange) mesh1
                , Scene3d.mesh (Material.color Color.blue) mesh2
                ]

        rotationAxis =
            Axis3d.through (Point3d.meters 0 2 0) Direction3d.x

        angles =
            Parameter1d.leading 16 <|
                Quantity.interpolateFrom
                    (Angle.degrees 0)
                    (Angle.degrees 360)

        rotatedSquare angle =
            square |> Scene3d.rotateAround rotationAxis angle
    in
    Scene3d.unlit
        { camera = camera
        , clipDepth = Length.meters 0.1
        , dimensions = ( Pixels.pixels 300, Pixels.pixels 300 )
        , background = Scene3d.transparentBackground
        , entities = List.map rotatedSquare angles
        }
