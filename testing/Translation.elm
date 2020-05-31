module Translation exposing (main)

import Angle exposing (Angle)
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
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
                        { focalPoint = Point3d.meters 0.5 0.5 0.75
                        , eyePoint = Point3d.meters 4 3 1.8
                        , upDirection = Direction3d.z
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        square =
            Scene3d.group
                [ Scene3d.mesh (Material.color Color.orange) mesh1
                , Scene3d.mesh (Material.color Color.blue) mesh2
                ]

        squareAtHeight height =
            square |> Scene3d.translateIn Direction3d.z height
    in
    Scene3d.unlit
        { camera = camera
        , dimensions = ( Pixels.pixels 300, Pixels.pixels 300 )
        , background = Scene3d.transparentBackground
        , clipDepth = Length.meters 0.1
        , entities =
            [ squareAtHeight (Length.meters 0)
            , squareAtHeight (Length.meters 0.5)
            , squareAtHeight (Length.meters 1)
            , squareAtHeight (Length.meters 1.5)
            ]
        }
