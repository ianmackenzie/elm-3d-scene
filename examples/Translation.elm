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
import Scene3d.Drawable as Drawable
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
            Mesh.triangles [] [ triangle1 ]

        mesh2 =
            Mesh.triangles [] [ triangle2 ]

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0.5 0.5 0.75
                , eyePoint = Point3d.meters 4 3 2
                , upDirection = Direction3d.z
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }

        square =
            Drawable.group
                [ Drawable.colored Color.orange mesh1
                , Drawable.colored Color.blue mesh2
                ]

        squareAtHeight height =
            square |> Drawable.translateIn Direction3d.z height
    in
    Scene3d.unlit []
        { camera = camera
        , width = Pixels.pixels 800
        , height = Pixels.pixels 600
        }
        [ squareAtHeight (Length.meters 0)
        , squareAtHeight (Length.meters 0.5)
        , squareAtHeight (Length.meters 1)
        , squareAtHeight (Length.meters 1.5)
        ]
