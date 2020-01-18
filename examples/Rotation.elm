module Rotation exposing (main)

import Angle exposing (Angle)
import Axis3d
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Palette.Tango as Tango
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

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.meters 0.5 2 0
                , eyePoint = Point3d.meters 9 5 4
                , upDirection = Direction3d.z
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = Length.meters 0.1
                }

        square =
            Scene3d.group
                [ Scene3d.mesh mesh1 (Material.unlit Tango.orange2)
                , Scene3d.mesh mesh2 (Material.unlit Tango.skyBlue2)
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
    Scene3d.toHtml
        { camera = camera
        , width = Pixels.pixels 800
        , height = Pixels.pixels 600
        , directLighting = Scene3d.noDirectLighting
        , environmentalLighting = Scene3d.noEnvironmentalLighting
        , backgroundColor = Scene3d.transparentBackground
        , exposure = Scene3d.defaultExposure
        , whiteBalance = Scene3d.defaultWhiteBalance
        }
        (List.map rotatedSquare angles)
