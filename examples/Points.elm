module Points exposing (main)

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Color
import Color.Transparent
import Direction3d
import Html exposing (Html)
import Length exposing (Meters)
import Palette.Tango as Tango
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
        points =
            Parameter1d.steps 20 <|
                Point3d.interpolateFrom
                    (Point3d.meters 1 0 -1)
                    (Point3d.meters -1 0 1)

        mesh =
            Mesh.points { radius = Pixels.pixels 5 } points

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
        [ Scene3d.mesh mesh (Material.unlit Tango.skyBlue2) ]
