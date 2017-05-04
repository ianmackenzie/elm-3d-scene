module OrthographicLogo exposing (..)

import OpenSolid.SceneGraph as SceneGraph
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Camera as Camera
import WebGL
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes as Attributes
import AnimationFrame
import Time
import Kintail.InputWidget as InputWidget
import Logo


view : Float -> Html Float
view angleInDegrees =
    let
        width =
            800

        height =
            600

        eyeElevation =
            degrees 20

        eyeAzimuth =
            degrees 65

        eyeDistance =
            5

        eyeX =
            eyeDistance * cos eyeElevation

        eyeZ =
            eyeDistance * sin eyeElevation

        eyePoint =
            Point3d ( eyeX, 0, eyeZ )
                |> Point3d.rotateAround Axis3d.z eyeAzimuth

        eyeFrame =
            Frame3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            Camera.orthographic
                { frame = eyeFrame
                , screenWidth = toFloat width
                , screenHeight = toFloat height
                , viewportHeight = 3
                , zNear = 0.1
                , zFar = 100
                }

        angle =
            degrees angleInDegrees

        rotatedLogo =
            Logo.node |> SceneGraph.rotateAround Axis3d.z angle

        entities =
            SceneGraph.toEntities camera rotatedLogo

        attributes =
            [ Attributes.width width, Attributes.height height ]

        sliderAttributes =
            [ Attributes.style [ ( "width", toString width ++ "px" ) ] ]

        sliderConfig =
            { min = 0
            , max = 360
            , step = 1
            }
    in
        Html.div []
            [ Html.div [] [ WebGL.toHtml attributes entities ]
            , InputWidget.slider sliderAttributes sliderConfig angleInDegrees
            ]


main : Program Never Float Float
main =
    Html.beginnerProgram
        { model = 0.0
        , view = view
        , update = always
        }
