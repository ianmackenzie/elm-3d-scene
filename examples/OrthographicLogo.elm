module OrthographicLogo exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import Logo
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Camera as Camera
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Drawable as Drawable
import OpenSolid.Viewpoint as Viewpoint


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
            Point3d.fromCoordinates ( eyeX, 0, eyeZ )
                |> Point3d.rotateAround Axis3d.z eyeAzimuth

        viewpoint =
            Viewpoint.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            Camera.orthographic
                { viewpoint = viewpoint
                , screenWidth = toFloat width
                , screenHeight = toFloat height
                , viewportHeight = 3
                , nearClipDistance = 0.1
                , farClipDistance = 100
                }

        angle =
            degrees angleInDegrees

        scene =
            Logo.drawable
                |> Drawable.rotateAround Axis3d.z angle

        sliderAttributes =
            [ Attributes.style [ ( "width", toString width ++ "px" ) ] ]

        sliderConfig =
            { min = 0
            , max = 360
            , step = 1
            }
    in
    Html.div []
        [ Html.div [] [ Scene.render [] camera scene ]
        , InputWidget.slider sliderAttributes sliderConfig angleInDegrees
        ]


main : Program Never Float Float
main =
    Html.beginnerProgram
        { model = 0.0
        , view = view
        , update = always
        }
