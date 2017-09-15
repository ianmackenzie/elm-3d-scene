module PerspectiveLogos exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import Logo
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Camera as Camera
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Node as Node
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Viewpoint as Viewpoint


view : Float -> Html Float
view angleInDegrees =
    let
        width =
            800

        height =
            600

        eyePoint =
            Point3d.fromCoordinates ( 10, 0, 0 )
                |> Point3d.rotateAround Axis3d.y (degrees -22.5)
                |> Point3d.rotateAround Axis3d.z (degrees 60)

        viewpoint =
            Viewpoint.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            Camera.perspective
                { viewpoint = viewpoint
                , screenWidth = toFloat width
                , screenHeight = toFloat height
                , verticalFieldOfView = degrees 30
                , nearClipDistance = 0.1
                , farClipDistance = 100
                }

        angle =
            degrees angleInDegrees

        rotatedLogo =
            Logo.node |> Node.rotateAround Axis3d.z angle

        rightLogo =
            rotatedLogo
                |> Node.translateBy (Vector3d.fromComponents ( 1, 1, 0.75 ))

        leftLogo =
            rightLogo |> Node.mirrorAcross Plane3d.zx

        frontLogos =
            Node.group [ rightLogo, leftLogo ]

        backLogos =
            frontLogos |> Node.mirrorAcross Plane3d.yz

        topLogos =
            Node.group [ frontLogos, backLogos ]

        bottomLogos =
            topLogos |> Node.mirrorAcross Plane3d.xy

        scene =
            Node.group [ topLogos, bottomLogos ]

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
