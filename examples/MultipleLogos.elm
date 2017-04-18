module MultipleLogos exposing (..)

import OpenSolid.SceneGraph as SceneGraph
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.WebGL.Frame3d as Frame3d
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

        eyeDistance =
            10

        eyeX =
            eyeDistance * cos eyeElevation

        eyeZ =
            eyeDistance * sin eyeElevation

        eyePoint =
            Point3d ( eyeX, 0, eyeZ )
                |> Point3d.rotateAround Axis3d.z (degrees 30)

        eyeFrame =
            Frame3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        projection =
            SceneGraph.perspectiveProjection
                { verticalFov = degrees 30
                , aspectRatio = width / height
                , zNear = 0.1
                , zFar = 100
                }

        angle =
            degrees angleInDegrees

        rotatedLogo =
            Logo.node |> SceneGraph.rotateAround Axis3d.z angle

        rightLogo =
            rotatedLogo |> SceneGraph.translateBy (Vector3d ( 1, 1, 0.75 ))

        leftLogo =
            rightLogo |> SceneGraph.mirrorAcross Plane3d.zx

        frontLogos =
            SceneGraph.group [ rightLogo, leftLogo ]

        backLogos =
            frontLogos |> SceneGraph.mirrorAcross Plane3d.yz

        topLogos =
            SceneGraph.group [ frontLogos, backLogos ]

        bottomLogos =
            topLogos |> SceneGraph.mirrorAcross Plane3d.xy

        scene =
            SceneGraph.group [ topLogos, bottomLogos ]

        entities =
            SceneGraph.toEntities eyeFrame projection scene

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
