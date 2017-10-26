module Overlay exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import Logo
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Camera as Camera
import OpenSolid.Camera.LineSegment3d as LineSegment3d
import OpenSolid.Camera.Point3d as Point3d
import OpenSolid.Circle2d as Circle2d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Drawable as Drawable
import OpenSolid.Svg as Svg
import OpenSolid.Vector2d as Vector2d
import OpenSolid.Viewpoint as Viewpoint
import Svg exposing (Svg)
import Svg.Attributes
import WebGL


type ProjectionType
    = Perspective
    | Orthographic


type Msg
    = SetAngleInDegrees Float
    | SetProjectionType ProjectionType


type alias Model =
    { angleInDegrees : Float
    , projectionType : ProjectionType
    }


update : Msg -> Model -> Model
update message model =
    case message of
        SetAngleInDegrees angleInDegrees ->
            { model | angleInDegrees = angleInDegrees }

        SetProjectionType projectionType ->
            { model | projectionType = projectionType }


view : Model -> Html Msg
view { angleInDegrees, projectionType } =
    let
        width =
            800

        height =
            600

        eyePoint =
            Point3d.fromCoordinates ( 4, 0, 0 )
                |> Point3d.rotateAround Axis3d.y (degrees -22.5)
                |> Point3d.rotateAround Axis3d.z (degrees 60)

        viewpoint =
            Viewpoint.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            case projectionType of
                Perspective ->
                    Camera.perspective
                        { viewpoint = viewpoint
                        , screenWidth = toFloat width
                        , screenHeight = toFloat height
                        , verticalFieldOfView = degrees 30
                        , nearClipDistance = 0.1
                        , farClipDistance = 100
                        }

                Orthographic ->
                    Camera.orthographic
                        { viewpoint = viewpoint
                        , screenWidth = toFloat width
                        , screenHeight = toFloat height
                        , viewportHeight = 2
                        , nearClipDistance = 0.1
                        , farClipDistance = 100
                        }

        angle =
            degrees angleInDegrees

        rotatedLogo =
            Logo.drawable |> Drawable.rotateAround Axis3d.z angle

        vertices2d =
            Logo.vertices
                |> List.map (Point3d.rotateAround Axis3d.z angle)
                |> List.map (Point3d.toScreenSpace camera)

        svgCircles =
            vertices2d
                |> List.map
                    (\vertex ->
                        Svg.circle2d
                            [ Svg.Attributes.stroke "grey"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "none"
                            ]
                            (Circle2d.with { centerPoint = vertex, radius = 7 })
                    )

        svgLines =
            Logo.edges
                |> List.map (LineSegment3d.rotateAround Axis3d.z angle)
                |> List.map (LineSegment3d.toScreenSpace camera)
                |> List.map
                    (\edge ->
                        Svg.lineSegment2d
                            [ Svg.Attributes.stroke "grey"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeDasharray "5 5"
                            ]
                            edge
                    )

        svgLabels =
            vertices2d
                |> List.indexedMap
                    (\index vertex ->
                        Svg.text2d
                            [ Svg.Attributes.fill "rgb(92, 92, 92)"
                            , Svg.Attributes.fontFamily "monospace"
                            , Svg.Attributes.fontSize "20px"
                            ]
                            (vertex
                                |> Point2d.translateBy
                                    (Vector2d.fromComponents ( 10, 0 ))
                            )
                            ("p" ++ toString index)
                    )

        topLeftFrame =
            Frame2d.atPoint (Point2d.fromCoordinates ( 0, height ))
                |> Frame2d.flipY

        svgElement =
            Svg.svg
                [ Attributes.width width
                , Attributes.height height
                , Attributes.style
                    [ ( "position", "absolute" )
                    , ( "z-index", "1" )
                    ]
                ]
                [ Svg.relativeTo topLeftFrame
                    (Svg.g [] (svgCircles ++ svgLines ++ svgLabels))
                ]

        entities =
            Scene.toEntities [] camera rotatedLogo

        attributes =
            [ Attributes.width width
            , Attributes.height height
            , Attributes.style
                [ ( "position", "absolute" )
                , ( "z-index", "1" )
                ]
            ]

        sliderAttributes =
            [ Attributes.style [ ( "width", toString width ++ "px" ) ] ]

        sliderConfig =
            { min = 0
            , max = 360
            , step = 1
            }

        slider =
            Html.div []
                [ InputWidget.slider sliderAttributes
                    sliderConfig
                    angleInDegrees
                    |> Html.map SetAngleInDegrees
                ]

        radioButton ownProjectionType =
            let
                label =
                    toString ownProjectionType

                id =
                    String.toLower label
            in
            Html.div []
                [ InputWidget.radioButton [ Attributes.id id ]
                    ownProjectionType
                    projectionType
                    |> Html.map SetProjectionType
                , Html.label [ Attributes.for id ] [ Html.text label ]
                ]
    in
    Html.div []
        [ Html.div
            [ Attributes.style
                [ ( "position", "relative" )
                , ( "width", toString width ++ "px" )
                , ( "height", toString height ++ "px" )
                ]
            ]
            [ WebGL.toHtml attributes entities
            , svgElement
            ]
        , slider
        , radioButton Perspective
        , radioButton Orthographic
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = { angleInDegrees = 0.0, projectionType = Perspective }
        , view = view
        , update = update
        }
