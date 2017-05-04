module OrthographicLogo exposing (..)

import OpenSolid.SceneGraph as SceneGraph
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Direction2d as Direction2d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Point2d as Point2d
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.LineSegment3d as LineSegment3d
import OpenSolid.Svg as Svg
import Svg exposing (Svg)
import Svg.Attributes
import WebGL
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes as Attributes
import Kintail.InputWidget as InputWidget
import Logo


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
            Point3d ( 4, 0, 0 )
                |> Point3d.rotateAround Axis3d.y (degrees -22.5)
                |> Point3d.rotateAround Axis3d.z (degrees 60)

        eyeFrame =
            Frame3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            case projectionType of
                Perspective ->
                    Camera.perspective
                        { frame = eyeFrame
                        , screenWidth = toFloat width
                        , screenHeight = toFloat height
                        , verticalFov = degrees 30
                        , zNear = 0.1
                        , zFar = 100
                        }

                Orthographic ->
                    Camera.orthographic
                        { frame = eyeFrame
                        , screenWidth = toFloat width
                        , screenHeight = toFloat height
                        , viewportHeight = 2
                        , zNear = 0.1
                        , zFar = 100
                        }

        angle =
            degrees angleInDegrees

        rotatedLogo =
            Logo.node |> SceneGraph.rotateAround Axis3d.z angle

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
                            (Circle2d { centerPoint = vertex, radius = 7 })
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
                            (vertex |> Point2d.translateBy (Vector2d ( 10, 0 )))
                            ("p" ++ toString index)
                    )

        topLeftFrame =
            Frame2d
                { originPoint = Point2d ( 0, height )
                , xDirection = Direction2d.positiveX
                , yDirection = Direction2d.negativeY
                }

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
            SceneGraph.toEntities camera rotatedLogo

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
