module Overlay exposing (..)

import Angle exposing (Angle)
import Axis2d exposing (Axis2d)
import Axis3d
import Browser
import Camera3d
import Circle2d
import Direction2d exposing (Direction2d)
import Direction3d
import Element
import Element.Border
import Element.Input as Input
import Frame2d
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as Attributes
import Length
import LineSegment3d
import LineSegment3d.Projection as LineSegment3d
import Logo
import Pixels exposing (Pixels)
import Point2d
import Point3d
import Point3d.Projection as Point3d
import Quantity
import Rectangle2d
import Scene3d
import Svg exposing (Svg)
import Svg.Attributes
import Vector2d
import Viewpoint3d
import WebGL


type ProjectionType
    = Perspective
    | Orthographic


type Msg
    = SetAngle Angle
    | SetProjectionType ProjectionType


type alias Model =
    { angle : Angle
    , projectionType : ProjectionType
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { angle = Angle.degrees 0, projectionType = Perspective }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        SetAngle angle ->
            ( { model | angle = angle }, Cmd.none )

        SetProjectionType projectionType ->
            ( { model | projectionType = projectionType }, Cmd.none )


view : Model -> Html Msg
view { angle, projectionType } =
    let
        width =
            800

        height =
            600

        eyePoint =
            Point3d.meters 4 0 0
                |> Point3d.rotateAround Axis3d.y (Angle.degrees -22.5)
                |> Point3d.rotateAround Axis3d.z (Angle.degrees 60)

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }

        camera =
            case projectionType of
                Perspective ->
                    Camera3d.perspective
                        { viewpoint = viewpoint
                        , verticalFieldOfView = Angle.degrees 30
                        }

                Orthographic ->
                    Camera3d.orthographic
                        { viewpoint = viewpoint
                        , viewportHeight = Length.meters 2
                        }

        -- Take the 3D model for the logo and rotate it by the current angle
        rotatedLogo =
            Logo.entity |> Scene3d.rotateAround Axis3d.z angle

        -- Defines the shape of the 'screen' that we will be using when
        -- projecting 3D points into 2D
        screenRectangle =
            Rectangle2d.from Point2d.origin (Point2d.pixels width height)

        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        vertices2d =
            Logo.vertices
                |> List.map (Point3d.rotateAround Axis3d.z angle)
                |> List.map (Point3d.toScreenSpace camera screenRectangle)

        -- Create an SVG circle at each projected 2D position
        svgCircles =
            vertices2d
                |> List.map
                    (\vertex ->
                        Svg.circle2d
                            [ Svg.Attributes.stroke "grey"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.fill "none"
                            ]
                            (Circle2d.withRadius (Pixels.float 6) vertex)
                    )

        -- Project all logo edges and create SVG elements in a similar way
        svgLines =
            Logo.edges
                |> List.map (LineSegment3d.rotateAround Axis3d.z angle)
                |> List.map (LineSegment3d.toScreenSpace camera screenRectangle)
                |> List.map
                    (\edge ->
                        Svg.lineSegment2d
                            [ Svg.Attributes.stroke "grey"
                            , Svg.Attributes.strokeWidth "2"
                            , Svg.Attributes.strokeDasharray "5 5"
                            ]
                            edge
                    )

        -- Create text SVG labels beside each projected 2D point
        svgLabels =
            vertices2d
                |> List.indexedMap
                    (\index vertex ->
                        Svg.text_
                            [ Svg.Attributes.fill "rgb(92, 92, 92)"
                            , Svg.Attributes.fontFamily "monospace"
                            , Svg.Attributes.fontSize "20px"
                            , Svg.Attributes.stroke "none"
                            , Svg.Attributes.x (String.fromFloat (Pixels.toFloat (Point2d.xCoordinate vertex) + 10))
                            , Svg.Attributes.y (String.fromFloat (Pixels.toFloat (Point2d.yCoordinate vertex)))
                            ]
                            [ Svg.text ("p" ++ String.fromInt index) ]
                            -- Hack: flip the text upside down since our later
                            -- 'Svg.relativeTo topLeftFrame' call will flip it
                            -- back right side up
                            |> Svg.mirrorAcross (Axis2d.through vertex Direction2d.x)
                    )

        -- Used for converting from coordinates relative to the bottom-left
        -- corner of the 2D drawing into coordinates relative to the top-left
        -- corner (which is what SVG natively works in)
        topLeftFrame =
            Frame2d.atPoint (Point2d.xy Quantity.zero (Pixels.float height))
                |> Frame2d.reverseY

        -- Create an SVG element with the projected points, lines and
        -- associated labels
        svgElement =
            Svg.svg
                [ Attributes.width width
                , Attributes.height height
                ]
                [ Svg.relativeTo topLeftFrame
                    (Svg.g [] (svgCircles ++ svgLines ++ svgLabels))
                ]

        -- Render a 3D scene
        sceneElement =
            Scene3d.unlit
                { entities = [ rotatedLogo ]
                , camera = camera
                , dimensions = ( Pixels.int width, Pixels.int height )
                , clipDepth = Length.centimeters 10
                , background = Scene3d.transparentBackground
                }

        -- Slider for controlling rotation angle
        slider =
            Input.slider
                [ Element.Border.solid
                , Element.Border.color (Element.rgb255 127 127 127)
                , Element.Border.width 1
                , Element.Border.rounded 9
                ]
                { onChange = Angle.degrees >> SetAngle
                , label = Input.labelLeft [] (Element.text "Angle:")
                , min = 0
                , max = 360
                , value = Angle.inDegrees angle
                , thumb = Input.defaultThumb
                , step = Just 1
                }

        -- A couple radio buttons for toggling between perspective and
        -- orthographic projection
        radioButtons =
            Input.radio [ Element.padding 6 ]
                { onChange = SetProjectionType
                , selected = Just projectionType
                , label = Input.labelAbove [] (Element.text "Projection type:")
                , options =
                    [ Input.option Perspective (Element.text "Perspective")
                    , Input.option Orthographic (Element.text "Orthographic")
                    ]
                }
    in
    Element.layout [] <|
        Element.column [ Element.spacing 12 ] <|
            [ -- Use Element.inFront to place the SVG element in front of the
              --WebGL element
              Element.el
                [ Element.inFront (Element.html svgElement) ]
                (Element.html sceneElement)
            , slider
            , radioButtons
            ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
