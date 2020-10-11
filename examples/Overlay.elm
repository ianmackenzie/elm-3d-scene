module Overlay exposing (..)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis2d exposing (Axis2d)
import Axis3d
import Browser
import Camera3d
import Circle2d
import Color exposing (Color)
import Direction2d exposing (Direction2d)
import Direction3d
import Element
import Element.Border
import Element.Input as Input
import Frame2d
import Frame3d exposing (Frame3d)
import Geometry.Svg as Svg
import Html exposing (Html)
import Html.Attributes as Attributes
import Length exposing (Length, Meters)
import LineSegment3d exposing (LineSegment3d)
import LineSegment3d.Projection as LineSegment3d
import Pixels exposing (Pixels)
import Point2d
import Point3d exposing (Point3d)
import Point3d.Projection as Point3d
import Quantity
import Rectangle2d
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import Svg exposing (Svg)
import Svg.Attributes
import TriangularMesh exposing (TriangularMesh)
import Vector2d
import Viewpoint3d


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
            blockEntity |> Scene3d.rotateAround Axis3d.z angle

        -- Defines the shape of the 'screen' that we will be using when
        -- projecting 3D points into 2D
        screenRectangle =
            Rectangle2d.from Point2d.origin (Point2d.pixels width height)

        -- Take all vertices of the logo shape, rotate them the same amount as
        -- the logo itself and then project them into 2D screen space
        vertices2d =
            blockVertices
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
            blockEdges
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



---------- COLORED BLOCK SHAPE ----------


type CenterCoordinates
    = CenterCoordinates


type CornerCoordinates
    = CornerCoordinates


blockHeight : Length
blockHeight =
    Length.meters 0.9


p0 : Point3d Meters CornerCoordinates
p0 =
    Point3d.origin


p1 : Point3d Meters CornerCoordinates
p1 =
    Point3d.meters 1 0 0


p2 : Point3d Meters CornerCoordinates
p2 =
    Point3d.meters 1 1 0


p3 : Point3d Meters CornerCoordinates
p3 =
    Point3d.meters 0 1 0


p4 : Point3d Meters CornerCoordinates
p4 =
    Point3d.xyz Quantity.zero Length.meter blockHeight


p5 : Point3d Meters CornerCoordinates
p5 =
    Point3d.xyz Quantity.zero Quantity.zero blockHeight


p6 : Point3d Meters CornerCoordinates
p6 =
    Point3d.xyz Length.meter Quantity.zero blockHeight


p7 : Point3d Meters CornerCoordinates
p7 =
    Point3d.xyz Length.meter (Length.meters 0.4) blockHeight


p8 : Point3d Meters CornerCoordinates
p8 =
    Point3d.xyz Length.meter Length.meter (Length.meters 0.4)


p9 : Point3d Meters CornerCoordinates
p9 =
    Point3d.xyz (Length.meters 0.4) Length.meter blockHeight


blockCenterFrame : Frame3d Meters CornerCoordinates { defines : CenterCoordinates }
blockCenterFrame =
    Frame3d.atPoint <|
        Point3d.xyz
            (Length.meters 0.5)
            (Length.meters 0.5)
            (Quantity.half blockHeight)


triangleFan : List (Point3d Meters coordinates) -> Mesh.Plain coordinates
triangleFan points =
    let
        faceIndices =
            List.range 1 (List.length points - 2)
                |> List.map
                    (\index ->
                        ( 0, index, index + 1 )
                    )
    in
    Mesh.indexedTriangles <|
        TriangularMesh.indexed (Array.fromList points) faceIndices


blockEntity : Scene3d.Entity CenterCoordinates
blockEntity =
    let
        orange =
            Color.rgb255 240 173 0

        green =
            Color.rgb255 127 209 59

        lightBlue =
            Color.rgb255 96 181 204

        darkBlue =
            Color.rgb255 90 99 86

        leftFace =
            Scene3d.mesh (Material.color orange) <|
                triangleFan [ p1, p2, p8, p7, p6 ]

        rightFace =
            Scene3d.mesh (Material.color lightBlue) <|
                triangleFan [ p2, p3, p4, p9, p8 ]

        topFace =
            Scene3d.mesh (Material.color green) <|
                triangleFan [ p6, p7, p9, p4, p5 ]

        triangleFace =
            Scene3d.mesh (Material.color darkBlue) <|
                triangleFan [ p7, p8, p9 ]

        bottomFace =
            Scene3d.mesh (Material.color green) <|
                triangleFan [ p0, p3, p2, p1 ]

        backLeftFace =
            Scene3d.mesh (Material.color lightBlue) <|
                triangleFan [ p6, p5, p0, p1 ]

        backRightFace =
            Scene3d.mesh (Material.color orange) <|
                triangleFan [ p3, p0, p5, p4 ]
    in
    Scene3d.group
        [ leftFace
        , rightFace
        , topFace
        , triangleFace
        , backLeftFace
        , backRightFace
        , bottomFace
        ]
        |> Scene3d.relativeTo blockCenterFrame


blockVertices : List (Point3d Meters CenterCoordinates)
blockVertices =
    [ p0, p1, p2, p3, p4, p5, p6, p7, p8, p9 ]
        |> List.map (Point3d.relativeTo blockCenterFrame)


blockEdges : List (LineSegment3d Meters CenterCoordinates)
blockEdges =
    [ LineSegment3d.from p0 p1
    , LineSegment3d.from p1 p2
    , LineSegment3d.from p2 p3
    , LineSegment3d.from p3 p0
    , LineSegment3d.from p0 p5
    , LineSegment3d.from p1 p6
    , LineSegment3d.from p2 p8
    , LineSegment3d.from p3 p4
    , LineSegment3d.from p5 p6
    , LineSegment3d.from p6 p7
    , LineSegment3d.from p7 p8
    , LineSegment3d.from p8 p9
    , LineSegment3d.from p7 p9
    , LineSegment3d.from p9 p4
    , LineSegment3d.from p4 p5
    ]
        |> List.map (LineSegment3d.relativeTo blockCenterFrame)
