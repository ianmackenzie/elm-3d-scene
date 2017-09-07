module Sprocket exposing (..)

import AnimationFrame
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Kintail.InputWidget as InputWidget
import Materials
import Math.Vector3 exposing (Vec3, vec3)
import Mouse
import OpenSolid.Arc2d as Arc2d
import OpenSolid.Arc3d as Arc3d
import OpenSolid.Axis2d as Axis2d
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Body3d as Body3d exposing (Body3d)
import OpenSolid.BoundaryType as BoundaryType
import OpenSolid.Circle2d as Circle2d
import OpenSolid.Curve2d as Curve2d exposing (Curve2d)
import OpenSolid.Curve3d as Curve3d
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Rectangle2d as Rectangle2d exposing (Rectangle2d)
import OpenSolid.Region2d as Region2d exposing (Region2d)
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Light as Light exposing (Light)
import OpenSolid.Scene.Node as Node exposing (Node)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Surface3d as Surface3d
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)
import OpenSolid.WebGL.Camera as Camera exposing (Camera)
import Shapes
import SingleTouch
import Task
import Time exposing (Time)
import Touch exposing (Touch, TouchEvent(..))
import Window


-- Types


type Msg
    = StartRotatingAt Point2d
    | PointerMovedTo Point2d
    | StopRotating
    | NumTeethChanged Int
    | Tick Time


type alias Model =
    { dragPoint : Maybe Point2d
    , placementFrame : Frame3d
    , numTeeth : Int
    , sprocketNode : () -> Node
    , rotationAngle : Float
    }



-- Rendering


screenWidth : Int
screenWidth =
    1024


screenHeight : Int
screenHeight =
    768


camera : Camera
camera =
    Camera.perspective
        { frame =
            Camera.lookAt
                { eyePoint = Point3d.withCoordinates ( 6, 0, 3 )
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.z
                }
        , verticalFieldOfView = degrees 30
        , screenWidth = toFloat screenWidth
        , screenHeight = toFloat screenHeight
        , nearClipDistance = 0.1
        , farClipDistance = 100
        }



-- Interactivity


createSprocketNode : Int -> Node
createSprocketNode numTeeth =
    let
        sprocketBody =
            sprocketWith
                { bore = 0.5
                , pitch = 0.25
                , thickness = 0.125
                , numTeeth = numTeeth
                , toothHeight = 0.175
                , rollerDiameter = 0.125
                }
    in
    Geometry.body tolerance sprocketBody
        |> Geometry.shaded Materials.aluminum
        |> Node.translateBy (Vector3d.withComponents ( 0, 0, -0.25 ))


mousePositionToPoint : Mouse.Position -> Point2d
mousePositionToPoint mousePosition =
    Point2d.withCoordinates ( toFloat mousePosition.x, toFloat mousePosition.y )


touchToPoint : Touch -> Point2d
touchToPoint touch =
    Point2d.withCoordinates ( touch.clientX, touch.clientY )


init : ( Model, Cmd Msg )
init =
    let
        initialNumTeeth =
            32

        model =
            { dragPoint = Nothing
            , placementFrame = Frame3d.xyz
            , numTeeth = initialNumTeeth
            , sprocketNode = always (createSprocketNode initialNumTeeth)
            , rotationAngle = 0
            }
    in
    ( model, Cmd.none )


dragAttributes : List (Attribute Msg)
dragAttributes =
    let
        onMouseDown pointToMsg =
            Events.on "mousedown" Mouse.position
                |> Attributes.map (mousePositionToPoint >> pointToMsg)

        onTouch touchEvent pointToMsg =
            SingleTouch.onSingleTouch touchEvent Touch.preventAndStop .touch
                |> Attributes.map (touchToPoint >> pointToMsg)
    in
    [ onMouseDown StartRotatingAt
    , onTouch TouchStart StartRotatingAt
    , onTouch TouchMove PointerMovedTo
    , onTouch TouchEnd (always StopRotating)
    , onTouch TouchCancel (always StopRotating)
    ]


roundedRectangle : Region2d
roundedRectangle =
    let
        rectangle =
            Rectangle2d.centeredOn Frame2d.xy ( 6, 4 )
    in
    Region2d.roundedRectangle rectangle 1


roundedBlock : Body3d
roundedBlock =
    Body3d.extrusion roundedRectangle SketchPlane3d.xy 2


bearingRaces : { bore : Float, outerDiameter : Float, externalRaceDiameter : Float, internalRaceDiameter : Float, width : Float, ballDiameter : Float } -> Body3d
bearingRaces { bore, outerDiameter, externalRaceDiameter, internalRaceDiameter, width, ballDiameter } =
    let
        halfWidth =
            width / 2

        innerRadius =
            bore / 2

        outerRadius =
            outerDiameter / 2

        innerRaceOuterRadius =
            internalRaceDiameter / 2

        outerRaceInnerRadius =
            externalRaceDiameter / 2

        ballRadius =
            ballDiameter / 2

        gap =
            outerRaceInnerRadius - innerRaceOuterRadius

        trackHalfWidth =
            sqrt (ballRadius * ballRadius - gap * gap / 4)

        trackSweptAngle =
            2 * acos (gap / (2 * ballRadius))

        p1 =
            Point2d.withCoordinates ( -halfWidth, innerRadius )

        p2 =
            Point2d.withCoordinates ( halfWidth, innerRadius )

        p3 =
            Point2d.withCoordinates ( halfWidth, innerRaceOuterRadius )

        p4 =
            Point2d.withCoordinates ( trackHalfWidth, innerRaceOuterRadius )

        p5 =
            Point2d.withCoordinates ( -trackHalfWidth, innerRaceOuterRadius )

        p6 =
            Point2d.withCoordinates ( -halfWidth, innerRaceOuterRadius )

        p7 =
            Point2d.withCoordinates ( -halfWidth, outerRaceInnerRadius )

        p8 =
            Point2d.withCoordinates ( -trackHalfWidth, outerRaceInnerRadius )

        p9 =
            Point2d.withCoordinates ( trackHalfWidth, outerRaceInnerRadius )

        p10 =
            Point2d.withCoordinates ( halfWidth, outerRaceInnerRadius )

        p11 =
            Point2d.withCoordinates ( halfWidth, outerRadius )

        p12 =
            Point2d.withCoordinates ( -halfWidth, outerRadius )

        centerPoint =
            Point2d.withCoordinates
                ( 0
                , (innerRaceOuterRadius + outerRaceInnerRadius) / 2
                )

        lineSegment firstPoint secondPoint =
            LineSegment2d.from firstPoint secondPoint
                |> LineSegment3d.on SketchPlane3d.zx
                |> Curve3d.lineSegment

        surface edge =
            Surface3d.revolution edge Axis3d.z (2 * pi)

        outerSurface =
            surface (lineSegment p11 p12)

        innerSurface =
            surface (lineSegment p1 p2)

        outerRaceFrontSurface =
            surface (lineSegment p10 p11)

        outerRaceBackSurface =
            surface (lineSegment p12 p7)

        outerRaceInnerFrontSurface =
            surface (lineSegment p9 p10)

        outerRaceInnerBackSurface =
            surface (lineSegment p7 p8)

        innerRaceFrontSurface =
            surface (lineSegment p2 p3)

        innerRaceBackSurface =
            surface (lineSegment p6 p1)

        innerRaceOuterFrontSurface =
            surface (lineSegment p3 p4)

        innerRaceOuterBackSurface =
            surface (lineSegment p5 p6)

        outerTrackArc =
            Arc2d.with
                { centerPoint = centerPoint
                , startPoint = p8
                , sweptAngle = -trackSweptAngle
                }

        innerTrackArc =
            Arc2d.with
                { centerPoint = centerPoint
                , startPoint = p4
                , sweptAngle = -trackSweptAngle
                }

        outerTrackSurface =
            surface (Curve3d.arc (Arc3d.on SketchPlane3d.zx outerTrackArc))

        innerTrackSurface =
            surface (Curve3d.arc (Arc3d.on SketchPlane3d.zx innerTrackArc))
    in
    Body3d.fromSurfaces
        [ outerSurface
        , innerSurface
        , outerRaceFrontSurface
        , outerRaceBackSurface
        , innerRaceFrontSurface
        , innerRaceBackSurface
        , outerRaceInnerFrontSurface
        , outerRaceInnerBackSurface
        , innerRaceOuterFrontSurface
        , innerRaceOuterBackSurface
        , outerTrackSurface
        , innerTrackSurface
        ]


sprocketWith : { bore : Float, pitch : Float, numTeeth : Int, thickness : Float, rollerDiameter : Float, toothHeight : Float } -> Body3d
sprocketWith { bore, pitch, numTeeth, thickness, rollerDiameter, toothHeight } =
    let
        theta =
            pi / toFloat numTeeth

        r =
            pitch / (2 * sin theta)

        c1 =
            Point2d.withPolarCoordinates ( r, pi / 2 - theta )

        c2 =
            Point2d.withPolarCoordinates ( r, pi / 2 + theta )

        rollerRadius =
            rollerDiameter / 2

        p1 =
            Point2d.withPolarCoordinates ( r - rollerRadius, pi / 2 - theta )

        p0 =
            p1 |> Point2d.projectOnto Axis2d.y

        p2 =
            c1 |> Point2d.translateBy (Vector2d.withComponents ( -rollerRadius, 0 ))

        arc1 =
            Curve2d.arc <|
                Arc2d.with
                    { centerPoint = c1
                    , startPoint = p1
                    , sweptAngle = theta - pi / 2
                    }

        arc2 =
            Curve2d.arc <|
                Arc2d.with
                    { centerPoint = c2
                    , startPoint = p2
                    , sweptAngle =
                        asin ((toothHeight - rollerRadius) / (pitch - rollerRadius))
                    }

        p3 =
            Curve2d.endPoint arc2

        p4 =
            p3 |> Point2d.projectOnto Axis2d.y

        p5 =
            Point2d.withCoordinates ( 0, bore / 2 )

        p6 =
            Point2d.withPolarCoordinates ( bore / 2, pi / 2 - theta )

        arc3 =
            Curve2d.arc <|
                Arc2d.with
                    { centerPoint = Point2d.origin
                    , startPoint = p5
                    , sweptAngle = -theta
                    }

        line1 =
            Curve2d.lineSegment <|
                LineSegment2d.from p6 p1

        line2 =
            Curve2d.lineSegment <|
                LineSegment2d.from p3 p4

        fan : Curve2d -> Region2d
        fan =
            Region2d.fanWith
                { start = BoundaryType.interior
                , end = BoundaryType.interior
                , curve = BoundaryType.exterior
                }
                p0

        halfWedge =
            Region2d.fuse
                [ fan line1
                , fan arc1
                , fan arc2
                , fan line2
                , fan arc3
                ]

        firstWedge =
            Region2d.fuse
                [ halfWedge, Region2d.mirrorAcross Axis2d.y halfWedge ]

        toWedge index =
            let
                angle =
                    toFloat index * theta * 2
            in
            Region2d.rotateAround Point2d.origin angle firstWedge

        wedges =
            List.range 0 (numTeeth - 1)
                |> List.map toWedge

        region =
            Region2d.fuse wedges
    in
    Body3d.extrusion region SketchPlane3d.xy thickness
        |> Body3d.translateBy (Vector3d.withComponents ( 0, 0, -thickness / 2 ))


hub : Body3d
hub =
    let
        lineSegment =
            LineSegment2d.withEndpoints
                ( Point2d.withCoordinates ( 0.25, 0 )
                , Point2d.withCoordinates ( 0.625, 0 )
                )

        region =
            Region2d.revolution
                (Curve2d.lineSegment lineSegment)
                Point2d.origin
                (turns 1)
    in
    Body3d.extrusion region SketchPlane3d.xy 0.5
        |> Body3d.translateBy (Vector3d.withComponents ( 0, 0, -0.25 ))


shaft : Body3d
shaft =
    let
        curve =
            Curve2d.arc <|
                Circle2d.toArc
                    (Circle2d.with
                        { centerPoint = Point2d.origin
                        , radius = 0.25
                        }
                    )

        region =
            Region2d.fan Point2d.origin curve
    in
    Body3d.extrusion region SketchPlane3d.xy 3
        |> Body3d.translateBy (Vector3d.withComponents ( 0, 0, -1.5 ))


tolerance : Float
tolerance =
    0.001


staticNodes : Node
staticNodes =
    Node.group
        [ Geometry.body tolerance hub |> Geometry.shaded Materials.aluminum
        , Geometry.body tolerance shaft |> Geometry.shaded Materials.gold
        ]


bearingNode : Node
bearingNode =
    let
        externalRaceDiameter =
            1.0625

        internalRaceDiameter =
            0.6875

        ballDiameter =
            0.25

        races =
            bearingRaces
                { bore = 0.5
                , outerDiameter = 1.25
                , externalRaceDiameter = externalRaceDiameter
                , internalRaceDiameter = internalRaceDiameter
                , width = 0.375
                , ballDiameter = ballDiameter
                }

        racesNode =
            Geometry.body tolerance races
                |> Geometry.shaded Materials.chromium

        numBalls =
            8

        ballAngle =
            2 * pi / toFloat numBalls

        ballPathRadius =
            (internalRaceDiameter + externalRaceDiameter) / 4

        firstBallPoint =
            Point3d.withCoordinates ( ballPathRadius, 0, 0 )

        firstBall =
            Shapes.sphere firstBallPoint (ballDiameter / 2)
                |> Geometry.shaded Materials.whitePlastic

        toBall index =
            let
                angle =
                    toFloat index * ballAngle
            in
            Node.rotateAround Axis3d.z angle firstBall

        ballNodes =
            List.map toBall (List.range 0 (numBalls - 1))

        singleBearing =
            Node.group (racesNode :: ballNodes)
    in
    Node.group
        [ singleBearing |> Node.translateBy (Vector3d.withComponents ( 0, 0, 1.125 ))
        , singleBearing |> Node.translateBy (Vector3d.withComponents ( 0, 0, -1.125 ))
        ]


lights : List Light
lights =
    let
        d =
            3

        color =
            vec3 10 10 10
    in
    [ Light.point (Point3d.withCoordinates ( d, d, d )) color
    , Light.point (Point3d.withCoordinates ( d, -d, d )) color
    , Light.point (Point3d.withCoordinates ( -d, d, d )) color
    , Light.point (Point3d.withCoordinates ( -d, -d, d )) color
    ]


view : Model -> Html Msg
view { placementFrame, numTeeth, sprocketNode, rotationAngle } =
    let
        sliderAttributes =
            [ Attributes.style [ ( "width", toString screenWidth ++ "px" ) ] ]

        sliderConfig =
            { min = 20
            , max = 64
            , step = 1
            }

        shaftNode =
            Node.group [ sprocketNode (), staticNodes ]

        scene =
            Node.group
                [ shaftNode |> Node.rotateAround Axis3d.z rotationAngle
                , bearingNode |> Node.rotateAround Axis3d.z (rotationAngle / 2)
                ]
    in
    Html.div []
        [ Html.div dragAttributes
            [ Scene.render lights camera (Node.placeIn placementFrame scene) ]
        , InputWidget.slider sliderAttributes sliderConfig (toFloat numTeeth)
            |> Html.map (round >> NumTeethChanged)
        ]


rotate : Frame3d -> Float -> Float -> Frame3d
rotate frame dx dy =
    let
        dragVector =
            Vector2d.withComponents ( dx, dy )
    in
    case Vector2d.direction dragVector of
        Just direction2d ->
            let
                axialDirection =
                    Direction3d.on SketchPlane3d.yz <|
                        Direction2d.perpendicularTo direction2d

                rotationAxis =
                    Axis3d.with
                        { originPoint = Point3d.origin
                        , direction = axialDirection
                        }

                rotationAngle =
                    degrees 1 * Vector2d.length dragVector
            in
            frame |> Frame3d.rotateAround rotationAxis rotationAngle

        Nothing ->
            frame


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        StartRotatingAt startPoint ->
            ( { model | dragPoint = Just startPoint }, Cmd.none )

        StopRotating ->
            ( { model | dragPoint = Nothing }, Cmd.none )

        PointerMovedTo newPoint ->
            case model.dragPoint of
                Just lastPoint ->
                    let
                        ( dx, dy ) =
                            Vector2d.from lastPoint newPoint
                                |> Vector2d.components

                        rotatedFrame =
                            rotate model.placementFrame dx -dy

                        updatedModel =
                            { model
                                | placementFrame = rotatedFrame
                                , dragPoint = Just newPoint
                            }
                    in
                    ( updatedModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NumTeethChanged numTeeth ->
            ( { model
                | numTeeth = numTeeth
                , sprocketNode = always (createSprocketNode numTeeth)
              }
            , Cmd.none
            )

        Tick time ->
            ( { model
                | rotationAngle = model.rotationAngle + Time.inSeconds time * degrees 45
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.dragPoint of
        Just _ ->
            Sub.batch
                [ Mouse.moves (mousePositionToPoint >> PointerMovedTo)
                , Mouse.ups (always StopRotating)
                , AnimationFrame.diffs Tick
                ]

        Nothing ->
            AnimationFrame.diffs Tick


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
