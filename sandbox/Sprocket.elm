module Sprocket exposing (..)

import Angle exposing (Angle)
import Arc2d
import Arc3d
import Axis2d
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Circle2d
import Color
import Direction2d exposing (Direction2d)
import Direction3d exposing (Direction3d)
import Frame2d exposing (Frame2d)
import Frame3d exposing (Frame3d)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Length exposing (Length, Meters)
import LineSegment2d
import LineSegment3d
import Materials
import Pixels exposing (Pixels)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d exposing (Rectangle2d)
import Scene3d
import Scene3d.Drawable as Drawable exposing (Drawable, Material)
import Scene3d.Light as Light exposing (Light)
import Scene3d.Shape as Shape
import SketchPlane3d exposing (SketchPlane3d)
import Vector2d exposing (Vector2d)
import Vector3d exposing (Vector3d)
import Viewpoint3d



-- Types


type Msg
    = MouseDown
    | MouseMove Float Float
    | MouseUp
    | NumTeethChanged Int
    | HubDiameterChanged Length
    | HubWidthChanged Length
    | MaterialChanged Material
    | Tick Duration


type WorldCoordinates
    = WorldCoordinates


type ModelCoordinates
    = ModelCoordinates


type alias Model =
    { orbiting : Bool
    , placementFrame : Frame3d Meters WorldCoordinates { defines : ModelCoordinates }
    , numTeeth : Int
    , hubDiameter : Length
    , hubWidth : Length
    , materialType : MaterialType
    , sprocketDrawable : () -> Drawable ModelCoordinates
    , rotationAngle : Angle
    }


type MaterialType
    = Aluminum
    | Copper
    | WhitePlastic
    | BluePlastic


materialDescription : MaterialType -> String
materialDescription materialType =
    case materialType of
        Aluminum ->
            "Aluminum"

        Copper ->
            "Copper"

        WhitePlastic ->
            "White plastic"

        BluePlastic ->
            "Blue plastic"



-- Rendering


screenWidth : Quantity Int Pixels
screenWidth =
    Pixels.pixels 800


screenHeight : Quantity Int Pixels
screenHeight =
    Pixels.pixels 600


camera : Camera
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 5 0 3
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.z
                }
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = Length.meters 0.1
        }



-- Interactivity


createSprocketDrawable : { numTeeth : Int, hubWidth : Length, hubDiameter : Length, materialType : MaterialType } -> Drawable ModelCoordinates
createSprocketDrawable { numTeeth, hubWidth, hubDiameter, materialType } =
    let
        sprocketMaterial =
            case materialType of
                Aluminum ->
                    Materials.aluminum

                Copper ->
                    Materials.copper

                WhitePlastic ->
                    Materials.whitePlastic

                BluePlastic ->
                    { color = Color.blue, roughness = 0.25, metallic = False }

        offset =
            Vector3d.meters 0 0 -0.25

        sprocketDrawable =
            sprocketMesh
                { bore = Length.inches 0.5
                , pitch = Length.inches 0.25
                , thickness = Length.inches 0.125
                , numTeeth = numTeeth
                , toothHeight = Length.inches 0.175
                , rollerDiameter = Length.inches 0.125
                }
                |> Drawable.physical sprocketMaterial
                |> Drawable.translateBy offset

        hubBody =
            let
                lineSegment =
                    LineSegment2d.fromEndpoints
                        (Point2d.meters 0.25 0)
                        (Point2d.meters (Quantity.half hubDiameter) 0)

                region =
                    Region2d.revolution
                        (Curve2d.lineSegment lineSegment)
                        Point2d.origin
                        (turns 1)
            in
            Body3d.extrusion region SketchPlane3d.xy hubWidth
                |> Body3d.translateBy (Vector3d.fromComponents ( 0, 0, 0.0625 ))
                |> Body3d.translateBy offset
    in
    Drawable.group
        [ Drawable.body tolerance drawableMaterial sprocketBody
        , Drawable.body tolerance drawableMaterial hubBody
        ]


mousePositionToPoint : Mouse.Position -> Point2d
mousePositionToPoint mousePosition =
    Point2d.fromCoordinates ( toFloat mousePosition.x, toFloat mousePosition.y )


touchToPoint : Touch -> Point2d
touchToPoint touch =
    Point2d.fromCoordinates ( touch.clientX, touch.clientY )


init : ( Model, Cmd Msg )
init =
    let
        initialNumTeeth =
            20

        initialHubDiameter =
            1.25

        initialHubWidth =
            0.5

        initialMaterial =
            Aluminum

        model =
            { dragPoint = Nothing
            , placementFrame =
                Frame3d.xyz
                    |> Frame3d.rotateAroundOwn Frame3d.zAxis (degrees -60)
                    |> Frame3d.rotateAroundOwn Frame3d.yAxis (degrees -90)
            , numTeeth = initialNumTeeth
            , hubDiameter = initialHubDiameter
            , hubWidth = initialHubWidth
            , sprocketDrawable =
                always <|
                    createSprocketDrawable
                        { numTeeth = initialNumTeeth
                        , hubDiameter = initialHubDiameter
                        , hubWidth = initialHubWidth
                        , material = initialMaterial
                        }
            , rotationAngle = 0
            , material = initialMaterial
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
            Point2d.fromCoordinates ( -halfWidth, innerRadius )

        p2 =
            Point2d.fromCoordinates ( halfWidth, innerRadius )

        p3 =
            Point2d.fromCoordinates ( halfWidth, innerRaceOuterRadius )

        p4 =
            Point2d.fromCoordinates ( trackHalfWidth, innerRaceOuterRadius )

        p5 =
            Point2d.fromCoordinates ( -trackHalfWidth, innerRaceOuterRadius )

        p6 =
            Point2d.fromCoordinates ( -halfWidth, innerRaceOuterRadius )

        p7 =
            Point2d.fromCoordinates ( -halfWidth, outerRaceInnerRadius )

        p8 =
            Point2d.fromCoordinates ( -trackHalfWidth, outerRaceInnerRadius )

        p9 =
            Point2d.fromCoordinates ( trackHalfWidth, outerRaceInnerRadius )

        p10 =
            Point2d.fromCoordinates ( halfWidth, outerRaceInnerRadius )

        p11 =
            Point2d.fromCoordinates ( halfWidth, outerRadius )

        p12 =
            Point2d.fromCoordinates ( -halfWidth, outerRadius )

        centerPoint =
            Point2d.fromCoordinates
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
            Point2d.fromPolarCoordinates ( r, pi / 2 - theta )

        c2 =
            Point2d.fromPolarCoordinates ( r, pi / 2 + theta )

        rollerRadius =
            rollerDiameter / 2

        p1 =
            Point2d.fromPolarCoordinates ( r - rollerRadius, pi / 2 - theta )

        p0 =
            p1 |> Point2d.projectOnto Axis2d.y

        p2 =
            c1 |> Point2d.translateBy (Vector2d.fromComponents ( -rollerRadius, 0 ))

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
            Point2d.fromCoordinates ( 0, bore / 2 )

        p6 =
            Point2d.fromPolarCoordinates ( bore / 2, pi / 2 - theta )

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
        |> Body3d.translateBy (Vector3d.fromComponents ( 0, 0, -thickness / 2 ))


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
        |> Body3d.translateBy (Vector3d.fromComponents ( 0, 0, -1.5 ))


tolerance : Float
tolerance =
    0.001


staticDrawables : Drawable
staticDrawables =
    Drawable.group
        [ Drawable.body tolerance Materials.gold shaft
        ]


bearingDrawable : Drawable
bearingDrawable =
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

        racesDrawable =
            Drawable.body tolerance Materials.chromium races

        numBalls =
            8

        ballAngle =
            2 * pi / toFloat numBalls

        ballPathRadius =
            (internalRaceDiameter + externalRaceDiameter) / 4

        firstBallPoint =
            Point3d.fromCoordinates ( ballPathRadius, 0, 0 )

        firstBall =
            Shapes.sphere Materials.whitePlastic
                firstBallPoint
                (ballDiameter / 2)

        toBall index =
            let
                angle =
                    toFloat index * ballAngle
            in
            Drawable.rotateAround Axis3d.z angle firstBall

        ballDrawables =
            List.map toBall (List.range 0 (numBalls - 1))

        singleBearing =
            Drawable.group (racesDrawable :: ballDrawables)
    in
    Drawable.group
        [ singleBearing |> Drawable.translateBy (Vector3d.fromComponents ( 0, 0, 1.125 ))
        , singleBearing |> Drawable.translateBy (Vector3d.fromComponents ( 0, 0, -1.125 ))
        ]


lights : List Light
lights =
    let
        d =
            3

        color =
            ( 10, 10, 10 )
    in
    [ Light.point (Point3d.fromCoordinates ( d, d, d )) color
    , Light.point (Point3d.fromCoordinates ( d, -d, d )) color
    , Light.point (Point3d.fromCoordinates ( -d, d, d )) color
    , Light.point (Point3d.fromCoordinates ( -d, -d, d )) color
    ]


view : Model -> Html Msg
view { placementFrame, numTeeth, hubDiameter, hubWidth, sprocketDrawable, rotationAngle, material } =
    let
        sliderAttributes =
            [ Attributes.style [ ( "width", toString screenWidth ++ "px" ) ] ]

        numTeethConfig =
            { min = 10
            , max = 64
            , step = 1
            }

        hubDiameterConfig =
            { min = 0.625
            , max = 3
            , step = 0.0625
            }

        hubWidthConfig =
            { min = 0.125
            , max = 1
            , step = 0.0625
            }

        shaftDrawable =
            Drawable.group [ sprocketDrawable (), staticDrawables ]

        scene =
            Drawable.group
                [ shaftDrawable |> Drawable.rotateAround Axis3d.z rotationAngle
                , bearingDrawable |> Drawable.rotateAround Axis3d.z (rotationAngle / 2)
                ]

        heading text =
            Html.h3 [] [ Html.text text ]

        materials =
            [ Aluminum, Copper, WhitePlastic, BluePlastic ]

        divStyle =
            Attributes.style [ ( "width", toString screenWidth ++ "px" ) ]
    in
    Html.div []
        [ Html.div (divStyle :: dragAttributes)
            [ Scene.render lights camera (Drawable.placeIn placementFrame scene) ]
        , heading ("Number of teeth: " ++ toString numTeeth)
        , InputWidget.slider sliderAttributes numTeethConfig (toFloat numTeeth)
            |> Html.map (round >> NumTeethChanged)
        , heading ("Hub diameter: " ++ toString hubDiameter)
        , InputWidget.slider sliderAttributes hubDiameterConfig hubDiameter
            |> Html.map HubDiameterChanged
        , heading ("Hub width: " ++ toString hubWidth)
        , InputWidget.slider sliderAttributes hubWidthConfig hubWidth
            |> Html.map HubWidthChanged
        , heading "Material:"
        , InputWidget.comboBox [] materialDescription materials material
            |> Html.map MaterialChanged
        ]


rotate : Frame3d -> Float -> Float -> Frame3d
rotate frame dx dy =
    let
        dragVector =
            Vector2d.fromComponents ( dx, dy )
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
                , sprocketDrawable =
                    always <|
                        createSprocketDrawable
                            { numTeeth = numTeeth
                            , hubDiameter = model.hubDiameter
                            , hubWidth = model.hubWidth
                            , material = model.material
                            }
              }
            , Cmd.none
            )

        HubWidthChanged hubWidth ->
            ( { model
                | hubWidth = hubWidth
                , sprocketDrawable =
                    always <|
                        createSprocketDrawable
                            { numTeeth = model.numTeeth
                            , hubDiameter = model.hubDiameter
                            , hubWidth = hubWidth
                            , material = model.material
                            }
              }
            , Cmd.none
            )

        HubDiameterChanged hubDiameter ->
            ( { model
                | hubDiameter = hubDiameter
                , sprocketDrawable =
                    always <|
                        createSprocketDrawable
                            { numTeeth = model.numTeeth
                            , hubDiameter = hubDiameter
                            , hubWidth = model.hubWidth
                            , material = model.material
                            }
              }
            , Cmd.none
            )

        MaterialChanged material ->
            ( { model
                | material = material
                , sprocketDrawable =
                    always <|
                        createSprocketDrawable
                            { numTeeth = model.numTeeth
                            , hubDiameter = model.hubDiameter
                            , hubWidth = model.hubWidth
                            , material = material
                            }
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
