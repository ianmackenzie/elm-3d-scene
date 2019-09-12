module Gearbox exposing (..)

import AnimationFrame
import Dict exposing (Dict)
import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Materials
import Mouse
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Drawable as Drawable exposing (Drawable)
import OpenSolid.Scene.Light as Light exposing (Light)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)
import OpenSolid.Viewpoint as Viewpoint exposing (Viewpoint)
import Shaft
import SingleTouch
import Time exposing (Time)
import Touch exposing (Touch, TouchEvent(..))


type Msg
    = StartRotatingAt Point2d
    | PointerMovedTo Point2d
    | StopRotating
    | NumTeeth1Changed Int
    | NumTeeth2Changed Int
    | Tick Time


type alias GearDrawable =
    { numTeeth : Int
    , cache : Dict Int (() -> Drawable)
    , rotationAngle : Float
    }


type alias Model =
    { dragPoint : Maybe Point2d
    , placementFrame : Frame3d
    }


init : ( Model, Cmd Msg )
init =
    ( { dragPoint = Nothing, placementFrame = Frame3d.xyz }, Cmd.none )



-- Rendering


screenWidth : Int
screenWidth =
    1024


screenHeight : Int
screenHeight =
    768


tolerance : Float
tolerance =
    0.001


camera : Camera
camera =
    Camera.perspective
        { viewpoint =
            Viewpoint.lookAt
                { eyePoint = Point3d.fromCoordinates ( 6, 6, 8 )
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.z
                }
        , verticalFieldOfView = degrees 30
        , screenWidth = toFloat screenWidth
        , screenHeight = toFloat screenHeight
        , nearClipDistance = 0.1
        , farClipDistance = 100
        }


createShaftDrawable : Point3d -> Point3d -> Float -> Drawable
createShaftDrawable startPoint endPoint diameter =
    case
        Shaft.fromEndpoints
            { startPoint = startPoint
            , endPoint = endPoint
            , diameter = diameter
            , flatLength = 0.5
            , flatThickness = 0.5 * diameter
            }
    of
        Just shaft ->
            Drawable.body tolerance Materials.aluminum (Shaft.body shaft)

        Nothing ->
            Drawable.empty


staticDrawables : Drawable
staticDrawables =
    Drawable.group
        [ createShaftDrawable
            (Point3d.fromCoordinates ( 1.5, 1, 0 ))
            (Point3d.fromCoordinates ( -1.5, 1, 0 ))
            0.5
        , createShaftDrawable
            (Point3d.fromCoordinates ( -1.5, -1, 0 ))
            (Point3d.fromCoordinates ( 1.5, -1, 0 ))
            0.5
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


mousePositionToPoint : Mouse.Position -> Point2d
mousePositionToPoint mousePosition =
    Point2d.fromCoordinates ( toFloat mousePosition.x, toFloat mousePosition.y )


touchToPoint : Touch -> Point2d
touchToPoint touch =
    Point2d.fromCoordinates ( touch.clientX, touch.clientY )


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


view : Model -> Html Msg
view model =
    let
        scene =
            staticDrawables
                |> Drawable.placeIn model.placementFrame
    in
    Html.div []
        [ Html.div dragAttributes
            [ Scene.render lights camera scene ]
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
                sketchPlane =
                    Viewpoint.viewPlane (Camera.viewpoint camera)

                axialDirection =
                    Direction3d.on sketchPlane <|
                        Direction2d.perpendicularTo direction2d

                rotationAxis =
                    Axis3d.with
                        { originPoint = Frame3d.originPoint frame
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

        Tick _ ->
            ( model, Cmd.none )

        NumTeeth1Changed numTeeth1 ->
            ( model, Cmd.none )

        NumTeeth2Changed numTeeth2 ->
            ( model, Cmd.none )


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
