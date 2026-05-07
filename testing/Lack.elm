module Lack exposing (main)

import Angle
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Density
import Direction3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode exposing (Decoder)
import Length exposing (Meters, meters, millimeters)
import Luminance
import Physics exposing (Body, BodyCoordinates, WorldCoordinates, onEarth)
import Physics.Constraint exposing (Constraint)
import Physics.Material
import Physics.Shape
import Pixels exposing (Pixels)
import Plane3d
import Point2d
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Sphere3d
import Task


type Id
    = Mouse
    | Floor
    | Table


type alias Model =
    { bodies : List ( Id, Body )
    , contacts : Physics.Contacts Id
    , dimensions : ( Quantity Int Pixels, Quantity Int Pixels )
    , dragTarget : Maybe ( Point3d Meters BodyCoordinates, Point3d Meters WorldCoordinates )
    }


type Msg
    = AnimationFrame
    | Resize Int Int
    | MouseDown (Axis3d Meters WorldCoordinates)
    | MouseMove (Axis3d Meters WorldCoordinates)
    | MouseUp


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { bodies = tableOnFloor
      , contacts = Physics.emptyContacts
      , dimensions = ( Pixels.int 0, Pixels.int 0 )
      , dragTarget = Nothing
      }
    , Task.perform
        (\{ viewport } -> Resize (round viewport.width) (round viewport.height))
        Browser.Dom.getViewport
    )


tableBlocks : List (Block3d Meters BodyCoordinates)
tableBlocks =
    [ Block3d.from
        (Point3d.millimeters 222 222 0)
        (Point3d.millimeters 272 272 400)
    , Block3d.from
        (Point3d.millimeters -272 222 0)
        (Point3d.millimeters -222 272 400)
    , Block3d.from
        (Point3d.millimeters -272 -272 0)
        (Point3d.millimeters -222 -222 400)
    , Block3d.from
        (Point3d.millimeters 222 -272 0)
        (Point3d.millimeters 272 -222 400)
    , Block3d.from
        (Point3d.millimeters -275 -275 400)
        (Point3d.millimeters 275 275 450)
    ]


honeycombBoard : Physics.Material.Material Physics.Material.Dense
honeycombBoard =
    Physics.Material.dense
        { density = Density.kilogramsPerCubicMeter 187.2
        , friction = 0.4
        , bounciness = 0.3
        }


tableOnFloor : List ( Id, Body )
tableOnFloor =
    [ ( Table
      , Physics.dynamic <|
            List.map
                (\block -> ( Physics.Shape.block block, honeycombBoard ))
                tableBlocks
      )
    , ( Floor, Physics.plane Plane3d.xy Physics.Material.wood )
    ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        AnimationFrame ->
            case model.dragTarget of
                Just ( pointOnTable, dragPoint ) ->
                    let
                        ( simulated, newContacts ) =
                            Physics.simulate
                                { onEarth
                                    | constrain = lockMouseTo pointOnTable
                                    , contacts = model.contacts
                                }
                                (( Mouse, Physics.static [] |> Physics.moveTo dragPoint )
                                    :: model.bodies
                                )
                    in
                    { model | bodies = List.drop 1 simulated, contacts = newContacts }

                Nothing ->
                    let
                        ( simulated, newContacts ) =
                            Physics.simulate
                                { onEarth | contacts = model.contacts }
                                model.bodies
                    in
                    { model | bodies = simulated, contacts = newContacts }

        MouseDown mouseRay ->
            case Physics.raycast mouseRay model.bodies of
                Just ( Table, body, { point } ) ->
                    let
                        pointOnTable =
                            Point3d.relativeTo (Physics.frame body) point
                    in
                    { model | dragTarget = Just ( pointOnTable, point ) }

                _ ->
                    model

        MouseMove mouseRay ->
            case model.dragTarget of
                Just ( pointOnTable, dragPoint ) ->
                    let
                        plane =
                            Plane3d.through dragPoint (Camera3d.viewDirection camera)
                    in
                    { model
                        | dragTarget =
                            Just
                                ( pointOnTable
                                , Axis3d.intersectionWithPlane plane mouseRay
                                    |> Maybe.withDefault dragPoint
                                )
                    }

                Nothing ->
                    model

        MouseUp ->
            { model | dragTarget = Nothing }

        Resize width height ->
            { model | dimensions = ( Pixels.int width, Pixels.int height ) }


lockMouseTo : Point3d Meters BodyCoordinates -> Id -> Maybe (Id -> List Constraint)
lockMouseTo pointOnTable mouseId =
    if mouseId == Mouse then
        Just
            (\tableId ->
                if tableId == Table then
                    [ Physics.Constraint.pointToPoint Point3d.origin pointOnTable ]

                else
                    []
            )

    else
        Nothing


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.lookAt
        { eyePoint = Point3d.meters 3 4 2
        , focalPoint = Point3d.meters -0.5 -0.5 0
        , upDirection = Direction3d.positiveZ
        , projection = Camera3d.Perspective
        , fov = Camera3d.angle (Angle.degrees 24)
        }


view : Model -> Html Msg
view { bodies, dimensions, dragTarget } =
    let
        sunlight =
            Light.directional (Light.castsShadows True)
                { chromaticity = Light.daylight
                , intensity = Illuminance.lux 10000
                , direction = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
                }

        daylight =
            Light.overhead
                { upDirection = Direction3d.z
                , chromaticity = Light.daylight
                , intensity = Illuminance.lux 15000
                }

        mouseEntity =
            case dragTarget of
                Just ( _, dragPoint ) ->
                    Scene3d.sphere (Material.matte Color.white)
                        (Sphere3d.atPoint dragPoint (millimeters 20))

                Nothing ->
                    Scene3d.nothing
    in
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.on "mousedown" (decodeMouseRay dimensions MouseDown)
        , Html.Events.on "mousemove" (decodeMouseRay dimensions MouseMove)
        , Html.Events.onMouseUp MouseUp
        ]
        [ Scene3d.custom
            { dimensions = dimensions
            , antialiasing = Scene3d.multisampling
            , camera = camera
            , lights = Scene3d.twoLights sunlight daylight
            , exposure = Scene3d.maxLuminance (Luminance.nits 10000)
            , toneMapping = Scene3d.noToneMapping
            , whiteBalance = Light.daylight
            , clipDepth = meters 0.1
            , background = Scene3d.transparentBackground
            , entities = mouseEntity :: List.map bodyEntity bodies
            }
        ]


bodyEntity : ( Id, Body ) -> Scene3d.Entity WorldCoordinates
bodyEntity ( id, body ) =
    Scene3d.placeIn (Physics.frame body) <|
        case id of
            Mouse ->
                Scene3d.nothing

            Table ->
                Scene3d.group <|
                    List.map
                        (Scene3d.blockWithShadow
                            (Material.nonmetal
                                { baseColor = Color.white
                                , roughness = 0.25
                                }
                            )
                        )
                        tableBlocks

            Floor ->
                Scene3d.quad (Material.matte Color.darkCharcoal)
                    (Point3d.meters -15 -15 0)
                    (Point3d.meters -15 15 0)
                    (Point3d.meters 15 15 0)
                    (Point3d.meters 15 -15 0)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize Resize
        , Browser.Events.onAnimationFrame (\_ -> AnimationFrame)
        ]


decodeMouseRay :
    ( Quantity Int Pixels, Quantity Int Pixels )
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decoder msg
decodeMouseRay ( width, height ) rayToMsg =
    Json.Decode.map2
        (\x y ->
            rayToMsg <|
                Camera3d.ray camera
                    (Rectangle2d.with
                        { x1 = Quantity.zero
                        , y1 = Quantity.toFloatQuantity height
                        , x2 = Quantity.toFloatQuantity width
                        , y2 = Quantity.zero
                        }
                    )
                    (Point2d.pixels x y)
        )
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)
