module Lack exposing (main)

import Acceleration
import Angle
import Axis3d exposing (Axis3d)
import Block3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Duration exposing (seconds)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode exposing (Decoder)
import Length exposing (Meters, meters, millimeters)
import Luminance
import Mass exposing (kilograms)
import Physics.Body as Body exposing (Body)
import Physics.Constraint
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Shape
import Physics.World as World exposing (RaycastResult, World)
import Pixels exposing (Pixels, pixels)
import Plane3d
import Point2d
import Point3d
import Quantity exposing (Quantity)
import Rectangle2d
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material
import Sphere3d
import Task
import Vector3d
import Viewpoint3d


type Id
    = Mouse
    | Floor
    | Table


type alias Data =
    { entity : Scene3d.Entity BodyCoordinates
    , id : Id
    }


type alias Model =
    { world : World Data
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , maybeRaycastResult : Maybe (RaycastResult Data)
    }


type Msg
    = AnimationFrame
    | Resize (Quantity Float Pixels) (Quantity Float Pixels)
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
    ( { world = initialWorld
      , width = pixels 0
      , height = pixels 0
      , maybeRaycastResult = Nothing
      }
    , Task.perform
        (\{ viewport } ->
            Resize (pixels viewport.width) (pixels viewport.height)
        )
        Browser.Dom.getViewport
    )


initialWorld : World Data
initialWorld =
    World.empty
        |> World.withGravity
            (Acceleration.metersPerSecondSquared 9.80665)
            Direction3d.negativeZ
        |> World.add table
        |> World.add floor


floor : Body Data
floor =
    let
        shape =
            Block3d.centeredOn Frame3d.atOrigin
                ( meters 25, meters 25, millimeters 10 )
    in
    Body.plane
        { id = Floor
        , entity =
            shape
                |> Scene3d.block (Material.matte Color.darkCharcoal)
                |> Scene3d.translateBy (Vector3d.millimeters 0 0 -5)
        }


table : Body Data
table =
    let
        blocks =
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

        shapes =
            blocks
                |> List.map Physics.Shape.block

        entities =
            blocks
                |> List.map
                    (Scene3d.blockWithShadow
                        (Material.nonmetal
                            { baseColor = Color.white
                            , roughness = 0.25
                            }
                        )
                    )
    in
    Body.compound shapes
        { id = Table
        , entity = Scene3d.group entities
        }
        |> Body.withBehavior (Body.dynamic (kilograms 3.58))


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 3 4 2
                , focalPoint = Point3d.meters -0.5 -0.5 0
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 24
        }


view : Model -> Html Msg
view { world, width, height } =
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

        drawables =
            List.map
                (\body ->
                    Scene3d.placeIn
                        (Body.frame body)
                        (Body.data body).entity
                )
                (World.bodies world)
    in
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        , Html.Events.on "mousedown" (decodeMouseRay camera width height MouseDown)
        , Html.Events.on "mousemove" (decodeMouseRay camera width height MouseMove)
        , Html.Events.onMouseUp MouseUp
        ]
        [ Scene3d.custom
            { dimensions = ( width, height )
            , antialiasing = Scene3d.multisampling
            , camera = camera
            , lights = Scene3d.twoLights sunlight daylight
            , exposure = Scene3d.maxLuminance (Luminance.nits 10000)
            , toneMapping = Scene3d.noToneMapping
            , whiteBalance = Light.daylight
            , clipDepth = meters 0.1
            , background = Scene3d.transparentBackground
            , entities = drawables
            }
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onResize
            (\width height ->
                Resize (pixels (toFloat width)) (pixels (toFloat height))
            )
        , Browser.Events.onAnimationFrame (\_ -> AnimationFrame)
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        AnimationFrame ->
            { model | world = World.simulate (seconds (1 / 60)) model.world }

        Resize width height ->
            { model | width = width, height = height }

        MouseDown mouseRay ->
            let
                maybeRaycastResult =
                    model.world
                        |> World.keepIf
                            (\body -> (Body.data body).id == Table)
                        |> World.raycast mouseRay
            in
            case maybeRaycastResult of
                Just raycastResult ->
                    let
                        worldPoint =
                            Point3d.placeIn
                                (Body.frame raycastResult.body)
                                raycastResult.point

                        selectedId =
                            (Body.data raycastResult.body).id
                    in
                    { model
                        | maybeRaycastResult = Just raycastResult
                        , world =
                            model.world
                                |> World.add (Body.moveTo worldPoint mouse)
                                |> World.constrain
                                    (\b1 b2 ->
                                        if
                                            ((Body.data b1).id == Mouse)
                                                && ((Body.data b2).id == selectedId)
                                        then
                                            [ Physics.Constraint.pointToPoint
                                                Point3d.origin
                                                raycastResult.point
                                            ]

                                        else
                                            []
                                    )
                    }

                Nothing ->
                    model

        MouseMove mouseRay ->
            case model.maybeRaycastResult of
                Just raycastResult ->
                    let
                        worldPoint =
                            Point3d.placeIn
                                (Body.frame raycastResult.body)
                                raycastResult.point

                        plane =
                            Plane3d.through
                                worldPoint
                                (Viewpoint3d.viewDirection (Camera3d.viewpoint camera))
                    in
                    { model
                        | world =
                            World.update
                                (\body ->
                                    if (Body.data body).id == Mouse then
                                        case Axis3d.intersectionWithPlane plane mouseRay of
                                            Just intersection ->
                                                Body.moveTo intersection body

                                            Nothing ->
                                                body

                                    else
                                        body
                                )
                                model.world
                    }

                Nothing ->
                    model

        MouseUp ->
            { model
                | maybeRaycastResult = Nothing
                , world =
                    World.keepIf
                        (\body -> (Body.data body).id /= Mouse)
                        model.world
            }


mouse : Body Data
mouse =
    Body.compound []
        { id = Mouse
        , entity =
            Scene3d.sphere (Material.matte Color.white)
                (Sphere3d.atOrigin (millimeters 20))
        }


decodeMouseRay :
    Camera3d Meters WorldCoordinates
    -> Quantity Float Pixels
    -> Quantity Float Pixels
    -> (Axis3d Meters WorldCoordinates -> msg)
    -> Decoder msg
decodeMouseRay camera3d width height rayToMsg =
    Json.Decode.map2
        (\x y ->
            rayToMsg
                (Camera3d.ray
                    camera3d
                    (Rectangle2d.with
                        { x1 = pixels 0
                        , y1 = height
                        , x2 = width
                        , y2 = pixels 0
                        }
                    )
                    (Point2d.pixels x y)
                )
        )
        (Json.Decode.field "pageX" Json.Decode.float)
        (Json.Decode.field "pageY" Json.Decode.float)
