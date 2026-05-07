module BallsAndBlocks exposing (main)

import Acceleration
import Angle
import Array exposing (Array)
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Camera3d
import Color
import Common.Materials as Materials
import Direction3d
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Illuminance
import Length exposing (Length, inMeters, meters)
import Luminance
import Mass
import Physics exposing (Body, BodyCoordinates, WorldCoordinates)
import Physics.Material as PhysicsMaterial
import Physics.Shape as PhysicsShape
import Pixels exposing (pixels)
import Point3d
import Quantity
import Random
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material
import Sphere3d
import Task
import Vector3d


type alias Model =
    { allFrames : List (List ( Scene3d.Entity BodyCoordinates, Body ))
    , remainingFrames : List (List ( Scene3d.Entity BodyCoordinates, Body ))
    , screenWidth : Float
    , screenHeight : Float
    }


type Msg
    = Tick Float
    | Resize Float Float


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


moonConfig : Physics.Config (Scene3d.Entity BodyCoordinates)
moonConfig =
    let
        base =
            Physics.onEarth
    in
    { base
        | gravity = Vector3d.withLength (Acceleration.metersPerSecondSquared 1.62) Direction3d.negativeZ
        , duration = Duration.seconds (1 / 60)
    }


simulate : Duration -> List ( Scene3d.Entity BodyCoordinates, Body ) -> List (List ( Scene3d.Entity BodyCoordinates, Body ))
simulate remainingDuration currentWorld =
    if remainingDuration |> Quantity.lessThanOrEqualTo Quantity.zero then
        [ currentWorld ]

    else
        let
            futureWorlds =
                simulate (remainingDuration |> Quantity.minus moonConfig.duration)
                    (Physics.simulate moonConfig currentWorld |> Tuple.first)
        in
        currentWorld :: futureWorlds


init : () -> ( Model, Cmd Msg )
init _ =
    let
        frames =
            -- Generate 10 seconds of simulation ahead of time
            simulate (Duration.seconds 10) (floor :: addBoxes [])
                -- Just for fun, reverse the frames to watch the simulation
                -- backwards =)
                |> List.reverse
    in
    ( { allFrames = frames
      , remainingFrames = frames
      , screenWidth = 0
      , screenHeight = 0
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            case model.remainingFrames of
                first :: rest ->
                    -- Advance by one simulation frame
                    ( { model | remainingFrames = rest }, Cmd.none )

                [] ->
                    -- If we've hit the end, restart from the beginning
                    ( { model | remainingFrames = model.allFrames }, Cmd.none )

        Resize width height ->
            ( { model | screenWidth = width, screenHeight = height }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { remainingFrames, screenWidth, screenHeight } =
    case remainingFrames of
        [] ->
            Html.text "Initializing..."

        currentFrame :: rest ->
            let
                camera =
                    Camera3d.lookAt
                        { eyePoint = Point3d.meters 0 20 20
                        , focalPoint = Point3d.meters 0 0 0
                        , upDirection = Direction3d.positiveZ
                        , fov = Camera3d.angle (Angle.degrees 24)
                        , projection = Camera3d.Perspective
                        }

                drawables =
                    List.map getTransformedDrawable currentFrame

                sunlight =
                    Light.directional (Light.castsShadows True)
                        { chromaticity = Light.sunlight
                        , intensity = Illuminance.lux 10000
                        , direction = Direction3d.xyZ (Angle.degrees 45) (Angle.degrees -60)
                        }

                daylight =
                    Light.overhead
                        { upDirection = Direction3d.z
                        , chromaticity = Light.daylight
                        , intensity = Illuminance.lux 15000
                        }
            in
            Html.div
                [ Html.Attributes.style "position" "absolute"
                , Html.Attributes.style "left" "0"
                , Html.Attributes.style "top" "0"
                ]
                [ Scene3d.custom
                    { dimensions = ( Pixels.int (round screenWidth), Pixels.int (round screenHeight) )
                    , antialiasing = Scene3d.multisampling
                    , camera = camera
                    , lights = Scene3d.twoLights sunlight daylight
                    , exposure = Scene3d.maxLuminance (Luminance.nits 10000)
                    , toneMapping = Scene3d.noToneMapping
                    , whiteBalance = Light.daylight
                    , background = Scene3d.transparentBackground
                    , clipDepth = meters 0.1
                    , entities = drawables
                    }
                ]


materials : Array (Material.Uniform coordinates)
materials =
    Array.fromList
        [ Materials.aluminum
        , Materials.whitePlastic
        , Materials.copper
        , Material.nonmetal
            { baseColor = Color.lightBlue
            , roughness = 0.25
            }
        , Materials.gold
        , Materials.whitePlastic
        , Material.nonmetal
            { baseColor = Color.blue
            , roughness = 0.25
            }
        ]


type alias Offsets =
    { x : Float
    , y : Float
    , z : Float
    }


offsetGenerator : Random.Generator Offsets
offsetGenerator =
    let
        magnitude =
            0.01
    in
    Random.map3 Offsets
        (Random.float -magnitude magnitude)
        (Random.float -magnitude magnitude)
        (Random.float -magnitude magnitude)


randomOffsets : Int -> Offsets
randomOffsets index =
    Random.step offsetGenerator (Random.initialSeed index)
        |> Tuple.first


addBoxes : List ( Scene3d.Entity BodyCoordinates, Body ) -> List ( Scene3d.Entity BodyCoordinates, Body )
addBoxes world =
    let
        xySize =
            4

        zSize =
            5

        xyDimensions =
            List.map toFloat (List.range 0 (xySize - 1))

        zDimensions =
            List.map toFloat (List.range 0 (zSize - 1))

        distance =
            1
    in
    List.foldl
        (\x world1 ->
            List.foldl
                (\y world2 ->
                    List.foldl
                        (\z acc ->
                            let
                                index =
                                    round (z * toFloat xySize * toFloat xySize + y * toFloat xySize + x)

                                material =
                                    Array.get (index |> modBy (Array.length materials)) materials
                                        |> Maybe.withDefault Materials.aluminum

                                ( entity, physBody ) =
                                    if (index |> modBy 3) == 0 then
                                        box material

                                    else
                                        sphere (Material.uniform material)

                                offsets =
                                    randomOffsets index
                            in
                            ( entity
                            , Physics.moveTo
                                (Point3d.meters
                                    ((x - (toFloat xySize - 1) / 2) * distance + offsets.x)
                                    ((y - (toFloat xySize - 1) / 2) * distance + offsets.y)
                                    ((z + (2 * toFloat zSize + 1) / 2) * distance + offsets.z)
                                )
                                physBody
                            )
                                :: acc
                        )
                        world2
                        zDimensions
                )
                world1
                xyDimensions
        )
        world
        xyDimensions


floorRadius : Length
floorRadius =
    Length.meters 30


floor : ( Scene3d.Entity BodyCoordinates, Body )
floor =
    let
        shape =
            Sphere3d.atOrigin floorRadius
    in
    ( Scene3d.sphere (Material.uniform Materials.aluminum) shape
    , Physics.static [ ( PhysicsShape.sphere shape, PhysicsMaterial.steel ) ]
        |> Physics.moveTo
            (Point3d.meters
                0
                0
                -(Length.inMeters floorRadius)
            )
    )


boxSize : Length
boxSize =
    Length.meters 0.9


box : Material.Uniform BodyCoordinates -> ( Scene3d.Entity BodyCoordinates, Body )
box material =
    let
        shape =
            Block3d.centeredOn Frame3d.atOrigin
                ( boxSize, boxSize, boxSize )
    in
    ( Scene3d.blockWithShadow material shape
    , Physics.block shape PhysicsMaterial.wood
        |> Physics.scaleMassTo (Mass.kilograms 5)
    )


sphereRadius : Length
sphereRadius =
    Length.meters 0.45


sphere : Material.Textured BodyCoordinates -> ( Scene3d.Entity BodyCoordinates, Body )
sphere material =
    let
        shape =
            Sphere3d.atOrigin sphereRadius
    in
    ( Scene3d.sphereWithShadow material shape
    , Physics.sphere shape PhysicsMaterial.wood
        |> Physics.scaleMassTo (Mass.kilograms 2.5)
    )


getTransformedDrawable : ( Scene3d.Entity BodyCoordinates, Body ) -> Scene3d.Entity WorldCoordinates
getTransformedDrawable ( entity, body ) =
    Scene3d.placeIn (Physics.frame body) entity
