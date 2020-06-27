module MultipleShadows exposing (main)

{-| This example illustrates a few different techniques:

  - Casting shadows from multiple lights and objects
  - Animating multiple objects
  - Rendering a scene to the full size of the current browser window

-}

import Angle exposing (Angle)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LuminousFlux exposing (LuminousFlux)
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Entity)
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material
import SolidAngle
import Sphere3d
import Task
import Temperature
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


type alias Model =
    { width : Quantity Int Pixels -- Width of the browser window
    , height : Quantity Int Pixels -- Height of the browser window
    , elapsedTime : Duration
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    }


type Msg
    = Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | Tick Duration
    | MouseDown
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | MouseUp
    | VisibilityChange Browser.Events.Visibility


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , elapsedTime = Quantity.zero
      , orbiting = False
      , azimuth = Angle.degrees -90
      , elevation = Angle.degrees 30
      }
    , Task.perform
        (\{ viewport } ->
            Resize
                (Pixels.int (round viewport.width))
                (Pixels.int (round viewport.height))
        )
        Browser.Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        -- Browser window size changed
        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        Tick elapsed ->
            ( { model | elapsedTime = model.elapsedTime |> Quantity.plus elapsed }, Cmd.none )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        -- User switched back to this tab
        VisibilityChange Browser.Events.Visible ->
            ( model, Cmd.none )

        -- User switched to a different tab, minimized the browser etc.; cancel
        -- any orbit in progress
        VisibilityChange Browser.Events.Hidden ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Angle.degrees 0.5 |> Quantity.per Pixels.pixel

                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees 5) (Angle.degrees 85)
                in
                ( { model
                    | orbiting = True
                    , azimuth = newAzimuth
                    , elevation = newElevation
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ -- Listen for resize events so we can render the full size of the
          -- browser window
          Browser.Events.onResize (\width height -> Resize (Pixels.int width) (Pixels.int height))

        -- Subscribe to animation frames to animate the cubes
        , Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)

        -- Listen for visibility change events so we can stop orbiting if the
        -- user switches to a different tab etc.
        , Browser.Events.onVisibilityChange VisibilityChange

        -- Listen for orbit-related mouse events
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        ]


{-| Create both a Light and an Entity (a bright glowing sphere) representing a
particular point light
-}
pointLight :
    { position : Point3d Meters WorldCoordinates
    , chromaticity : Chromaticity
    , intensity : LuminousFlux
    }
    -> ( Light WorldCoordinates Bool, Entity WorldCoordinates )
pointLight properties =
    let
        -- Create a sphere to represent a light bulb
        sphere =
            Sphere3d.atPoint properties.position (Length.millimeters 5)

        -- Calculate the luminance of the sphere surface by dividing the given
        -- total luminous flux of the light by the surface area of the sphere
        -- and by the solid angle of a hemisphere (assuming that each point on
        -- the surface of the bulb emits light equally in all directions)...I
        -- am not 100% sure this is exactly correct =)
        sphereLuminance =
            properties.intensity
                |> Quantity.per (SolidAngle.spats 0.5)
                |> Quantity.per (Sphere3d.surfaceArea sphere)

        -- Create an emissive (glowing) material for the sphere
        sphereMaterial =
            Material.emissive properties.chromaticity sphereLuminance
    in
    ( Light.point (Light.castsShadows True) properties
    , Scene3d.sphere sphereMaterial sphere
    )


view : Model -> Html Msg
view model =
    let
        -- Incandescent light bulb
        ( firstLight, firstLightBall ) =
            pointLight
                { position = Point3d.centimeters -50 -30 70
                , chromaticity = Light.incandescent
                , intensity = LuminousFlux.lumens 500
                }

        -- Fluorescent light bulb
        ( secondLight, secondLightBall ) =
            pointLight
                { position = Point3d.centimeters -40 10 90
                , chromaticity = Light.fluorescent
                , intensity = LuminousFlux.lumens 500
                }

        -- Rough approximation of unlight near sunset
        thirdLight =
            Light.directional (Light.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -45)
                , chromaticity = Light.colorTemperature (Temperature.kelvins 2000)
                , intensity = Illuminance.lux 30
                }

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 30
                , intensityBelow = Illuminance.lux 5
                }

        -- Create a quad to act as a 'floor'
        plane =
            Scene3d.quad (Material.matte Color.gray)
                (Point3d.centimeters -90 -90 0)
                (Point3d.centimeters 90 -90 0)
                (Point3d.centimeters 90 90 0)
                (Point3d.centimeters -90 90 0)

        -- Define overall box dimensions for the two boxes
        boxDimensions =
            ( Length.centimeters 20
            , Length.centimeters 20
            , Length.centimeters 20
            )

        -- Create the floating aluminum-colored box
        firstBoxCenter =
            Point3d.centimeters -30 -10 40

        firstBoxRotationAxis =
            Axis3d.through firstBoxCenter <|
                Direction3d.xyZ (Angle.degrees 45)
                    (Angle.atan2 (Quantity.float 1) (Quantity.float (sqrt 2)))

        firstBoxRotationSpeed =
            Angle.degrees 30 |> Quantity.per Duration.second

        firstBoxAngle =
            model.elapsedTime |> Quantity.at firstBoxRotationSpeed

        firstBoxMaterial =
            Material.metal
                { baseColor = Color.rgb255 233 235 236
                , roughness = 0.6
                }

        firstBox =
            Scene3d.blockWithShadow firstBoxMaterial
                (Block3d.centeredOn (Frame3d.atPoint firstBoxCenter) boxDimensions)
                |> Scene3d.rotateAround firstBoxRotationAxis firstBoxAngle

        -- Create the blue box sitting on the 'floor'
        secondBoxCenter =
            Point3d.centimeters 30 0 10

        secondBoxRotationAxis =
            Axis3d.through secondBoxCenter Direction3d.positiveZ

        secondBoxRotationSpeed =
            Angle.degrees 30 |> Quantity.per Duration.second

        secondBoxAngle =
            model.elapsedTime |> Quantity.at secondBoxRotationSpeed

        bluePlastic =
            Material.nonmetal { baseColor = Color.blue, roughness = 0.25 }

        secondBox =
            Scene3d.blockWithShadow bluePlastic
                (Block3d.centeredOn (Frame3d.atPoint secondBoxCenter) boxDimensions)
                |> Scene3d.rotateAround secondBoxRotationAxis secondBoxAngle

        -- Define a camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.centimeters 0 0 20
                        , azimuth = model.azimuth
                        , elevation = model.elevation
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Scene3d.custom
        { lights = Scene3d.fourLights firstLight secondLight thirdLight softLighting
        , camera = camera
        , clipDepth = Length.centimeters 10
        , exposure = Scene3d.exposureValue 6
        , toneMapping = Scene3d.hableFilmicToneMapping -- See ExposureAndToneMapping.elm for details
        , whiteBalance = Light.fluorescent
        , antialiasing = Scene3d.multisampling
        , dimensions = ( model.width, model.height )
        , background = Scene3d.backgroundColor Color.lightBlue
        , entities = [ plane, firstBox, secondBox, firstLightBall, secondLightBall ]
        }
