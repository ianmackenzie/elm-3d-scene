module DynamicBackground exposing (main)

{-| This example shows how you might animate the background color of a scene
(with a bit of extra logic to add some lighting matching the background color,
so that it appears that the object being rendered is being illuminated by its
surroundings).
-}

import Angle exposing (Angle)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d
import Duration exposing (Duration)
import Html exposing (Html)
import Illuminance
import Length exposing (Meters)
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Sphere3d
import Viewpoint3d exposing (Viewpoint3d)


{-| Compute background color based on total elapsed time
-}
backgroundColor : Duration -> Color
backgroundColor elapsedTime =
    let
        original =
            Color.toHsla Color.blue

        -- Perform a full 'hue rotation' every 24 seconds
        incrementedHue =
            original.hue + Duration.inSeconds elapsedTime / 24

        -- Adjust hue to be in the range 0-1
        adjustedHue =
            incrementedHue - toFloat (floor incrementedHue)
    in
    Color.fromHsla { original | hue = adjustedHue }


type alias Model =
    { elapsedTime : Duration
    }


type Msg
    = Tick Duration


{-| Initialize to zero elapsed time
-}
init : () -> ( Model, Cmd Msg )
init () =
    ( { elapsedTime = Quantity.zero }, Cmd.none )


{-| When we receive a Tick message, increment the total elapsed time
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update (Tick duration) model =
    ( { model | elapsedTime = model.elapsedTime |> Quantity.plus duration }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Convert Float value from onAnimationFrameDelta (a number of milliseconds)
    -- into a typed Duration value, then wrap in a Tick message
    Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)


view : Model -> Html Msg
view model =
    let
        -- Get background color based on current time
        currentBackgroundColor =
            backgroundColor model.elapsedTime

        -- Create a fixed directional light
        sunlight =
            Light.directional (Light.castsShadows False)
                { chromaticity = Light.daylight
                , intensity = Illuminance.lux 10000
                , direction = Direction3d.yz (Angle.degrees -120)
                }

        -- Create some soft overhead lighting the same color as the background,
        -- so that the sphere appears to be lit by its surroundings
        overheadLighting =
            Light.overhead
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.color currentBackgroundColor
                , intensity = Illuminance.lux 10000
                }

        sphereMaterial =
            Material.nonmetal
                { baseColor = Color.blue
                , roughness = 0.4
                }
    in
    Scene3d.custom
        { camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.centimeters 20 10 20
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.centimeters 0.5
        , dimensions = ( Pixels.int 400, Pixels.int 300 )
        , antialiasing = Scene3d.multisampling
        , lights = Scene3d.twoLights sunlight overheadLighting
        , exposure = Scene3d.exposureValue 12
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Light.daylight
        , background = Scene3d.backgroundColor currentBackgroundColor
        , entities =
            [ Scene3d.sphere sphereMaterial <|
                Sphere3d.withRadius (Length.centimeters 5) Point3d.origin
            ]
        }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
