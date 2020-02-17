module DynamicBackground exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Color.Transparent
import Direction3d
import Duration exposing (Duration)
import Html exposing (Html)
import Illuminance
import Length exposing (Meters)
import Luminance
import LuminousFlux
import Palette.Tango as Tango
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Exposure as Exposure
import Scene3d.Material as Material exposing (Material)
import Sphere3d
import Temperature
import Viewpoint3d exposing (Viewpoint3d)


type World
    = World


viewpoint : Viewpoint3d Meters World
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.centimeters 20 10 20
        , upDirection = Direction3d.positiveZ
        }


sunlight =
    Scene3d.directionalLight Scene3d.doesNotCastShadows
        { chromaticity = Chromaticity.d65
        , intensity = Illuminance.lux 10000
        , direction = Direction3d.yz (Angle.degrees -120)
        }


camera : Camera3d Meters World
camera =
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = Length.centimeters 0.5
        }


material : Material.Textured coordinates
material =
    Material.nonmetal
        { baseColor = Tango.skyBlue2
        , roughness = 0.4
        }


backgroundColor : Duration -> Color
backgroundColor elapsedTime =
    Color.rotateHue (45 * Duration.inSeconds elapsedTime) Tango.skyBlue2


main : Program () Duration Duration
main =
    Browser.element
        { init = always ( Quantity.zero, Cmd.none )
        , update =
            \delta elapsed -> ( elapsed |> Quantity.plus delta, Cmd.none )
        , subscriptions = always (Browser.Events.onAnimationFrameDelta Duration.milliseconds)
        , view =
            \elapsedTime ->
                Scene3d.toHtml []
                    { camera = camera
                    , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                    , environmentalLighting =
                        Scene3d.softLighting
                            { upDirection = Direction3d.positiveZ
                            , above = ( Luminance.nits 5000, Chromaticity.d65 )
                            , below = ( Quantity.zero, Chromaticity.d65 )
                            }
                    , directLighting =
                        Scene3d.oneLightSource sunlight
                    , exposure =
                        Exposure.fromMaxLuminance (Luminance.nits 5000)
                    , whiteBalance = Scene3d.defaultWhiteBalance
                    , background = Scene3d.backgroundColor (backgroundColor elapsedTime)
                    }
                    [ Scene3d.sphere Scene3d.doesNotCastShadows material <|
                        Sphere3d.withRadius (Length.centimeters 5) Point3d.origin
                    ]
        }
