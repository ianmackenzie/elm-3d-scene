module ColorChannel exposing (main)

import Angle
import Axis3d
import Browser
import Camera3d
import Direction3d
import Html exposing (Html)
import Illuminance
import Length
import Luminance
import Palette.Tango as Tango
import Pixels
import Point2d
import Point3d
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Exposure as Exposure
import Scene3d.Material as Material
import Scene3d.Material.Channel as Channel
import SketchPlane3d
import Task
import Viewpoint3d
import WebGL.Texture


type Model
    = Loading
    | Error
    | Loaded (Material.Texture Float)


type Msg
    = GotTexture (Result WebGL.Texture.Error (Material.Texture Float))


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotTexture result) model =
    case model of
        Loading ->
            case result of
                Ok texture ->
                    ( Loaded texture, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )

        Error ->
            ( model, Cmd.none )

        Loaded _ ->
            ( model, Cmd.none )


view : Model -> Html msg
view model =
    case model of
        Loading ->
            Html.text "Loading"

        Error ->
            Html.text "Error loading texture"

        Loaded roughnessTexture ->
            let
                viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint =
                            Point3d.on
                                (SketchPlane3d.xz
                                    |> SketchPlane3d.rotateAround Axis3d.z (Angle.degrees 45)
                                )
                                (Point2d.meters 5 5)
                        , focalPoint = Point3d.origin
                        , upDirection = Direction3d.z
                        }

                camera =
                    Camera3d.perspective
                        { viewpoint = viewpoint
                        , clipDepth = Length.centimeters 10
                        , verticalFieldOfView = Angle.degrees 30
                        }

                sunlight =
                    Scene3d.directionalLight Scene3d.doesNotCastShadows
                        { chromaticity = Chromaticity.sunlight
                        , direction = Direction3d.xyZ (Angle.degrees 45) (Angle.degrees -45)
                        , intensity = Illuminance.lux 50000
                        }

                environmentalLighting =
                    Scene3d.softLighting
                        { upDirection = Direction3d.z
                        , above = ( Luminance.nits 10000, Chromaticity.d65 )
                        , below = ( Luminance.nits 5000, Chromaticity.d65 )
                        }

                quad =
                    Scene3d.quad Scene3d.doesNotCastShadows
                        (Material.texturedNonmetal
                            { baseColor = Material.constant Tango.skyBlue2
                            , roughness = roughnessTexture
                            }
                        )
                        (Point3d.meters 1 -1 0)
                        (Point3d.meters 1 1 0)
                        (Point3d.meters -1 1 0)
                        (Point3d.meters -1 -1 0)
            in
            Scene3d.toHtml []
                { camera = camera
                , background = Scene3d.transparentBackground
                , dimensions = ( Pixels.pixels 800, Pixels.pixels 800 )
                , whiteBalance = Scene3d.defaultWhiteBalance
                , exposure = Exposure.sunny16
                , lights = Scene3d.oneLight sunlight
                , environmentalLighting = environmentalLighting
                }
                [ quad ]


textureUrl : String
textureUrl =
    "https://ianmackenzie.github.io/elm-3d-scene/examples/rgb_stripes.png"


loadOptions : WebGL.Texture.Options
loadOptions =
    let
        defaultOptions =
            WebGL.Texture.defaultOptions
    in
    { defaultOptions
        | magnify = WebGL.Texture.nearest
        , minify = WebGL.Texture.nearest
    }


main : Program () Model Msg
main =
    Browser.element
        { init =
            always
                ( Loading
                , Task.attempt GotTexture
                    (Material.loadChannelWith loadOptions Channel.luminance textureUrl)
                )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
