module MultipleShadows exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d
import Block3d exposing (Block3d)
import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onAnimationFrameDelta, onMouseDown, onMouseMove, onMouseUp, onResize, onVisibilityChange)
import Camera3d
import Common.Materials as Materials
import Dict exposing (Dict)
import Direction3d
import Duration exposing (Duration)
import Frame3d
import Html exposing (Html)
import Html.Attributes as Attributes exposing (style)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import LuminousFlux exposing (LuminousFlux)
import Palette.Tango as Tango
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d exposing (Chromaticity, Entity, Light)
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import SketchPlane3d
import SolidAngle
import Sphere3d
import Task
import Temperature
import Vector3d
import Viewpoint3d


type alias Model =
    { width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , elapsedTime : Duration
    , activeDrag : Maybe ( Float, Float )
    , azimuth : Angle
    , elevation : Angle
    }


type WorldCoordinates
    = WorldCoordinates


type Msg
    = Resize (Quantity Float Pixels) (Quantity Float Pixels)
    | Tick Duration
    | MouseDown ( Float, Float )
    | MouseMove ( Float, Float )
    | MouseUp
    | VisibilityChange Browser.Events.Visibility


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( { width = Quantity.zero
                  , height = Quantity.zero
                  , elapsedTime = Quantity.zero
                  , activeDrag = Nothing
                  , azimuth = Angle.degrees -90
                  , elevation = Angle.degrees 30
                  }
                , Task.perform
                    (\{ viewport } ->
                        Resize
                            (Pixels.pixels viewport.width)
                            (Pixels.pixels viewport.height)
                    )
                    getViewport
                )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Resize width height ->
            ( { model | width = width, height = height }, Cmd.none )

        Tick elapsed ->
            ( { model | elapsedTime = model.elapsedTime |> Quantity.plus elapsed }, Cmd.none )

        MouseDown position ->
            ( { model | activeDrag = Just position }, Cmd.none )

        MouseUp ->
            ( { model | activeDrag = Nothing }, Cmd.none )

        VisibilityChange Browser.Events.Visible ->
            ( model, Cmd.none )

        VisibilityChange Browser.Events.Hidden ->
            ( { model | activeDrag = Nothing }, Cmd.none )

        MouseMove ( x, y ) ->
            case model.activeDrag of
                Just ( previousX, previousY ) ->
                    let
                        deltaX =
                            x - previousX

                        deltaY =
                            y - previousY

                        newAzimuth =
                            model.azimuth
                                |> Quantity.minus
                                    (Angle.degrees 0.5 |> Quantity.multiplyBy deltaX)

                        newElevation =
                            model.elevation
                                |> Quantity.plus
                                    (Angle.degrees 0.5 |> Quantity.multiplyBy deltaY)
                                |> Quantity.clamp (Angle.degrees 0) (Angle.degrees 90)
                    in
                    ( { model
                        | activeDrag = Just ( x, y )
                        , azimuth = newAzimuth
                        , elevation = newElevation
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )


mouseEventDecoder : Decoder ( Float, Float )
mouseEventDecoder =
    Decode.map2 Tuple.pair
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\w h -> Resize (Pixels.pixels (toFloat w)) (Pixels.pixels (toFloat h)))
        , onAnimationFrameDelta (Duration.milliseconds >> Tick)
        , onVisibilityChange VisibilityChange
        , if model.activeDrag == Nothing then
            Sub.none

          else
            onMouseMove (Decode.map MouseMove mouseEventDecoder)
        , onMouseDown (Decode.map MouseDown mouseEventDecoder)
        , onMouseUp (Decode.succeed MouseUp)
        ]



-- VIEW


pointLight :
    { position : Point3d Meters WorldCoordinates, chromaticity : Chromaticity, intensity : LuminousFlux }
    -> ( Light WorldCoordinates Bool, Entity WorldCoordinates )
pointLight properties =
    let
        sphereRadius =
            Length.centimeters 5

        surfaceArea =
            Sphere3d.atOrigin sphereRadius |> Sphere3d.surfaceArea

        sphereLuminance =
            properties.intensity
                |> Quantity.per (SolidAngle.spats 1)
                |> Quantity.per surfaceArea
    in
    ( Scene3d.pointLight (Scene3d.castsShadows True) properties
    , Scene3d.sphere (Scene3d.castsShadows False)
        (Material.emissive properties.chromaticity sphereLuminance)
        (Sphere3d.atPoint properties.position sphereRadius)
    )


view : Model -> Html Msg
view { width, height, elapsedTime, azimuth, elevation } =
    let
        ( firstLight, firstLightBall ) =
            pointLight
                { position = Point3d.meters -2 -2 3
                , chromaticity = Scene3d.incandescentLighting
                , intensity = LuminousFlux.lumens 50000
                }

        ( secondLight, secondLightBall ) =
            pointLight
                { position = Point3d.meters -1 2 5
                , chromaticity = Scene3d.fluorescentLighting
                , intensity = LuminousFlux.lumens 50000
                }

        thirdLight =
            Scene3d.directionalLight (Scene3d.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -45)
                , chromaticity = Scene3d.colorTemperature (Temperature.kelvins 2000)
                , intensity = Illuminance.lux 30
                }

        viewpoint =
            Viewpoint3d.orbit
                { focalPoint = Point3d.meters 4 2 -2
                , groundPlane = SketchPlane3d.xy
                , azimuth = azimuth
                , elevation = elevation
                , distance = Length.meters 20
                }

        camera =
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = Angle.degrees 45
                }

        plane =
            Scene3d.quad (Scene3d.castsShadows False)
                (Material.matte Tango.aluminum2)
                (Point3d.meters -9 -9 0)
                (Point3d.meters 9 -9 0)
                (Point3d.meters 9 9 0)
                (Point3d.meters -9 9 0)
                |> Scene3d.translateBy (Vector3d.meters 3 1 -4)

        firstBoxRotationAxis =
            Axis3d.through Point3d.origin <|
                Direction3d.xyZ (Angle.degrees 45)
                    (Angle.atan2 (Quantity.float 1) (Quantity.float (sqrt 2)))

        firstBoxRotationSpeed =
            Angle.degrees 30 |> Quantity.per Duration.second

        boxDimensions =
            ( Length.meters 2
            , Length.meters 2
            , Length.meters 2
            )

        firstBox =
            Scene3d.block (Scene3d.castsShadows True)
                Materials.aluminum
                (Block3d.centeredOn Frame3d.atOrigin boxDimensions)
                |> Scene3d.rotateAround firstBoxRotationAxis
                    (elapsedTime |> Quantity.at firstBoxRotationSpeed)

        secondBoxCenter =
            Point3d.meters 6 1 -3

        secondBoxRotationAxis =
            Axis3d.through secondBoxCenter Direction3d.positiveZ

        secondBoxRotationSpeed =
            Angle.degrees 30 |> Quantity.per Duration.second

        secondBox =
            Scene3d.block (Scene3d.castsShadows True)
                (Material.pbr { baseColor = Tango.skyBlue2, roughness = 0.25, metallic = 0 })
                (Block3d.centeredOn (Frame3d.atPoint secondBoxCenter) boxDimensions)
                |> Scene3d.rotateAround secondBoxRotationAxis
                    (elapsedTime |> Quantity.at secondBoxRotationSpeed)

        softLighting =
            Scene3d.softLighting
                { upDirection = Direction3d.positiveZ
                , chromaticity = Scene3d.fluorescentLighting
                , intensityAbove = Illuminance.lux 30
                , intensityBelow = Illuminance.lux 5
                }
    in
    Scene3d.toHtml
        { lights = Scene3d.fourLights firstLight secondLight thirdLight softLighting
        , camera = camera
        , clipDepth = Length.meters 1
        , exposure = Scene3d.exposureValue 6
        , toneMapping = Scene3d.hableFilmicToneMapping
        , whiteBalance = Scene3d.fluorescentLighting
        , antialiasing = Scene3d.multisampling
        , dimensions = ( width, height )
        , background = Scene3d.backgroundColor Tango.skyBlue1
        , entities = [ plane, firstBox, secondBox, firstLightBall, secondLightBall ]
        }
