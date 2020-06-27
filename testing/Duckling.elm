module Duckling exposing (main)

import Angle
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color exposing (Color)
import Cylinder3d
import Direction3d
import Duration exposing (Duration)
import Html exposing (Html)
import Http
import Illuminance
import Length
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material exposing (Texture)
import Scene3d.Mesh as Mesh
import Scene3d.Mesh.Decode as Decode
import Sphere3d
import Task
import Viewpoint3d
import WebGL.Texture


type WorldCoordinates
    = WorldCoordinates


type Model
    = Loading (Maybe (Mesh.Textured WorldCoordinates)) (Maybe (Texture Color))
    | Error String
    | Loaded
        { mesh : Mesh.Textured WorldCoordinates
        , texture : Texture Color
        , shadow : Mesh.Shadow WorldCoordinates
        , elapsedDuration : Duration
        }


type Msg
    = MeshResponse (Result Http.Error (Mesh.Textured WorldCoordinates))
    | TextureResponse (Result WebGL.Texture.Error (Texture Color))
    | Tick Duration


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading Nothing Nothing
    , Cmd.batch
        [ Task.attempt TextureResponse <|
            Material.loadWith Material.trilinearFiltering
                "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling.png"
        , Http.get
            { url = "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling.json"
            , expect = Http.expectJson MeshResponse Decode.texturedFaces
            }
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        MeshResponse (Err _) ->
            ( Error "Error loading mesh", Cmd.none )

        TextureResponse (Err _) ->
            ( Error "Error loading texture", Cmd.none )

        MeshResponse (Ok mesh) ->
            case model of
                Loading _ Nothing ->
                    ( Loading (Just mesh) Nothing, Cmd.none )

                Loading _ (Just texture) ->
                    ( Loaded
                        { mesh = mesh
                        , texture = texture
                        , shadow = Mesh.shadow mesh
                        , elapsedDuration = Quantity.zero
                        }
                    , Cmd.none
                    )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        TextureResponse (Ok texture) ->
            case model of
                Loading Nothing _ ->
                    ( Loading Nothing (Just texture), Cmd.none )

                Loading (Just mesh) _ ->
                    ( Loaded
                        { mesh = mesh
                        , texture = texture
                        , shadow = Mesh.shadow mesh
                        , elapsedDuration = Quantity.zero
                        }
                    , Cmd.none
                    )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        Tick duration ->
            case model of
                Loading _ _ ->
                    ( model, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded loadedModel ->
                    ( Loaded
                        { loadedModel
                            | elapsedDuration =
                                loadedModel.elapsedDuration |> Quantity.plus duration
                        }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading _ _ ->
            Sub.none

        Error _ ->
            Sub.none

        Loaded _ ->
            Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)


view : Model -> Html Msg
view model =
    case model of
        Loading _ _ ->
            Html.text "Loading..."

        Error text ->
            Html.text text

        Loaded { mesh, texture, shadow, elapsedDuration } ->
            let
                ducklingMaterial =
                    Material.texturedNonmetal
                        { baseColor = texture
                        , roughness = Material.constant 0.25
                        }

                rotationOrigin =
                    Point3d.meters 0 0 -0.35

                pitchAxis =
                    Axis3d.through rotationOrigin Direction3d.y

                rollAxis =
                    Axis3d.through rotationOrigin Direction3d.x

                pitchPeriod =
                    Duration.seconds 4

                rollPeriod =
                    Duration.seconds 2

                pitchAngle =
                    Angle.degrees (2 + 2 * sin (2 * pi * Quantity.ratio elapsedDuration pitchPeriod))

                rollAngle =
                    Angle.degrees (5 * sin (2 * pi * Quantity.ratio elapsedDuration rollPeriod))

                duckling =
                    Scene3d.meshWithShadow ducklingMaterial mesh shadow
                        |> Scene3d.rotateAround Axis3d.x (Angle.degrees 90)
                        |> Scene3d.translateIn Direction3d.negativeZ (Length.meters 0.67)
                        |> Scene3d.rotateAround rollAxis rollAngle
                        |> Scene3d.rotateAround pitchAxis pitchAngle

                water =
                    Scene3d.quad (Material.matte Color.lightBlue)
                        (Point3d.meters 1.5 1.5 -0.35)
                        (Point3d.meters -1.5 1.5 -0.35)
                        (Point3d.meters -1.5 -1.5 -0.35)
                        (Point3d.meters 1.5 -1.5 -0.35)

                wallMaterial =
                    Material.nonmetal { baseColor = Color.darkGreen, roughness = 0.5 }

                wallRadius =
                    Length.meters 0.1

                leftWall =
                    Scene3d.group
                        [ Scene3d.cylinderWithShadow wallMaterial
                            (Cylinder3d.startingAt (Point3d.meters -1.5 1.5 -0.35)
                                Direction3d.positiveX
                                { length = Length.meters 3
                                , radius = wallRadius
                                }
                            )
                        , Scene3d.sphereWithShadow wallMaterial
                            (Sphere3d.atPoint (Point3d.meters -1.5 1.5 -0.35) wallRadius)
                        ]

                walls =
                    Scene3d.group
                        [ leftWall
                        , leftWall |> Scene3d.rotateAround Axis3d.z (Angle.turns 0.25)
                        , leftWall |> Scene3d.rotateAround Axis3d.z (Angle.turns 0.5)
                        , leftWall |> Scene3d.rotateAround Axis3d.z (Angle.turns 0.75)
                        ]

                sunlight =
                    Light.directional (Light.castsShadows True)
                        { chromaticity = Light.sunlight
                        , intensity = Illuminance.lux 100000
                        , direction = Direction3d.yz (Angle.degrees -135)
                        }

                daylight =
                    Light.overhead
                        { chromaticity = Light.daylight
                        , intensity = Illuminance.lux 10000
                        , upDirection = Direction3d.positiveZ
                        }

                camera =
                    Camera3d.perspective
                        { viewpoint =
                            Viewpoint3d.orbitZ
                                { focalPoint = Point3d.meters 0 0 -0.35
                                , distance = Length.meters 7
                                , azimuth = Angle.degrees 30
                                , elevation = Angle.degrees 32
                                }
                        , verticalFieldOfView = Angle.degrees 30
                        }
            in
            Scene3d.custom
                { entities = [ duckling, water, walls ]
                , lights = Scene3d.twoLights sunlight daylight
                , background = Scene3d.transparentBackground
                , camera = camera
                , antialiasing = Scene3d.multisampling
                , toneMapping = Scene3d.hableFilmicToneMapping
                , exposure = Scene3d.exposureValue 13
                , whiteBalance = Light.daylight
                , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                , clipDepth = Length.centimeters 1
                }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
