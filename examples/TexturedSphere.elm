module TexturedSphere exposing (main)

{-| This example illustrates creating a fully textured physically-based object,
with textures controlling color, roughness and metallicness. It also includes
some logic for rotating an object in the scene (as opposed to orbiting the
camera).
-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Direction3d
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length exposing (Meters)
import Pixels exposing (Pixels)
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Sphere3d exposing (Sphere3d)
import Task
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture


type WorldCoordinates
    = WorldCoordinates


{-| Here, we define a separate coordinate system for the local sphere coordinate
system
-}
type SphereCoordinates
    = SphereCoordinates


type Model
    = Loading
        { colorTexture : Maybe (Material.Texture Color)
        , roughnessTexture : Maybe (Material.Texture Float)
        , metallicTexture : Maybe (Material.Texture Float)
        }
    | Loaded
        { colorTexture : Material.Texture Color
        , roughnessTexture : Material.Texture Float
        , metallicTexture : Material.Texture Float
        , orbiting : Bool

        -- This frame exists in world coordinates and defines the orientation
        -- of the sphere coordinate system (and therefore the orientation of the
        -- sphere itself)
        , sphereFrame : Frame3d Meters WorldCoordinates { defines : SphereCoordinates }
        }
    | Errored String


type Msg
    = GotColorTexture (Result WebGL.Texture.Error (Material.Texture Color))
    | GotRoughnessTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | GotMetallicTexture (Result WebGL.Texture.Error (Material.Texture Float))
    | MouseDown
    | MouseUp
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)


init : ( Model, Cmd Msg )
init =
    ( Loading
        { colorTexture = Nothing
        , roughnessTexture = Nothing
        , metallicTexture = Nothing
        }
    , Cmd.batch
        [ Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_col.jpg"
            |> Task.attempt GotColorTexture
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_rgh.jpg"
            |> Task.attempt GotRoughnessTexture
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_met.jpg"
            |> Task.attempt GotMetallicTexture
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        updatedModel =
            case model of
                Loading textures ->
                    case message of
                        GotColorTexture (Ok colorTexture) ->
                            checkIfLoaded { textures | colorTexture = Just colorTexture }

                        GotRoughnessTexture (Ok roughnessTexture) ->
                            checkIfLoaded { textures | roughnessTexture = Just roughnessTexture }

                        GotMetallicTexture (Ok metallicTexture) ->
                            checkIfLoaded { textures | metallicTexture = Just metallicTexture }

                        GotColorTexture (Err _) ->
                            Errored "Error loading color texture"

                        GotRoughnessTexture (Err _) ->
                            Errored "Error loading roughness texture"

                        GotMetallicTexture (Err _) ->
                            Errored "Error loading metallic texture"

                        MouseDown ->
                            model

                        MouseUp ->
                            model

                        MouseMove _ _ ->
                            model

                Loaded loadedModel ->
                    case message of
                        GotColorTexture _ ->
                            model

                        GotRoughnessTexture _ ->
                            model

                        GotMetallicTexture _ ->
                            model

                        MouseDown ->
                            Loaded { loadedModel | orbiting = True }

                        MouseUp ->
                            Loaded { loadedModel | orbiting = False }

                        MouseMove dx dy ->
                            if loadedModel.orbiting then
                                let
                                    rotationRate =
                                        Angle.degrees 1 |> Quantity.per Pixels.pixel

                                    -- Here we figure out what axis to rotate the sphere around,
                                    -- based on the drag direction. For example, if we drag
                                    -- vertically, then we want to rotate around a horizontal axis,
                                    -- and if we drag horizontally then we want to rotate around a
                                    -- vertical axis. 'Horizontal' and 'vertical' here should be
                                    -- with respect to the view plane, not the global coordinate
                                    -- system, so we use the X and Y directions of the viewpoint.
                                    rotationVector =
                                        Vector3d.withLength (dx |> Quantity.at rotationRate)
                                            (Viewpoint3d.yDirection viewpoint)
                                            |> Vector3d.plus
                                                (Vector3d.withLength (dy |> Quantity.at rotationRate)
                                                    (Viewpoint3d.xDirection viewpoint)
                                                )
                                in
                                case Vector3d.direction rotationVector of
                                    Just direction ->
                                        let
                                            -- Rotate the Frame3d defining the orientation of the
                                            -- sphere around the axis that we've constructed, by
                                            -- an angle based on the total length of the drag
                                            newFrame =
                                                loadedModel.sphereFrame
                                                    |> Frame3d.rotateAround
                                                        (Axis3d.through (Frame3d.originPoint loadedModel.sphereFrame) direction)
                                                        (Vector3d.length rotationVector)
                                        in
                                        Loaded { loadedModel | sphereFrame = newFrame }

                                    Nothing ->
                                        model

                            else
                                model

                Errored _ ->
                    model
    in
    ( updatedModel, Cmd.none )


{-| Every time a texture gets returned from an HTTP request, one of the Maybe
fields in the record below gets set to 'Just texture'. Every time that happens,
we use this function to check if _all_ of the textures have been loaded. If so,
we can transition into the Loaded state; otherwise we stay in the Loading state
and wait for the remaining textures to load.
-}
checkIfLoaded :
    { colorTexture : Maybe (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , metallicTexture : Maybe (Material.Texture Float)
    }
    -> Model
checkIfLoaded textures =
    case ( textures.colorTexture, textures.roughnessTexture, textures.metallicTexture ) of
        ( Just colorTexture, Just roughnessTexture, Just metallicTexture ) ->
            Loaded
                { colorTexture = colorTexture
                , roughnessTexture = roughnessTexture
                , metallicTexture = metallicTexture
                , orbiting = False

                -- Start with the sphere coordinate system aligned with the
                -- world coordinate system
                , sphereFrame = Frame3d.atOrigin
                }

        _ ->
            Loading textures


{-| In this example, we have a fixed viewpoint since we rotate the sphere
instead.
-}
viewpoint : Viewpoint3d Meters WorldCoordinates
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.centimeters 20 10 10
        , upDirection = Direction3d.positiveZ
        }


sunlight : Light WorldCoordinates Bool
sunlight =
    Light.directional (Light.castsShadows False)
        { chromaticity = Light.sunlight
        , intensity = Illuminance.lux 20000
        , direction = Direction3d.yz (Angle.degrees -120)
        }


sky : Light WorldCoordinates Never
sky =
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , intensity = Illuminance.lux 7000
        , chromaticity = Light.skylight
        }


environment : Light WorldCoordinates Never
environment =
    Light.overhead
        { upDirection = Direction3d.negativeZ
        , intensity = Illuminance.lux 5000
        , chromaticity = Light.daylight
        }


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        }


{-| Define the sphere in its own coordinate system
-}
sphere : Sphere3d Meters SphereCoordinates
sphere =
    Sphere3d.withRadius (Length.centimeters 5) Point3d.origin


view : Model -> Html msg
view model =
    case model of
        Loaded { colorTexture, roughnessTexture, metallicTexture, sphereFrame } ->
            let
                -- Create a fully textured PBR material from the three loaded
                -- textures. Note that you can also use Material.constant if you
                -- want to use a constant value for one or two of the parameters
                -- instead of an actual texture.
                material =
                    Material.texturedPbr
                        { baseColor = colorTexture
                        , roughness = roughnessTexture
                        , metallic = metallicTexture
                        }
            in
            Scene3d.custom
                { camera = camera
                , clipDepth = Length.centimeters 0.5
                , dimensions = ( Pixels.int 400, Pixels.int 400 )
                , antialiasing = Scene3d.multisampling
                , lights = Scene3d.threeLights sunlight sky environment
                , exposure = Scene3d.exposureValue 11
                , toneMapping = Scene3d.hableFilmicToneMapping
                , whiteBalance = Light.daylight
                , background = Scene3d.transparentBackground
                , entities =
                    [ -- Create a sphere entity in local sphere coordinates
                      Scene3d.sphere material sphere
                        -- Place the sphere in the (rotated) frame to convert it
                        -- into world coordinates
                        |> Scene3d.placeIn sphereFrame
                    ]
                }

        Loading _ ->
            Html.text "Loading..."

        Errored message ->
            Html.text message


decodeMouseMove : Decoder Msg
decodeMouseMove =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loading _ ->
            Sub.none

        Errored _ ->
            Sub.none

        Loaded { orbiting } ->
            if orbiting then
                Sub.batch
                    [ Browser.Events.onMouseMove decodeMouseMove
                    , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                    ]

            else
                Browser.Events.onMouseDown (Decode.succeed MouseDown)


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
