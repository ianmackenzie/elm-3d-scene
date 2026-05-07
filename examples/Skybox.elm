module Skybox exposing (main)

{-| This example shows how to load and set a skybox texture. The exmple also
implements camera rotation, and the skybox can be previewed from different
angles.
-}

import Angle exposing (Angle)
import Block3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import Frame3d
import Json.Decode as Decode exposing (Decoder)
import Length
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Scene3d.Skybox as Skybox exposing (Skybox, loadEquirectangular)
import Task
import Viewpoint3d
import WebGL.Texture


type alias Model =
    { width : Quantity Int Pixels -- Width of the browser window
    , height : Quantity Int Pixels -- Height of the browser window
    , orbiting : Bool
    , azimuth : Angle
    , elevation : Angle
    , skybox : Maybe Skybox
    }


type Msg
    = MouseUp
    | MouseDown
    | Resize (Quantity Int Pixels) (Quantity Int Pixels)
    | MouseMove (Quantity Float Pixels) (Quantity Float Pixels)
    | SkyboxLoaded (Result WebGL.Texture.Error Skybox)


type WorldCoordinates
    = WorldCoordinates


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , orbiting = False
      , azimuth = Angle.degrees 135
      , elevation = Angle.degrees 5
      , skybox = Nothing
      }
    , Cmd.batch
        [ Task.perform
            (\{ viewport } ->
                Resize
                    (Pixels.int (round viewport.width))
                    (Pixels.int (round viewport.height))
            )
            Browser.Dom.getViewport

        -- Load equirectangular texture
        , "https://ianmackenzie.github.io/elm-3d-scene/examples/skybox/umhlanga_sunrise_8k.jpg"
            |> Skybox.loadEquirectangular
            |> Task.attempt SkyboxLoaded
        ]
    )


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))


subs : Model -> Sub Msg
subs model =
    Sub.batch
        [ Browser.Events.onResize
            (\w h -> Resize (Pixels.int w) (Pixels.int h))

        --
        , if model.orbiting then
            Sub.batch
                [ Browser.Events.onMouseMove mouseMoveDecoder
                , Browser.Events.onMouseUp (Decode.succeed MouseUp)
                ]

          else
            Browser.Events.onMouseDown (Decode.succeed MouseDown)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize w h ->
            ( { model | width = w, height = h }, Cmd.none )

        MouseDown ->
            ( { model | orbiting = True }, Cmd.none )

        MouseUp ->
            ( { model | orbiting = False }, Cmd.none )

        MouseMove dx dy ->
            if model.orbiting then
                let
                    rotationRate =
                        Angle.degrees -0.05 |> Quantity.per Pixels.pixel

                    newAzimuth =
                        model.azimuth
                            |> Quantity.minus (dx |> Quantity.at rotationRate)

                    newElevation =
                        model.elevation
                            |> Quantity.plus (dy |> Quantity.at rotationRate)
                            |> Quantity.clamp (Angle.degrees -60) (Angle.degrees 60)
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

        SkyboxLoaded (Ok texture) ->
            ( { model | skybox = Just texture }, Cmd.none )

        SkyboxLoaded (Err err) ->
            ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
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
    { title = "Skybox"
    , body =
        [ Scene3d.sunny
            { upDirection = Direction3d.z
            , sunlightDirection = Direction3d.xyZ (Angle.degrees -135) (Angle.degrees -45)
            , shadows = False
            , dimensions = ( model.width, model.height )
            , camera = camera
            , clipDepth = Length.centimeters 10
            , background =
                model.skybox
                    |> Maybe.map Scene3d.backgroundSkybox
                    |> Maybe.withDefault (Scene3d.backgroundColor Color.lightBlue)
            , entities =
                [ Scene3d.block
                    (Material.matte Color.lightBrown)
                    (Block3d.centeredOn
                        (Frame3d.atPoint
                            (Point3d.centimeters 0 0 20)
                        )
                        ( Length.centimeters 0
                        , Length.centimeters 0
                        , Length.centimeters 0
                        )
                    )
                ]
            }
        ]
    }
