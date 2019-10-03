module Sphere exposing (main)

import Angle exposing (Angle)
import Browser
import Browser.Events as Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Html exposing (Html)
import Illuminance
import Length exposing (Meters)
import Luminance
import LuminousFlux
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable
import Scene3d.Exposure as Exposure
import Scene3d.Light as Light
import Scene3d.Mesh as Mesh exposing (Mesh, NoTangents, NoUV, ShadowsDisabled, Triangles, WithNormals)
import Scene3d.Shape as Shape
import Task
import Temperature
import Vector3d
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture exposing (Texture)



-- MSG


type Msg
    = LoadedBaseColorTexture (Result WebGL.Texture.Error Texture)
    | LoadedRoughnessTexture (Result WebGL.Texture.Error Texture)
    | LoadedMetallicTexture (Result WebGL.Texture.Error Texture)
    | Tick Float



-- MODEL


type alias Model =
    { baseColor : Maybe Texture
    , roughness : Maybe Texture
    , metallic : Maybe Texture
    , time : Float
    }


initModel =
    { baseColor = Nothing, roughness = Nothing, metallic = Nothing, time = 0 }



-- INIT


init flags =
    ( initModel
    , Cmd.batch
        [ WebGL.Texture.load "assets/metal/Metal03_col.jpg" |> Task.attempt LoadedBaseColorTexture
        , WebGL.Texture.load "assets/metal/Metal03_rgh.jpg" |> Task.attempt LoadedRoughnessTexture
        , WebGL.Texture.load "assets/metal/Metal03_met.jpg" |> Task.attempt LoadedMetallicTexture
        ]
    )



-- UPDATE


update msg model =
    case msg of
        LoadedBaseColorTexture (Ok t) ->
            ( { model | baseColor = Just t }, Cmd.none )

        LoadedRoughnessTexture (Ok t) ->
            ( { model | roughness = Just t }, Cmd.none )

        LoadedMetallicTexture (Ok t) ->
            ( { model | metallic = Just t }, Cmd.none )

        Tick dt ->
            ( { model | time = model.time + dt }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Texture example"
    , body =
        [ case ( model.baseColor, model.roughness, model.metallic ) of
            ( Just c, Just r, Just m ) ->
                renderScene { baseColor = c, roughness = r, metallic = m } model.time

            _ ->
                Html.text "Not loaded"
        ]
    }



-- SCENE


type World
    = World


mesh : Mesh World (Triangles WithNormals NoUV NoTangents ShadowsDisabled)
mesh =
    Shape.sphere
        { radius = Length.centimeters 5
        , subdivisions = 72
        }


mesh2 =
    Shape.sphere
        { radius = Length.centimeters 4.6
        , subdivisions = 72
        }


viewpoint : Float -> Viewpoint3d Meters World
viewpoint time =
    let
        t =
            0.25 * pi * time / 1000.0

        x =
            30 * sin (sin t + 1.7)

        y =
            30 * cos (sin t + 1.7)
    in
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.centimeters x y 15
        , upDirection = Direction3d.positiveZ
        }


sunlight =
    Light.directional
        Chromaticity.daylight
        (Illuminance.lux 10000)
        (Direction3d.xy (Angle.degrees -190))


ambientLighting =
    Light.overcast
        { chromaticity = Chromaticity.daylight
        , zenithDirection = Direction3d.positiveZ
        , zenithLuminance = Luminance.nits 5000
        }


camera : Float -> Camera3d Meters World
camera t =
    Camera3d.perspective
        { viewpoint = viewpoint t
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = Length.centimeters 0.5
        }


renderScene textures t =
    Scene3d.render []
        { camera = camera t
        , width = Pixels.pixels 800
        , height = Pixels.pixels 600
        , ambientLighting = Just ambientLighting
        , lights =
            Scene3d.oneLight sunlight { castsShadows = False }
        , exposure =
            Exposure.fromMaxLuminance
                (Luminance.nits 5000)
        , whiteBalance = Chromaticity.daylight
        }
        [ Drawable.physical textures mesh
            |> Drawable.translateBy (Vector3d.centimeters 0 3 0)
        , Drawable.physical textures mesh2
            |> Drawable.translateBy (Vector3d.centimeters 0 -7 0)
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Events.onAnimationFrameDelta Tick
        }
