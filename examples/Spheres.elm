module Spheres exposing (Model, Msg(..), aluminumSphere, blackPlasticSphere, camera, goldSphere, height, init, main, update, view, viewpoint, whitePlasticSphere, width)

import Angle
import Browser
import Camera3d exposing (Camera3d)
import Direction3d exposing (Direction3d)
import Html exposing (Html)
import Length exposing (Meters, meters)
import Materials
import Pixels exposing (Pixels, pixels)
import Point3d
import Quantity exposing (Quantity, zero)
import Rectangle2d
import Scene3d
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Light as Light
import Shapes
import SketchPlane3d
import Task
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture


type alias Model =
    { loadedTexture : Maybe (Result WebGL.Texture.Error Light.AmbientLookupTexture)
    }


type Msg
    = LoadComplete (Result WebGL.Texture.Error Light.AmbientLookupTexture)


type WorldCoordinates
    = WorldCoordinates


type ScreenCoordinates
    = ScreenCoordinates


init : () -> ( Model, Cmd Msg )
init () =
    ( { loadedTexture = Nothing }
    , Task.attempt LoadComplete (Light.loadAmbientLookupTextureFrom "lookup.png")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update (LoadComplete loadedTexture) _ =
    ( { loadedTexture = Just loadedTexture }, Cmd.none )


goldSphere : Drawable Meters WorldCoordinates
goldSphere =
    Shapes.sphere Materials.gold
        (Point3d.meters 2 2 0)
        (meters 1)


aluminumSphere : Drawable Meters WorldCoordinates
aluminumSphere =
    Shapes.sphere Materials.aluminum
        (Point3d.meters 2 -2 0)
        (meters 1)


blackPlasticSphere : Drawable Meters WorldCoordinates
blackPlasticSphere =
    Shapes.sphere Materials.blackPlastic
        (Point3d.meters -2 -2 0)
        (meters 1)


whitePlasticSphere : Drawable Meters WorldCoordinates
whitePlasticSphere =
    Shapes.sphere Materials.whitePlastic
        (Point3d.meters -2 2 0)
        (meters 1)


viewpoint : Viewpoint3d Meters WorldCoordinates
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.meters 10 10 10
        , upDirection = Direction3d.positiveZ
        }


width : Quantity Float Pixels
width =
    pixels 1024


height : Quantity Float Pixels
height =
    pixels 768


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 30
        , clipDepth = meters 0.1
        }


view : Model -> Html Msg
view model =
    case model.loadedTexture of
        Nothing ->
            Html.text "Loading ambient lookup texture..."

        Just (Err _) ->
            Html.text "Error loading ambient lookup texture"

        Just (Ok lookupTexture) ->
            let
                lightDirection =
                    Direction3d.fromAzimuthInAndElevationFrom
                        SketchPlane3d.xy
                        (Angle.degrees -60)
                        (Angle.degrees -30)

                lights =
                    [ Light.directional lightDirection ( 0, 0.2, 0.2 )
                    , Light.directional Direction3d.negativeX ( 0.2, 0, 0 )
                    , Light.point (Point3d.meters 0 -2 3)
                        ( 0.75, 0.75, 0.75 )
                    , Light.ambient lookupTexture ( 0.03, 0.03, 0.03 )
                    ]

                scene =
                    Drawable.group
                        [ goldSphere
                        , aluminumSphere
                        , blackPlasticSphere
                        , whitePlasticSphere
                        ]
            in
            Scene3d.renderWith [ Scene3d.devicePixelRatio 2 ] lights camera ( width, height ) scene


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \model -> { title = "Spheres", body = [ view model ] }
        , subscriptions = always Sub.none
        }
