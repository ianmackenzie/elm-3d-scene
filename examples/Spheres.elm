module Spheres exposing (..)

import Html exposing (Html)
import Materials
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Drawable as Drawable exposing (Drawable)
import OpenSolid.Scene.Light as Light
import OpenSolid.Viewpoint as Viewpoint exposing (Viewpoint)
import Shapes
import Task
import WebGL.Texture


type alias Model =
    { loadedTexture : Maybe (Result WebGL.Texture.Error Light.AmbientLookupTexture)
    }


type Msg
    = LoadComplete (Result WebGL.Texture.Error Light.AmbientLookupTexture)


init : ( Model, Cmd Msg )
init =
    ( { loadedTexture = Nothing }
    , Task.attempt LoadComplete (Light.loadAmbientLookupTextureFrom "lookup.png")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update (LoadComplete loadedTexture) _ =
    ( { loadedTexture = Just loadedTexture }, Cmd.none )


goldSphere : Drawable
goldSphere =
    Shapes.sphere Materials.gold
        (Point3d.fromCoordinates ( 2, 2, 0 ))
        1.0


aluminumSphere : Drawable
aluminumSphere =
    Shapes.sphere Materials.aluminum
        (Point3d.fromCoordinates ( 2, -2, 0 ))
        1.0


blackPlasticSphere : Drawable
blackPlasticSphere =
    Shapes.sphere Materials.blackPlastic
        (Point3d.fromCoordinates ( -2, -2, 0 ))
        1.0


whitePlasticSphere : Drawable
whitePlasticSphere =
    Shapes.sphere Materials.whitePlastic
        (Point3d.fromCoordinates ( -2, 2, 0 ))
        1.0


viewpoint : Viewpoint
viewpoint =
    Viewpoint.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d.fromCoordinates ( 10, 10, 10 )
        , upDirection = Direction3d.positiveZ
        }


width : Float
width =
    1024


height : Float
height =
    768


camera : Camera
camera =
    Camera.perspective
        { viewpoint = viewpoint
        , screenWidth = width
        , screenHeight = height
        , verticalFieldOfView = degrees 30
        , nearClipDistance = 0.1
        , farClipDistance = 100
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
                    Direction3d.with
                        { elevation = degrees -30
                        , azimuth = degrees -60
                        }

                lights =
                    [ Light.directional lightDirection ( 0, 0.2, 0.2 )
                    , Light.directional Direction3d.negativeX ( 0.2, 0, 0 )
                    , Light.point (Point3d.fromCoordinates ( 0, -2, 3 ))
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
            Scene.renderWith [ Scene.devicePixelRatio 2 ] lights camera scene


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
