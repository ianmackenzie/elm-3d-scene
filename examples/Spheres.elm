module Spheres exposing (..)

import Html exposing (Html)
import Materials
import Math.Vector3 exposing (vec3)
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Light as Light
import OpenSolid.Scene.Node as Node
import OpenSolid.Vector3d as Vector3d
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


unitSphere : Geometry
unitSphere =
    Shapes.sphere Point3d.origin 1.0


cameraFrame : Frame3d
cameraFrame =
    Camera.lookAt
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
        { frame = cameraFrame
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
                    [ Light.directional lightDirection (vec3 0 0.2 0.2)
                    , Light.directional Direction3d.negativeX (vec3 0.2 0 0)
                    , Light.point (Point3d.fromCoordinates ( 0, -2, 3 ))
                        (vec3 0.75 0.75 0.75)
                    , Light.ambient lookupTexture (vec3 0.03 0.03 0.03)
                    ]

                goldSphere =
                    Geometry.shaded Materials.gold unitSphere
                        |> Node.translateBy
                            (Vector3d.fromComponents ( 2, 2, 0 ))

                aluminumSphere =
                    Geometry.shaded Materials.aluminum unitSphere
                        |> Node.translateBy
                            (Vector3d.fromComponents ( 2, -2, 0 ))

                blackPlasticSphere =
                    Geometry.shaded Materials.blackPlastic unitSphere
                        |> Node.translateBy
                            (Vector3d.fromComponents ( -2, -2, 0 ))

                whitePlasticSphere =
                    Geometry.shaded Materials.whitePlastic unitSphere
                        |> Node.translateBy
                            (Vector3d.fromComponents ( -2, 2, 0 ))

                scene =
                    Node.group
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
        , subscriptions = subscriptions
        }
