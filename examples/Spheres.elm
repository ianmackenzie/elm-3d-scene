module Spheres exposing (..)

import Html exposing (Html)
import Html.Attributes as Attributes
import Materials
import Math.Vector3 exposing (vec3)
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Light as Light
import OpenSolid.Scene.Node as Node
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d
import Shapes
import Task
import WebGL
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


direction phi theta =
    Direction3d
        ( -(sin phi * cos theta)
        , -(sin phi * sin theta)
        , -(cos phi)
        )


cameraFrame =
    Frame3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d ( 10, 10, 10 )
        , upDirection = Direction3d.positiveZ
        }


width =
    1024


height =
    768


camera =
    Camera.perspective
        { frame = cameraFrame
        , screenWidth = width
        , screenHeight = height
        , verticalFov = degrees 30
        , zNear = 0.1
        , zFar = 100
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
                    direction (degrees 60) (degrees 120)

                lights =
                    [ Light.directional lightDirection (vec3 0 0.2 0.2)
                    , Light.directional Direction3d.negativeX (vec3 0.2 0 0)
                    , Light.point (Point3d ( 0, -2, 3 )) (vec3 0.75 0.75 0.75)
                    , Light.ambient lookupTexture (vec3 0.03 0.03 0.03)
                    ]

                goldSphere =
                    Geometry.shaded Materials.gold unitSphere
                        |> Node.translateBy (Vector3d ( 2, 2, 0 ))

                aluminumSphere =
                    Geometry.shaded Materials.aluminum unitSphere
                        |> Node.translateBy (Vector3d ( 2, -2, 0 ))

                blackPlasticSphere =
                    Geometry.shaded Materials.blackPlastic unitSphere
                        |> Node.translateBy (Vector3d ( -2, -2, 0 ))

                whitePlasticSphere =
                    Geometry.shaded Materials.whitePlastic unitSphere
                        |> Node.translateBy (Vector3d ( -2, 2, 0 ))

                scene =
                    Node.group
                        [ goldSphere
                        , aluminumSphere
                        , blackPlasticSphere
                        , whitePlasticSphere
                        ]
            in
            Scene.renderWith { devicePixelRatio = 2 } lights camera scene


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
