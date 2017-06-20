module Spheres exposing (..)

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as Attributes
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Light as Light
import OpenSolid.Scene.Lighting as Lighting
import OpenSolid.Scene.Material as Material exposing (Material)
import OpenSolid.Scene.Node as Node
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d
import Sphere
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
    , Task.attempt LoadComplete (Light.loadAmbientLookupTexture "lookup.png")
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update (LoadComplete loadedTexture) _ =
    ( { loadedTexture = Just loadedTexture }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


unitSphere : Geometry
unitSphere =
    Sphere.sphere Point3d.origin 1.0


gold : Material
gold =
    Material.metal { color = Color.rgb 255 219 145, roughness = 0.4 }


aluminum : Material
aluminum =
    Material.metal { color = Color.rgb 245 245 247, roughness = 0.6 }


blackPlastic : Material
blackPlastic =
    Material.nonmetal { color = Color.black, roughness = 0.25 }


whitePlastic : Material
whitePlastic =
    Material.nonmetal { color = Color.white, roughness = 0.25 }


direction phi theta =
    Direction3d
        ( -(sin phi * cos theta)
        , -(sin phi * sin theta)
        , -(cos phi)
        )


lightDirection =
    direction (degrees 30) (degrees 90)


lightColor =
    Color.rgb 127 127 127


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
                lighting =
                    Lighting.singleLight (Light.ambient lookupTexture lightColor)

                goldSphere =
                    unitSphere
                        |> Node.shaded gold lighting
                        |> Node.translateBy (Vector3d ( 2, 2, 0 ))

                aluminumSphere =
                    unitSphere
                        |> Node.shaded aluminum lighting
                        |> Node.translateBy (Vector3d ( 2, -2, 0 ))

                blackPlasticSphere =
                    unitSphere
                        |> Node.shaded blackPlastic lighting
                        |> Node.translateBy (Vector3d ( -2, -2, 0 ))

                whitePlasticSphere =
                    unitSphere
                        |> Node.shaded whitePlastic lighting
                        |> Node.translateBy (Vector3d ( -2, 2, 0 ))

                scene =
                    Node.group
                        [ goldSphere
                        , aluminumSphere
                        , blackPlasticSphere
                        , whitePlasticSphere
                        ]
            in
            WebGL.toHtml
                [ Attributes.width width, Attributes.height height ]
                (Node.toEntities camera scene)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
