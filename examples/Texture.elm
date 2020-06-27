module Texture exposing (main)

{-| This example shows how to load a texture and apply it to a simple 3D entity.
-}

import Angle exposing (Angle)
import Browser
import Camera3d
import Color exposing (Color)
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Task
import Viewpoint3d
import WebGL.Texture


type Model
    = Loading -- Waiting for texture to load
    | Loaded (Material.Texture Color) -- Successfully loaded texture
    | Error WebGL.Texture.Error -- Error occurred when loading texture


type Msg
    = GotTexture (Result WebGL.Texture.Error (Material.Texture Color))


textureUrl : String
textureUrl =
    "https://ianmackenzie.github.io/elm-3d-scene/images/1.0.0/blue-elm-logo.png"


init : () -> ( Model, Cmd Msg )
init () =
    -- Attempt to load texture, using trilinear filtering for maximum smoothness
    ( Loading
    , Task.attempt GotTexture (Material.loadWith Material.trilinearFiltering textureUrl)
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        GotTexture (Ok texture) ->
            -- Successfully loaded the texture
            ( Loaded texture, Cmd.none )

        GotTexture (Err error) ->
            -- Network error, bad image dimensions etc.
            ( Error error, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        -- Construct a fixed camera
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbitZ
                        { focalPoint = Point3d.meters 0.5 0.5 0
                        , azimuth = Angle.degrees 20
                        , elevation = Angle.degrees 60
                        , distance = Length.meters 3
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    { title = "Texture"
    , body =
        case model of
            Loading ->
                -- Waiting for texture to load
                [ Html.text "Loading..." ]

            Loaded texture ->
                -- Texture loaded successfully, render a scene using it
                [ Scene3d.unlit
                    { camera = camera
                    , dimensions = ( Pixels.int 400, Pixels.int 400 )
                    , background = Scene3d.transparentBackground
                    , clipDepth = Length.meters 0.1
                    , entities =
                        [ -- Use the loaded texture as the material for a square
                          Scene3d.quad (Material.texturedColor texture)
                            (Point3d.meters 0 0 0)
                            (Point3d.meters 1 0 0)
                            (Point3d.meters 1 1 0)
                            (Point3d.meters 0 1 0)
                        ]
                    }
                ]

            Error error ->
                -- Use a placeholder error message if texture loading fails
                [ Html.text (Debug.toString error) ]
    }


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
