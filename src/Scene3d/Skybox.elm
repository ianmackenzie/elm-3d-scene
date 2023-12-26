module Scene3d.Skybox exposing
    ( Skybox
    , loadEquirectangular
    )

{-|

@docs Skybox


# Loading a Skybox texture

@docs loadEquirectangular

-}

import Scene3d.Skybox.Protected exposing (Skybox(..))
import Task exposing (Task)
import WebGL.Texture


{-| Loaded and ready to use equirectangular `Skybox` texture.
-}
type alias Skybox =
    Scene3d.Skybox.Protected.Skybox



-- LOADING EQUIRECTANGULAR SKYBOX TEXTURE


{-| Function which defines a task for loading an equirectangular texture,
which can then be applied as a `Scene3d` skybox background.

The first argument is the path (absolute or relative) to the texture.

-}
loadEquirectangular : String -> Task WebGL.Texture.Error Skybox
loadEquirectangular =
    Task.map EquirectTexture
        << WebGL.Texture.loadWith
            { magnify = WebGL.Texture.linear
            , minify = WebGL.Texture.nearest
            , horizontalWrap = WebGL.Texture.clampToEdge
            , verticalWrap = WebGL.Texture.clampToEdge
            , flipY = True
            }
