module HelloWorld exposing (main)

{-| Just about the simplest elm-3d-scene program! This example introduces
several fundamental concepts used in elm-3d-scene:

  - Creating an entity to draw
  - Defining a camera
  - Rendering a scene to create an HTML element

-}

import Angle
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d


main : Html msg
main =
    let
        -- Create a single rectangle from its color and four vertices
        -- (Scene3d.quad can be used to create any flat four-sided shape)
        square =
            Scene3d.quad (Material.color Color.blue)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)

        -- Create a camera using perspective projection
        camera =
            Camera3d.perspective
                { -- Camera is at the point (4, 2, 2), looking at the point
                  -- (0, 0, 0), oriented so that positive Z appears up
                  viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 4 2 2
                        , upDirection = Direction3d.positiveZ
                        }

                -- The image on the screen will have a total rendered 'height'
                -- of 30 degrees; small angles make the camera act more like a
                -- telescope and large numbers make it act more like a fisheye
                -- lens
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    -- Render a scene that doesn't involve any lighting (no lighting is needed
    -- here since we provided a material that will result in a constant color
    -- no matter what lighting is used)
    Scene3d.unlit
        { -- Our scene has a single 'entity' in it
          entities = [ square ]

        -- Provide the camera to be used when rendering the scene
        , camera = camera

        -- Anything closer than 1 meter to the camera will be clipped away
        -- (this is necessary because of the internals of how WebGL works)
        , clipDepth = Length.meters 1

        -- Using a transparent background means that the HTML underneath the
        -- scene will show through
        , background = Scene3d.transparentBackground

        -- Size in pixels of the generated HTML element
        , dimensions = ( Pixels.int 400, Pixels.int 300 )
        }
