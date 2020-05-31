module HelloWorld exposing (main)

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
    Scene3d.unlit
        { entities =
            -- Create a single rectangle from its color and four vertices
            [ Scene3d.quad (Material.color Color.blue)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)
            ]
        , camera =
            -- Create a camera using perspective projection
            Camera3d.perspective
                { viewpoint =
                    -- Camera is at the point (4, 2, 2), looking at the point
                    -- (0, 0, 0), oriented so that positive Z appears up
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 4 2 2
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        -- Anything closer than 1 meter to the camera will be clipped away
        , clipDepth = Length.meters 1

        -- Using a transparent background means that the HTML underneath the
        -- scene will show through
        , background = Scene3d.transparentBackground

        -- Size in pixels of the generated HTML element
        , dimensions = ( Pixels.pixels 400, Pixels.pixels 300 )
        }
