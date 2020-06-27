module PhysicallyBasedRendering exposing (main)

{-| This example introduces rendering with realistic materials and light
sources.
-}

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Html exposing (Html)
import Length exposing (Meters)
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material exposing (Material)
import Sphere3d
import Viewpoint3d exposing (Viewpoint3d)


main : Html msg
main =
    let
        -- Define a blue nonmetal (plastic or similar) material
        material =
            Material.nonmetal
                { baseColor = Color.lightBlue
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }

        -- Create a sphere entity using the defined material
        sphereEntity =
            Scene3d.sphere material <|
                Sphere3d.withRadius (Length.centimeters 5) Point3d.origin

        -- Define a camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.centimeters 20 10 20
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    -- Use the preset 'Scene3d.sunny' which handles most of the lighting details
    -- for us (creates one direct light source approximating sunlight, and
    -- another soft light source representing sky light and light reflected from
    -- surrounding objects)
    Scene3d.sunny
        { camera = camera
        , clipDepth = Length.centimeters 0.5
        , dimensions = ( Pixels.int 300, Pixels.int 300 )
        , background = Scene3d.transparentBackground
        , entities = [ sphereEntity ]

        -- Specify that sunlight should not cast shadows (since we wouldn't see
        -- them anyways in this scene)
        , shadows = False

        -- Specify the global up direction (this controls the orientation of
        -- the sky light)
        , upDirection = Direction3d.z

        -- Specify the direction of incoming sunlight (note that this is the
        -- opposite of the direction *to* the sun)
        , sunlightDirection = Direction3d.yz (Angle.degrees -120)
        }
