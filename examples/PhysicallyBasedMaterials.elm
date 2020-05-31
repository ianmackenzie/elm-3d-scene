module PhysicallyBasedMaterials exposing (main)

import Angle exposing (Angle)
import Camera3d exposing (Camera3d)
import Color
import Direction3d
import Html exposing (Html)
import Illuminance
import Length exposing (Meters)
import Pixels
import Point3d
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Sphere3d
import Viewpoint3d exposing (Viewpoint3d)


main : Html msg
main =
    let
        -- Define a blue nonmetal (plastic or similar) material
        material =
            Material.nonmetal
                { baseColor = Color.blue
                , roughness = 0.4 -- varies from 0 (mirror-like) to 1 (matte)
                }

        -- Create a sphere entity using the defined material
        sphereEntity =
            Scene3d.sphere material <|
                Sphere3d.withRadius (Length.centimeters 5) Point3d.origin

        -- Add a directional light approximating sunlight
        sunlight =
            Light.directional (Light.castsShadows False)
                { chromaticity = Light.sunlight
                , intensity = Illuminance.lux 10000
                , direction = Direction3d.yz (Angle.degrees -120)
                }

        -- Add some soft overhead lighting to account for light coming from the
        -- sky and reflected from surrounding objects
        overheadLighting =
            Light.overhead
                { upDirection = Direction3d.positiveZ
                , intensity = Illuminance.lux 15000
                , chromaticity = Light.daylight
                }

        -- Use a typical exposure value for a reasonably sunny scene (see
        -- https://en.wikipedia.org/wiki/Exposure_value#Tabulated_exposure_values
        -- for some decent values to start with)
        exposure =
            Scene3d.exposureValue 12

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
    Scene3d.custom
        { camera = camera
        , clipDepth = Length.centimeters 0.5
        , dimensions = ( Pixels.pixels 300, Pixels.pixels 300 )
        , antialiasing = Scene3d.multisampling
        , lights = Scene3d.twoLights sunlight overheadLighting
        , exposure = exposure
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Light.daylight
        , background = Scene3d.transparentBackground
        , entities = [ sphereEntity ]
        }
