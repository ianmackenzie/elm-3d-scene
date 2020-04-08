module ShadowTest exposing (main)

import Angle
import Camera3d exposing (Camera3d)
import Cylinder3d
import Direction3d
import Html exposing (Html)
import Illuminance
import Length exposing (Meters)
import Luminance
import Palette.Tango as Tango
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material as Material
import Sphere3d
import Viewpoint3d exposing (Viewpoint3d)


type WorldCoordinates
    = WorldCoordinates


main : Html msg
main =
    Scene3d.toHtml []
        { camera = camera
        , dimensions = ( Pixels.pixels 1400, Pixels.pixels 900 )
        , environmentalLighting =
            Scene3d.softLighting
                { upDirection = Direction3d.positiveZ
                , above = { intensity = Illuminance.lux (pi * 5000), chromaticity = Scene3d.daylight }
                , below = { intensity = Quantity.zero, chromaticity = Scene3d.daylight }
                }
        , lights =
            Scene3d.oneLight sunlight
        , exposure = Scene3d.maxLuminance (Luminance.nits 5000)
        , whiteBalance = Scene3d.daylight
        , background = Scene3d.transparentBackground
        , clipDepth = Length.centimeters 0.5
        }
        [ Scene3d.sphere Scene3d.castsShadows (Material.uniform ballMaterial) <|
            Sphere3d.withRadius (Length.meters 0.1) (Point3d.fromMeters { x = 30, y = 30, z = 0.2 })
        , Scene3d.quad Scene3d.doesNotCastShadows (Material.uniform grassMaterial) (Point3d.meters 4 4 0) (Point3d.meters 104 4 0) (Point3d.meters 104 64 0) (Point3d.meters 4 64 0)
        , Cylinder3d.from (Point3d.fromMeters { x = 20, y = 40, z = 0 }) (Point3d.fromMeters { x = 20, y = 40, z = 2 }) (Length.meters 1)
            |> Maybe.map (Scene3d.cylinder Scene3d.castsShadows (Material.uniform playerMaterialA))
            |> Maybe.withDefault
                (Scene3d.sphere Scene3d.castsShadows (Material.uniform ballMaterial) <|
                    Sphere3d.withRadius (Length.meters 0.35) (Point3d.fromMeters { x = 20, y = 20, z = 0 })
                )
        ]


viewpoint : Viewpoint3d Meters WorldCoordinates
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.fromMeters { x = 64, y = 34, z = 0 }
        , eyePoint = Point3d.meters 64 34 80
        , upDirection = Direction3d.positiveY
        }


sunlight =
    Scene3d.directionalLight Scene3d.castsShadows
        { chromaticity = Scene3d.daylight
        , intensity = Illuminance.lux 5000
        , direction = Direction3d.yz (Angle.degrees -150)
        }


camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.perspective
        { viewpoint = viewpoint
        , verticalFieldOfView = Angle.degrees 60
        }


ballMaterial : Material.Uniform coordinates
ballMaterial =
    Material.nonmetal
        { baseColor = Tango.aluminum2
        , roughness = 0.4
        }


playerMaterialA : Material.Uniform coordinates
playerMaterialA =
    Material.nonmetal
        { baseColor = Tango.orange1
        , roughness = 0.4
        }


grassMaterial : Material.Uniform coordinates
grassMaterial =
    Material.nonmetal
        { baseColor = Tango.chameleon3
        , roughness = 0.4
        }
