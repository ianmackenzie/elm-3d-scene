module ShadowTest exposing (main)

import Angle
import Camera3d exposing (Camera3d)
import Color
import Cylinder3d
import Direction3d
import Html exposing (Html)
import Illuminance
import Length exposing (Meters)
import Luminance
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material
import Sphere3d
import Viewpoint3d exposing (Viewpoint3d)


type WorldCoordinates
    = WorldCoordinates


main : Html msg
main =
    Scene3d.custom
        { camera = camera
        , dimensions = ( Pixels.pixels 1400, Pixels.pixels 900 )
        , antialiasing = Scene3d.multisampling
        , lights = Scene3d.twoLights sunlight daylight
        , exposure = Scene3d.maxLuminance (Luminance.nits 5000)
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Light.daylight
        , background = Scene3d.transparentBackground
        , clipDepth = Length.centimeters 0.5
        , entities =
            [ Scene3d.sphereWithShadow (Material.uniform ballMaterial) <|
                Sphere3d.withRadius (Length.meters 0.1)
                    (Point3d.fromMeters { x = 30, y = 30, z = 0.2 })
            , Scene3d.quad (Material.uniform grassMaterial)
                (Point3d.meters 4 4 0)
                (Point3d.meters 104 4 0)
                (Point3d.meters 104 64 0)
                (Point3d.meters 4 64 0)
            , Cylinder3d.from
                (Point3d.fromMeters { x = 20, y = 40, z = 0 })
                (Point3d.fromMeters { x = 20, y = 40, z = 2 })
                (Length.meters 1)
                |> Maybe.map (Scene3d.cylinderWithShadow (Material.uniform playerMaterialA))
                |> Maybe.withDefault
                    (Scene3d.sphereWithShadow (Material.uniform ballMaterial) <|
                        Sphere3d.withRadius (Length.meters 0.35)
                            (Point3d.fromMeters { x = 20, y = 20, z = 0 })
                    )
            ]
        }


viewpoint : Viewpoint3d Meters WorldCoordinates
viewpoint =
    Viewpoint3d.lookAt
        { focalPoint = Point3d.fromMeters { x = 64, y = 34, z = 0 }
        , eyePoint = Point3d.meters 64 34 80
        , upDirection = Direction3d.positiveY
        }


sunlight : Light WorldCoordinates Bool
sunlight =
    Light.directional (Light.castsShadows True)
        { chromaticity = Light.daylight
        , intensity = Illuminance.lux 5000
        , direction = Direction3d.yz (Angle.degrees -150)
        }


daylight : Light WorldCoordinates Never
daylight =
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , chromaticity = Light.daylight
        , intensity = Illuminance.lux 15000
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
        { baseColor = Color.gray
        , roughness = 0.4
        }


playerMaterialA : Material.Uniform coordinates
playerMaterialA =
    Material.nonmetal
        { baseColor = Color.lightOrange
        , roughness = 0.4
        }


grassMaterial : Material.Uniform coordinates
grassMaterial =
    Material.nonmetal
        { baseColor = Color.darkGreen
        , roughness = 0.4
        }
