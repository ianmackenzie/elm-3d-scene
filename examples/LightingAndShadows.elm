module LightingAndShadows exposing (main)

{-| This example extends the CustomMesh example to set up a scene with
customized lighting and a mesh that casts a shadow.
-}

import Angle
import Array
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Illuminance
import Length
import LuminousFlux
import Pixels
import Point3d
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import TriangularMesh
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


{-| This is a slightly modified pyramid (taller, to make for a more
interesting shadow).
-}
pyramidMesh : Mesh.Uniform WorldCoordinates
pyramidMesh =
    let
        frontLeft =
            Point3d.centimeters 10 10 0

        frontRight =
            Point3d.centimeters 10 -10 0

        backLeft =
            Point3d.centimeters -10 10 0

        backRight =
            Point3d.centimeters -10 -10 0

        tip =
            Point3d.centimeters 0 0 20

        triangularMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ frontLeft
                    , frontRight
                    , backLeft
                    , backRight
                    , tip
                    ]
                )
                [ ( 1, 0, 4 )
                , ( 0, 2, 4 )
                , ( 2, 3, 4 )
                , ( 3, 1, 4 )
                , ( 1, 3, 0 )
                , ( 0, 3, 2 )
                ]
    in
    Mesh.indexedFacets triangularMesh


{-| In addition to constructing a Mesh value, we need to construct a special
Shadow value from that mesh. As with meshes, Shadow values should be created
once and then saved in your model.
-}
pyramidShadow : Mesh.Shadow WorldCoordinates
pyramidShadow =
    Mesh.shadow pyramidMesh


main : Html msg
main =
    let
        -- Use Scene3d.meshWithShadow instead of Scene3d.mesh, passing the
        -- Shadow value that we created
        pyramidEntity =
            Scene3d.meshWithShadow (Material.matte Color.blue) pyramidMesh pyramidShadow

        -- Create a simple 'floor' object to cast a shadow onto
        floor =
            Scene3d.quad (Material.matte Color.darkGrey)
                (Point3d.centimeters 25 30 -5)
                (Point3d.centimeters -30 30 -5)
                (Point3d.centimeters -30 -25 -5)
                (Point3d.centimeters 25 -25 -5)

        -- Define a camera as usual
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.centimeters 90 50 50
                        , upDirection = Direction3d.z
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        -- Create a point light representing an incandescent (tungsten) light
        -- bulb, and specify that it should cast shadows. Only up to four lights
        -- in a given scene can cast shadows, and casting shadows is relatively
        -- expensive, so try to limit the number of shadows and shadow-casting
        -- lights in a given scene.
        lightBulb =
            Light.point (Light.castsShadows True)
                { position = Point3d.centimeters 50 -40 50
                , chromaticity = Light.incandescent -- color of the light
                , intensity = LuminousFlux.lumens 400 -- total light 'power'
                }

        -- Create some soft lighting to fill in shadowed areas
        softLighting =
            Light.overhead
                { upDirection = Direction3d.z
                , chromaticity = Light.incandescent
                , intensity = Illuminance.lux 50
                }
    in
    -- Render a scene with custom lighting and other settings
    Scene3d.custom
        { entities = [ pyramidEntity, floor ]
        , camera = camera
        , background = Scene3d.transparentBackground
        , clipDepth = Length.centimeters 1
        , dimensions = ( Pixels.int 400, Pixels.int 300 )

        -- Define the lights to use in the scene. elm-3d-scene only supports up
        -- to eight total lights, so there are different functions for different
        -- numbers of lights instead of a single function taking a list.
        , lights = Scene3d.twoLights lightBulb softLighting

        -- This is a reasonably typical exposure value for an interior home
        -- scene; see https://en.wikipedia.org/wiki/Exposure_value#Tabulated_exposure_values
        -- for some representative values for different types of scenes
        , exposure = Scene3d.exposureValue 5

        -- White balance specifies what color shows up as white in the rendered
        -- scene; this should usually be set to the dominant light color
        , whiteBalance = Light.incandescent

        -- When using Scene3d.custom, we have to explicitly specify what kind of
        -- antialiasing (if any) to use
        , antialiasing = Scene3d.multisampling

        -- Similarly, we have to specify what kind of tone mapping (if any) to
        -- use; see the ExposureAndToneMapping example for details
        , toneMapping = Scene3d.noToneMapping
        }
