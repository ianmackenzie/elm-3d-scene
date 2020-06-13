module MeshWithShadow exposing (main)

{-| This example extends the CustomMesh example to show the different steps
needed to have a mesh cast a shadow:

  - Create a Shadow value from the mesh
  - Include that shadow in the scene using Scene3d.meshWithShadow
  - Include at least one light that casts a shadow

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
Shadow value from that mesh.
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

        -- Create a simple floor object to cast a shadow onto
        floor =
            Scene3d.quad (Material.matte Color.darkGrey)
                (Point3d.centimeters 25 30 -5)
                (Point3d.centimeters -30 30 -5)
                (Point3d.centimeters -30 -25 -5)
                (Point3d.centimeters 25 -25 -5)

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
        -- bulb, and specify that it should cast shadows (only up to four lights
        -- in a given scene can cast shadows, and casting shadows is relatively
        -- expensive so try to limit the number of shadows and shadow-casting
        -- lights in a given scene)
        lightBulb =
            Light.point (Light.castsShadows True)
                { position = Point3d.centimeters 50 -40 50
                , chromaticity = Light.incandescent -- color of the light
                , intensity = LuminousFlux.lumens 400 -- total light 'power'
                }

        -- Add some soft lighting to fill in shadowed areas
        softLighting =
            Light.overhead
                { upDirection = Direction3d.z
                , chromaticity = Light.incandescent
                , intensity = Illuminance.lux 50
                }
    in
    Scene3d.custom
        { entities = [ pyramidEntity, floor ]
        , camera = camera
        , lights = Scene3d.twoLights lightBulb softLighting
        , exposure = Scene3d.exposureValue 5 -- typical exposure value for indoors scenes
        , whiteBalance = Light.incandescent
        , antialiasing = Scene3d.multisampling
        , toneMapping = Scene3d.noToneMapping
        , background = Scene3d.transparentBackground
        , clipDepth = Length.centimeters 1
        , dimensions = ( Pixels.pixels 400, Pixels.pixels 300 )
        }
