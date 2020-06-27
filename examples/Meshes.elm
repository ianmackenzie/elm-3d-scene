module Meshes exposing (main)

{-| If you want to create complex shapes and render them efficiently, you will
likely need to create your own Mesh values. This example illustrates how to
create a mesh from scratch; in your own apps you might generate meshes
programmatically using some geometry-generation logic, or parse them from some
file format.
-}

import Angle
import Array
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh)
import TriangularMesh
import Viewpoint3d


{-| Declare a coordinate system type (many apps will only need a single
"world coordinates" type, but you can call it whatever you want)
-}
type WorldCoordinates
    = WorldCoordinates


{-| Define a pyramid shape to render. The 'Mesh.Uniform' type refers to a mesh
that does not have UV (texture) coordinates and therefore cannot have textures
applied to it, but _does_ have normal vectors defined and can therefore use
materials that involve lighting (in addition to materials like 'constant color'
which ignore lighting). Once created, a Mesh value should generally be stored
in your model and reused from frame to frame.
-}
pyramidMesh : Mesh.Uniform WorldCoordinates
pyramidMesh =
    let
        -- Define the vertices of our pyramid
        frontLeft =
            Point3d.centimeters 10 10 0

        frontRight =
            Point3d.centimeters 10 -10 0

        backLeft =
            Point3d.centimeters -10 10 0

        backRight =
            Point3d.centimeters -10 -10 0

        tip =
            Point3d.centimeters 0 0 4

        -- Create a TriangularMesh value from an array of vertices and list
        -- of index triples defining faces (see https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest/TriangularMesh#indexed)
        triangularMesh =
            TriangularMesh.indexed
                (Array.fromList
                    [ frontLeft -- 0
                    , frontRight -- 1
                    , backLeft -- 2
                    , backRight -- 3
                    , tip -- 4
                    ]
                )
                [ ( 1, 0, 4 ) -- front
                , ( 0, 2, 4 ) -- left
                , ( 2, 3, 4 ) -- back
                , ( 3, 1, 4 ) -- right
                , ( 1, 3, 0 ) -- bottom
                , ( 0, 3, 2 ) -- bottom
                ]
    in
    -- Create a elm-3d-scene Mesh value from the TriangularMesh; we use
    -- Mesh.indexedFacets so that normal vectors will be generated for each face
    Mesh.indexedFacets triangularMesh


main : Html msg
main =
    let
        -- Create an entity from a mesh. This is a cheap operation, so you can
        -- do things like dynamically change the material applied to mesh from
        -- frame to frame.
        pyramidEntity =
            Scene3d.mesh (Material.matte Color.blue) pyramidMesh

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.centimeters 40 20 30
                        , upDirection = Direction3d.z
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Scene3d.sunny
        { entities = [ pyramidEntity ]
        , camera = camera
        , upDirection = Direction3d.z
        , sunlightDirection = Direction3d.yz (Angle.degrees -120)
        , background = Scene3d.transparentBackground
        , clipDepth = Length.centimeters 1
        , shadows = False
        , dimensions = ( Pixels.int 400, Pixels.int 300 )
        }
