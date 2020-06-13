module Triangles exposing (main)

import Angle exposing (Angle)
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import SketchPlane3d
import Triangle3d
import Viewpoint3d


main : Html msg
main =
    let
        triangle1 =
            Scene3d.triangle (Material.color Color.orange) <|
                Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 0 0)
                    (Point3d.meters 1 1 0)

        triangle2 =
            Scene3d.triangle (Material.color Color.blue) <|
                Triangle3d.from
                    (Point3d.meters 0 0 0)
                    (Point3d.meters 1 1 0)
                    (Point3d.meters 0 1 0)

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.meters 0.5 0.5 0
                        , eyePoint = Point3d.meters 3 3 2
                        , upDirection = Direction3d.z
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Scene3d.unlit
        { entities = [ triangle1, triangle2 ]
        , camera = camera
        , clipDepth = Length.meters 0.1
        , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
        , background = Scene3d.transparentBackground
        }
