module Rotation exposing (main)

{-| This example shows how to create multiple rotated copies of a single 3D
entity in order to create circular pattern.
-}

import Angle exposing (Angle)
import Axis3d
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Parameter1d
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material as Material
import Triangle3d
import Viewpoint3d


main : Html msg
main =
    let
        -- Create two triangles that together form the unit square in the XY
        -- plane (the square from (0, 0) to (1, 1))
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

        -- Create a square entity by grouping together the two triangles (and
        -- giving them separate colors)
        square =
            Scene3d.group [ triangle1, triangle2 ]

        -- Construct an axis that the square will be rotated around
        rotationAxis =
            Axis3d.through (Point3d.meters 0 2 0) Direction3d.x

        -- Generate 16 evenly spaced rotation angles (see https://package.elm-lang.org/packages/ianmackenzie/elm-1d-parameter/latest/Parameter1d#leading
        -- and https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Quantity#interpolateFrom
        -- for details)
        angles =
            Parameter1d.leading 16 <|
                Quantity.interpolateFrom
                    (Angle.degrees 0)
                    (Angle.degrees 360)

        -- Helper function for constructing a rotated square at a particular
        -- angle by rotating it around the axis
        rotatedSquare angle =
            square |> Scene3d.rotateAround rotationAxis angle

        -- Generate a list of rotated squares
        entities =
            List.map rotatedSquare angles

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.meters 0.5 2 0
                        , eyePoint = Point3d.meters 9 5 4
                        , upDirection = Direction3d.z
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Scene3d.unlit
        { camera = camera
        , clipDepth = Length.meters 0.1
        , dimensions = ( Pixels.int 300, Pixels.int 300 )
        , background = Scene3d.transparentBackground
        , entities = entities
        }
