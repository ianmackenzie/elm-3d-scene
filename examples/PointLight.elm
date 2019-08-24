module PointLight exposing
    ( PointLight
    , at
    , drawable
    , light
    , mirrorAcross
    , placeIn
    , relativeTo
    , rotateAround
    , translateBy
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Color
import Frame3d exposing (Frame3d)
import Length exposing (Meters)
import LuminousFlux exposing (LuminousFlux)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), zero)
import Scene3d.Chromaticity exposing (Chromaticity)
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Light as Light exposing (Light)
import Scene3d.Shape as Shape
import SolidAngle exposing (spats)
import Sphere3d
import Vector3d exposing (Vector3d)


type PointLight coordinates
    = PointLight
        { position : Point3d Meters coordinates
        , chromaticity : Chromaticity
        , luminousFlux : LuminousFlux
        , radius : Quantity Float Meters
        , drawable : Drawable Meters coordinates
        }


at : Point3d Meters coordinates -> Chromaticity -> LuminousFlux -> Quantity Float Meters -> PointLight coordinates
at position chromaticity luminousFlux radius =
    let
        sphere =
            Sphere3d.withRadius radius position

        surfaceArea =
            Sphere3d.surfaceArea sphere

        luminance =
            luminousFlux
                |> Quantity.per (spats 1)
                |> Quantity.per surfaceArea

        sphereDrawable =
            Drawable.emissive
                chromaticity
                luminance
                (Shape.sphere
                    { radius = radius
                    , maxError = Quantity.multiplyBy 0.001 radius
                    }
                )
                |> Drawable.translateBy
                    (Vector3d.from Point3d.origin position)
    in
    PointLight
        { position = position
        , chromaticity = chromaticity
        , luminousFlux = luminousFlux
        , radius = radius
        , drawable = sphereDrawable
        }


light : PointLight coordinates -> Light Meters coordinates
light (PointLight pointLight) =
    Light.point pointLight.chromaticity pointLight.luminousFlux pointLight.position


drawable : PointLight coordinates -> Drawable Meters coordinates
drawable (PointLight pointLight) =
    pointLight.drawable


translateBy : Vector3d Meters coordinates -> PointLight coordinates -> PointLight coordinates
translateBy displacement (PointLight pointLight) =
    PointLight
        { position = Point3d.translateBy displacement pointLight.position
        , chromaticity = pointLight.chromaticity
        , luminousFlux = pointLight.luminousFlux
        , radius = pointLight.radius
        , drawable = Drawable.translateBy displacement pointLight.drawable
        }


rotateAround : Axis3d Meters coordinates -> Angle -> PointLight coordinates -> PointLight coordinates
rotateAround axis angle (PointLight pointLight) =
    PointLight
        { position = Point3d.rotateAround axis angle pointLight.position
        , chromaticity = pointLight.chromaticity
        , luminousFlux = pointLight.luminousFlux
        , radius = pointLight.radius
        , drawable = Drawable.rotateAround axis angle pointLight.drawable
        }


mirrorAcross : Plane3d Meters coordinates -> PointLight coordinates -> PointLight coordinates
mirrorAcross plane (PointLight pointLight) =
    PointLight
        { position = Point3d.mirrorAcross plane pointLight.position
        , chromaticity = pointLight.chromaticity
        , luminousFlux = pointLight.luminousFlux
        , radius = pointLight.radius
        , drawable = Drawable.mirrorAcross plane pointLight.drawable
        }


placeIn : Frame3d Meters globalCoordinates { defines : localCoordinates } -> PointLight localCoordinates -> PointLight globalCoordinates
placeIn frame (PointLight pointLight) =
    PointLight
        { position = Point3d.placeIn frame pointLight.position
        , chromaticity = pointLight.chromaticity
        , luminousFlux = pointLight.luminousFlux
        , radius = pointLight.radius
        , drawable = Drawable.placeIn frame pointLight.drawable
        }


relativeTo : Frame3d Meters globalCoordinates { defines : localCoordinates } -> PointLight globalCoordinates -> PointLight localCoordinates
relativeTo frame (PointLight pointLight) =
    PointLight
        { position = Point3d.relativeTo frame pointLight.position
        , chromaticity = pointLight.chromaticity
        , luminousFlux = pointLight.luminousFlux
        , radius = pointLight.radius
        , drawable = Drawable.relativeTo frame pointLight.drawable
        }
