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
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), zero)
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material
import Shapes
import Vector3d exposing (Vector3d)


type PointLight units coordinates
    = PointLight
        { position : Point3d units coordinates
        , color : ( Float, Float, Float )
        , radius : Quantity Float units
        , drawable : Drawable units coordinates
        }


at : Point3d units coordinates -> { color : ( Float, Float, Float ), radius : Quantity Float units } -> PointLight units coordinates
at position { color, radius } =
    let
        ( red, green, blue ) =
            color

        (Quantity r) =
            radius

        scale =
            1.0 / (r * r)

        scaled component =
            min (scale * component) 1

        emissiveColor =
            Color.rgb (scaled red) (scaled green) (scaled blue)

        material =
            Material.emissive emissiveColor

        sphereDrawable =
            Shapes.sphere material Point3d.origin radius
    in
    PointLight
        { position = position
        , color = color
        , radius = radius
        , drawable = sphereDrawable
        }


light : PointLight units coordinates -> Light units coordinates
light (PointLight pointLight) =
    Light.point pointLight.position pointLight.color


drawable : PointLight units coordinates -> Drawable units coordinates
drawable (PointLight pointLight) =
    pointLight.drawable


translateBy : Vector3d units coordinates -> PointLight units coordinates -> PointLight units coordinates
translateBy displacement (PointLight pointLight) =
    PointLight
        { position = Point3d.translateBy displacement pointLight.position
        , color = pointLight.color
        , radius = pointLight.radius
        , drawable = Drawable.translateBy displacement pointLight.drawable
        }


rotateAround : Axis3d units coordinates -> Angle -> PointLight units coordinates -> PointLight units coordinates
rotateAround axis angle (PointLight pointLight) =
    PointLight
        { position = Point3d.rotateAround axis angle pointLight.position
        , color = pointLight.color
        , radius = pointLight.radius
        , drawable = Drawable.rotateAround axis angle pointLight.drawable
        }


mirrorAcross : Plane3d units coordinates -> PointLight units coordinates -> PointLight units coordinates
mirrorAcross plane (PointLight pointLight) =
    PointLight
        { position = Point3d.mirrorAcross plane pointLight.position
        , color = pointLight.color
        , radius = pointLight.radius
        , drawable = Drawable.mirrorAcross plane pointLight.drawable
        }


placeIn : Frame3d units globalCoordinates { defines : localCoordinates } -> PointLight units localCoordinates -> PointLight units globalCoordinates
placeIn frame (PointLight pointLight) =
    PointLight
        { position = Point3d.placeIn frame pointLight.position
        , color = pointLight.color
        , radius = pointLight.radius
        , drawable = Drawable.placeIn frame pointLight.drawable
        }


relativeTo : Frame3d units globalCoordinates { defines : localCoordinates } -> PointLight units globalCoordinates -> PointLight units localCoordinates
relativeTo frame (PointLight pointLight) =
    PointLight
        { position = Point3d.relativeTo frame pointLight.position
        , color = pointLight.color
        , radius = pointLight.radius
        , drawable = Drawable.relativeTo frame pointLight.drawable
        }
