module PointLight
    exposing
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

import Color
import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Scene.Drawable as Drawable exposing (Drawable)
import OpenSolid.Scene.Light as Light exposing (Light)
import OpenSolid.Scene.Material as Material
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)
import Shapes


type PointLight
    = PointLight
        { position : Point3d
        , color : ( Float, Float, Float )
        , radius : Float
        , baseDrawable : Drawable
        }


at : Point3d -> { color : ( Float, Float, Float ), radius : Float } -> PointLight
at position { color, radius } =
    let
        ( r, g, b ) =
            color

        scale =
            1.0 / (radius * radius)

        scaled component =
            round (255 * min (scale * component) 1)

        emissiveColor =
            Color.rgb (scaled r) (scaled g) (scaled b)

        material =
            Material.emissive emissiveColor

        baseDrawable =
            Shapes.sphere material Point3d.origin radius
    in
    PointLight
        { position = position
        , color = color
        , radius = radius
        , baseDrawable = baseDrawable
        }


light : PointLight -> Light
light (PointLight { position, color }) =
    Light.point position color


drawable : PointLight -> Drawable
drawable (PointLight { baseDrawable, position }) =
    baseDrawable
        |> Drawable.translateBy
            (Vector3d.fromComponents (Point3d.coordinates position))


transformBy : (Point3d -> Point3d) -> PointLight -> PointLight
transformBy transform (PointLight properties) =
    PointLight { properties | position = transform properties.position }


translateBy : Vector3d -> PointLight -> PointLight
translateBy displacement =
    transformBy (Point3d.translateBy displacement)


rotateAround : Axis3d -> Float -> PointLight -> PointLight
rotateAround axis angle =
    transformBy (Point3d.rotateAround axis angle)


mirrorAcross : Plane3d -> PointLight -> PointLight
mirrorAcross plane =
    transformBy (Point3d.mirrorAcross plane)


placeIn : Frame3d -> PointLight -> PointLight
placeIn frame =
    transformBy (Point3d.placeIn frame)


relativeTo : Frame3d -> PointLight -> PointLight
relativeTo frame =
    transformBy (Point3d.relativeTo frame)
