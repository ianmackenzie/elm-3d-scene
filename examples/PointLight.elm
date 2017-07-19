module PointLight
    exposing
        ( PointLight(PointLight)
        , light
        , node
        )

import Math.Vector3 exposing (Vec3)
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene.Geometry as Geometry exposing (Geometry)
import OpenSolid.Scene.Light as Light exposing (Light)
import OpenSolid.Scene.Material as Material
import OpenSolid.Scene.Node as Node exposing (Node)
import Shapes


type PointLight
    = PointLight
        { position : Point3d
        , color : Vec3
        , radius : Float
        }


position : PointLight -> Point3d
position (PointLight properties) =
    properties.position


color : PointLight -> Vec3
color (PointLight properties) =
    properties.color


radius : PointLight -> Float
radius (PointLight properties) =
    properties.radius


baseGeometry : Geometry
baseGeometry =
    Shapes.sphere Point3d.origin 1.0


light : PointLight -> Light
light pointLight =
    Light.point (position pointLight) (color pointLight)


node : PointLight -> Node
node pointLight =
    let
        ( r, g, b ) =
            Math.Vector3.toTuple (color pointLight)

        scale =
            1.0 / (radius pointLight * radius pointLight)

        emissiveColor =
            Math.Vector3.vec3 (scale * r) (scale * g) (scale * b)
    in
    baseGeometry
        |> Geometry.shaded (Material.emissive emissiveColor)
        |> Node.scaleAbout Point3d.origin (radius pointLight)
        |> Node.placeIn (Frame3d.at (position pointLight))
