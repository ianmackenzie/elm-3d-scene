module OpenSolid.Scene.Node
    exposing
        ( Node
        , colored
        , group
        , mirrorAcross
        , placeIn
        , relativeTo
        , rotateAround
        , shaded
        , translateBy
        )

import Color exposing (Color)
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Scene.Geometry exposing (Geometry)
import OpenSolid.Scene.Material exposing (Material)
import OpenSolid.Scene.SimpleGeometry exposing (SimpleGeometry)
import OpenSolid.Scene.Types as Types
import OpenSolid.WebGL.Color as Color


type alias Node =
    Types.Node


leaf : Types.Drawable -> Node
leaf drawable =
    Types.LeafNode Frame3d.xyz drawable


colored : Color -> SimpleGeometry -> Node
colored color geometry =
    leaf (Types.ColoredGeometry (Color.toVec3 color) geometry)


shaded : Material -> Geometry -> Node
shaded material geometry =
    leaf (Types.ShadedGeometry material geometry)


group : List Node -> Node
group nodes =
    Types.GroupNode Frame3d.xyz nodes


transformBy : (Frame3d -> Frame3d) -> Node -> Node
transformBy frameTransformation node =
    case node of
        Types.LeafNode frame drawable ->
            Types.LeafNode (frameTransformation frame) drawable

        Types.GroupNode frame nodes ->
            Types.GroupNode (frameTransformation frame) nodes


rotateAround : Axis3d -> Float -> Node -> Node
rotateAround axis angle node =
    transformBy (Frame3d.rotateAround axis angle) node


translateBy : Vector3d -> Node -> Node
translateBy displacement node =
    transformBy (Frame3d.translateBy displacement) node


mirrorAcross : Plane3d -> Node -> Node
mirrorAcross plane node =
    transformBy (Frame3d.mirrorAcross plane) node


relativeTo : Frame3d -> Node -> Node
relativeTo frame node =
    transformBy (Frame3d.relativeTo frame) node


placeIn : Frame3d -> Node -> Node
placeIn frame node =
    transformBy (Frame3d.placeIn frame) node
