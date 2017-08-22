module OpenSolid.Scene.Node
    exposing
        ( Node
        , empty
        , group
        , mirrorAcross
        , placeIn
        , relativeTo
        , rotateAround
        , scaleAbout
        , translateBy
        )

import OpenSolid.Axis3d as Axis3d exposing (Axis3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Plane3d as Plane3d exposing (Plane3d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Scene.Placement as Placement exposing (Placement)
import OpenSolid.Scene.Types as Types
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type alias Node =
    Types.Node


empty : Node
empty =
    Types.EmptyNode


group : List Node -> Node
group nodes =
    Types.GroupNode nodes


transformBy : (Placement -> Placement) -> Node -> Node
transformBy placementTransformation node =
    case node of
        Types.TransformedNode placement node ->
            Types.TransformedNode (placementTransformation placement)
                node

        Types.EmptyNode ->
            Types.EmptyNode

        Types.LeafNode drawable ->
            Types.TransformedNode (placementTransformation Placement.identity)
                node

        Types.GroupNode childNodes ->
            Types.TransformedNode (placementTransformation Placement.identity)
                node


rotateAround : Axis3d -> Float -> Node -> Node
rotateAround axis angle node =
    transformBy (Placement.rotateAround axis angle) node


translateBy : Vector3d -> Node -> Node
translateBy displacement node =
    transformBy (Placement.translateBy displacement) node


mirrorAcross : Plane3d -> Node -> Node
mirrorAcross plane node =
    transformBy (Placement.mirrorAcross plane) node


relativeTo : Frame3d -> Node -> Node
relativeTo frame node =
    transformBy (Placement.relativeTo frame) node


placeIn : Frame3d -> Node -> Node
placeIn frame node =
    transformBy (Placement.placeIn frame) node


scaleAbout : Point3d -> Float -> Node -> Node
scaleAbout point scale node =
    transformBy (Placement.scaleAbout point scale) node
