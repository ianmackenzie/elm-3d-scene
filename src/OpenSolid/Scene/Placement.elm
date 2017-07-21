module OpenSolid.Scene.Placement
    exposing
        ( Placement
        , compose
        , frame
        , identity
        , isRightHanded
        , mirrorAcross
        , placeIn
        , relativeTo
        , rotateAround
        , scale
        , scaleAbout
        , translateBy
        )

import Math.Matrix4 exposing (Mat4)
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene.Types as Types
import OpenSolid.Vector3d as Vector3d


type alias Placement =
    Types.Placement


identity : Placement
identity =
    Types.Placement
        { frame = Frame3d.xyz
        , scale = 1.0
        , isRightHanded = True
        }


frame : Placement -> Frame3d
frame (Types.Placement properties) =
    properties.frame


scale : Placement -> Float
scale (Types.Placement properties) =
    properties.scale


isRightHanded : Placement -> Bool
isRightHanded (Types.Placement properties) =
    properties.isRightHanded


scaleAbout : Point3d -> Float -> Placement -> Placement
scaleAbout referencePoint scalingFactor placement =
    let
        currentFrame =
            frame placement

        currentOrigin =
            Frame3d.originPoint currentFrame

        scaledOrigin =
            Point3d.scaleAbout referencePoint scalingFactor currentOrigin
    in
    Types.Placement
        { frame = Frame3d.moveTo scaledOrigin currentFrame
        , scale = scalingFactor * scale placement
        , isRightHanded =
            if scalingFactor >= 0.0 then
                isRightHanded placement
            else
                not (isRightHanded placement)
        }


rotateAround : Axis3d -> Float -> Placement -> Placement
rotateAround axis angle =
    let
        rotateFrame =
            Frame3d.rotateAround axis angle
    in
    \placement ->
        Types.Placement
            { frame = rotateFrame (frame placement)
            , scale = scale placement
            , isRightHanded = isRightHanded placement
            }


translateBy : Vector3d -> Placement -> Placement
translateBy displacement placement =
    Types.Placement
        { frame = Frame3d.translateBy displacement (frame placement)
        , scale = scale placement
        , isRightHanded = isRightHanded placement
        }


mirrorAcross : Plane3d -> Placement -> Placement
mirrorAcross plane placement =
    Types.Placement
        { frame = Frame3d.mirrorAcross plane (frame placement)
        , scale = scale placement
        , isRightHanded = not (isRightHanded placement)
        }


placeIn : Frame3d -> Placement -> Placement
placeIn newFrame placement =
    Types.Placement
        { frame = Frame3d.placeIn newFrame (frame placement)
        , scale = scale placement
        , isRightHanded =
            isRightHanded placement == Frame3d.isRightHanded newFrame
        }


relativeTo : Frame3d -> Placement -> Placement
relativeTo baseFrame placement =
    Types.Placement
        { frame = Frame3d.relativeTo baseFrame (frame placement)
        , scale = scale placement
        , isRightHanded =
            isRightHanded placement == Frame3d.isRightHanded baseFrame
        }


compose : Placement -> Placement -> Placement
compose firstPlacement secondPlacement =
    firstPlacement
        |> scaleAbout Point3d.origin (scale secondPlacement)
        |> placeIn (frame secondPlacement)
