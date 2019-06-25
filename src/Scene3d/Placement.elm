module Scene3d.Placement exposing
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

import Axis3d exposing (Axis3d)
import Frame3d exposing (Frame3d)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Scene3d.Types as Types
import Vector3d exposing (Vector3d)


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
        , isRightHanded = isRightHanded placement == (scalingFactor >= 0.0)
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
