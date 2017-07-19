module OpenSolid.Scene.Placement
    exposing
        ( Placement
        , compose
        , identity
        , isRightHanded
        , mirrorAcross
        , placeIn
        , projectOnto
        , relativeTo
        , rotateAround
        , scaleAbout
        , toMatrix
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
        { originPoint = Point3d.origin
        , xVector = Vector3d ( 1, 0, 0 )
        , yVector = Vector3d ( 0, 1, 0 )
        , zVector = Vector3d ( 0, 0, 1 )
        , isRightHanded = True
        }


originPoint : Placement -> Point3d
originPoint (Types.Placement properties) =
    properties.originPoint


xVector : Placement -> Vector3d
xVector (Types.Placement properties) =
    properties.xVector


yVector : Placement -> Vector3d
yVector (Types.Placement properties) =
    properties.yVector


zVector : Placement -> Vector3d
zVector (Types.Placement properties) =
    properties.zVector


isRightHanded : Placement -> Bool
isRightHanded (Types.Placement properties) =
    properties.isRightHanded


toMatrix : Placement -> Mat4
toMatrix placement =
    let
        ( x0, y0, z0 ) =
            Point3d.coordinates (originPoint placement)

        ( x1, y1, z1 ) =
            Vector3d.components (xVector placement)

        ( x2, y2, z2 ) =
            Vector3d.components (yVector placement)

        ( x3, y3, z3 ) =
            Vector3d.components (zVector placement)
    in
    Math.Matrix4.fromRecord
        { m11 = x1
        , m21 = y1
        , m31 = z1
        , m41 = 0
        , m12 = x2
        , m22 = y2
        , m32 = z2
        , m42 = 0
        , m13 = x3
        , m23 = y3
        , m33 = z3
        , m43 = 0
        , m14 = x0
        , m24 = y0
        , m34 = z0
        , m44 = 1
        }


scaleAbout : Point3d -> Float -> Placement -> Placement
scaleAbout referencePoint scale placement =
    Types.Placement
        { originPoint =
            Point3d.scaleAbout referencePoint scale (originPoint placement)
        , xVector = Vector3d.scaleBy scale (xVector placement)
        , yVector = Vector3d.scaleBy scale (yVector placement)
        , zVector = Vector3d.scaleBy scale (zVector placement)
        , isRightHanded =
            if scale >= 0.0 then
                isRightHanded placement
            else
                not (isRightHanded placement)
        }


rotateAround : Axis3d -> Float -> Placement -> Placement
rotateAround axis angle =
    let
        rotatePoint =
            Point3d.rotateAround axis angle

        rotateVector =
            Vector3d.rotateAround axis angle
    in
    \placement ->
        Types.Placement
            { originPoint = rotatePoint (originPoint placement)
            , xVector = rotateVector (xVector placement)
            , yVector = rotateVector (yVector placement)
            , zVector = rotateVector (zVector placement)
            , isRightHanded = isRightHanded placement
            }


translateBy : Vector3d -> Placement -> Placement
translateBy displacement placement =
    Types.Placement
        { originPoint = Point3d.translateBy displacement (originPoint placement)
        , xVector = xVector placement
        , yVector = yVector placement
        , zVector = zVector placement
        , isRightHanded = isRightHanded placement
        }


mirrorAcross : Plane3d -> Placement -> Placement
mirrorAcross plane placement =
    Types.Placement
        { originPoint = Point3d.mirrorAcross plane (originPoint placement)
        , xVector = Vector3d.mirrorAcross plane (xVector placement)
        , yVector = Vector3d.mirrorAcross plane (yVector placement)
        , zVector = Vector3d.mirrorAcross plane (zVector placement)
        , isRightHanded = not (isRightHanded placement)
        }


projectOnto : Plane3d -> Placement -> Placement
projectOnto plane placement =
    Types.Placement
        { originPoint = Point3d.projectOnto plane (originPoint placement)
        , xVector = Vector3d.projectOnto plane (xVector placement)
        , yVector = Vector3d.projectOnto plane (yVector placement)
        , zVector = Vector3d.projectOnto plane (zVector placement)
        , isRightHanded = isRightHanded placement
        }


placeIn : Frame3d -> Placement -> Placement
placeIn frame placement =
    Types.Placement
        { originPoint = Point3d.placeIn frame (originPoint placement)
        , xVector = Vector3d.placeIn frame (xVector placement)
        , yVector = Vector3d.placeIn frame (yVector placement)
        , zVector = Vector3d.placeIn frame (zVector placement)
        , isRightHanded = isRightHanded placement == Frame3d.isRightHanded frame
        }


relativeTo : Frame3d -> Placement -> Placement
relativeTo frame placement =
    Types.Placement
        { originPoint = Point3d.relativeTo frame (originPoint placement)
        , xVector = Vector3d.relativeTo frame (xVector placement)
        , yVector = Vector3d.relativeTo frame (yVector placement)
        , zVector = Vector3d.relativeTo frame (zVector placement)
        , isRightHanded = isRightHanded placement == Frame3d.isRightHanded frame
        }


placePoint : Point3d -> Placement -> Point3d
placePoint point placement =
    let
        ( x0, y0, z0 ) =
            Point3d.coordinates (originPoint placement)

        ( x1, y1, z1 ) =
            Vector3d.components (xVector placement)

        ( x2, y2, z2 ) =
            Vector3d.components (yVector placement)

        ( x3, y3, z3 ) =
            Vector3d.components (zVector placement)

        ( x, y, z ) =
            Point3d.coordinates point
    in
    Point3d
        ( x0 + x * x1 + y * x2 + z * x3
        , y0 + x * y1 + y * y2 + z * y3
        , z0 + x * z1 + y * z2 + z * z3
        )


placeVector : Vector3d -> Placement -> Vector3d
placeVector vector placement =
    let
        ( x1, y1, z1 ) =
            Vector3d.components (xVector placement)

        ( x2, y2, z2 ) =
            Vector3d.components (yVector placement)

        ( x3, y3, z3 ) =
            Vector3d.components (zVector placement)

        ( x, y, z ) =
            Vector3d.components vector
    in
    Vector3d
        ( x * x1 + y * x2 + z * x3
        , x * y1 + y * y2 + z * y3
        , x * z1 + y * z2 + z * z3
        )


compose : Placement -> Placement -> Placement
compose firstPlacement secondPlacement =
    Types.Placement
        { originPoint = placePoint (originPoint firstPlacement) secondPlacement
        , xVector = placeVector (xVector firstPlacement) secondPlacement
        , yVector = placeVector (yVector firstPlacement) secondPlacement
        , zVector = placeVector (zVector firstPlacement) secondPlacement
        , isRightHanded =
            isRightHanded firstPlacement == isRightHanded secondPlacement
        }
