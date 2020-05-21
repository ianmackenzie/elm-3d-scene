module Scene3d.Transformation exposing
    ( Transformation
    , compose
    , identity
    , mirrorAcross
    , modelMatrix
    , placeIn
    , placementFrame
    , relativeTo
    , rotateAround
    , scaleAbout
    , translateBy
    )

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Length exposing (Meters)
import Math.Matrix4 exposing (Mat4)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Scene3d.Types as Types
import Vector3d exposing (Vector3d)


type alias Transformation =
    Types.Transformation


placementFrame : Transformation -> Frame3d Meters coordinates defines
placementFrame transformation =
    Frame3d.unsafe
        { originPoint =
            Point3d.unsafe
                { x = transformation.px
                , y = transformation.py
                , z = transformation.pz
                }
        , xDirection =
            Direction3d.unsafe
                { x = transformation.ix
                , y = transformation.iy
                , z = transformation.iz
                }
        , yDirection =
            Direction3d.unsafe
                { x = transformation.jx
                , y = transformation.jy
                , z = transformation.jz
                }
        , zDirection =
            Direction3d.unsafe
                { x = transformation.kx
                , y = transformation.ky
                , z = transformation.kz
                }
        }


modelMatrix : Transformation -> Mat4
modelMatrix transformation =
    Math.Matrix4.fromRecord
        { m11 = transformation.ix
        , m21 = transformation.iy
        , m31 = transformation.iz
        , m41 = 0
        , m12 = transformation.jx
        , m22 = transformation.jy
        , m32 = transformation.jz
        , m42 = 0
        , m13 = transformation.kx
        , m23 = transformation.ky
        , m33 = transformation.kz
        , m43 = 0
        , m14 = transformation.px
        , m24 = transformation.py
        , m34 = transformation.pz
        , m44 = 1
        }


identity : Transformation
identity =
    { ix = 1
    , iy = 0
    , iz = 0
    , jx = 0
    , jy = 1
    , jz = 0
    , kx = 0
    , ky = 0
    , kz = 1
    , px = 0
    , py = 0
    , pz = 0
    , scale = 1
    , isRightHanded = True
    }


translateBy : Vector3d Meters coordinates -> Transformation
translateBy displacement =
    let
        v =
            Vector3d.unwrap displacement
    in
    { ix = 1
    , iy = 0
    , iz = 0
    , jx = 0
    , jy = 1
    , jz = 0
    , kx = 0
    , ky = 0
    , kz = 1
    , px = v.x
    , py = v.y
    , pz = v.z
    , scale = 1
    , isRightHanded = True
    }


scaleAbout : Point3d Meters coordinates -> Float -> Transformation
scaleAbout point k =
    let
        p =
            Point3d.unwrap point

        oneMinusK =
            1 - k
    in
    { ix = 1
    , iy = 0
    , iz = 0
    , jx = 0
    , jy = 1
    , jz = 0
    , kx = 0
    , ky = 0
    , kz = 1
    , px = oneMinusK * p.x
    , py = oneMinusK * p.y
    , pz = oneMinusK * p.z
    , scale = k
    , isRightHanded = k >= 0
    }


rotateAround : Axis3d Meters coordinates -> Angle -> Transformation
rotateAround axis (Quantity angle) =
    let
        p0 =
            Point3d.unwrap (Axis3d.originPoint axis)

        a =
            Direction3d.unwrap (Axis3d.direction axis)

        halfAngle =
            0.5 * angle

        sinHalfAngle =
            sin halfAngle

        qx =
            a.x * sinHalfAngle

        qy =
            a.y * sinHalfAngle

        qz =
            a.z * sinHalfAngle

        qw =
            cos halfAngle

        wx =
            qw * qx

        wy =
            qw * qy

        wz =
            qw * qz

        xx =
            qx * qx

        xy =
            qx * qy

        xz =
            qx * qz

        yy =
            qy * qy

        yz =
            qy * qz

        zz =
            qz * qz

        a00 =
            1 - 2 * (yy + zz)

        a10 =
            2 * (xy + wz)

        a20 =
            2 * (xz - wy)

        a01 =
            2 * (xy - wz)

        a11 =
            1 - 2 * (xx + zz)

        a21 =
            2 * (yz + wx)

        a02 =
            2 * (xz + wy)

        a12 =
            2 * (yz - wx)

        a22 =
            1 - 2 * (xx + yy)
    in
    { ix = a00
    , iy = a10
    , iz = a20
    , jx = a01
    , jy = a11
    , jz = a21
    , kx = a02
    , ky = a12
    , kz = a22
    , px = p0.x - a00 * p0.x - a01 * p0.y - a02 * p0.z
    , py = p0.y - a10 * p0.x - a11 * p0.y - a12 * p0.z
    , pz = p0.z - a20 * p0.x - a21 * p0.y - a22 * p0.z
    , scale = 1
    , isRightHanded = True
    }


mirrorAcross : Plane3d Meters coordinates -> Transformation
mirrorAcross plane =
    let
        n =
            Direction3d.unwrap (Plane3d.normalDirection plane)

        p0 =
            Point3d.unwrap (Plane3d.originPoint plane)

        a00 =
            1 - 2 * n.x * n.x

        a11 =
            1 - 2 * n.y * n.y

        a22 =
            1 - 2 * n.z * n.z

        a12 =
            -2 * n.y * n.z

        a02 =
            -2 * n.x * n.z

        a01 =
            -2 * n.x * n.y
    in
    { ix = a00
    , iy = a01
    , iz = a02
    , jx = a01
    , jy = a11
    , jz = a12
    , kx = a02
    , ky = a12
    , kz = a22
    , px = p0.x - a00 * p0.x - a01 * p0.y - a02 * p0.z
    , py = p0.y - a01 * p0.x - a11 * p0.y - a12 * p0.z
    , pz = p0.z - a02 * p0.x - a12 * p0.y - a22 * p0.z
    , scale = 1
    , isRightHanded = False
    }


relativeTo : Frame3d Meters coordinates defines -> Transformation
relativeTo frame =
    let
        i =
            Direction3d.unwrap (Frame3d.xDirection frame)

        j =
            Direction3d.unwrap (Frame3d.yDirection frame)

        k =
            Direction3d.unwrap (Frame3d.zDirection frame)

        p0 =
            Point3d.unwrap (Frame3d.originPoint frame)
    in
    { ix = i.x
    , iy = j.x
    , iz = k.x
    , jx = i.y
    , jy = j.y
    , jz = k.y
    , kx = i.z
    , ky = j.z
    , kz = k.z
    , px = -p0.x * i.x - p0.y * i.y - p0.z * i.z
    , py = -p0.x * j.x - p0.y * j.y - p0.z * j.z
    , pz = -p0.x * k.x - p0.y * k.y - p0.z * k.z
    , scale = 1
    , isRightHanded = Frame3d.isRightHanded frame
    }


placeIn : Frame3d Meters coordinates defines -> Transformation
placeIn frame =
    let
        i =
            Direction3d.unwrap (Frame3d.xDirection frame)

        j =
            Direction3d.unwrap (Frame3d.yDirection frame)

        k =
            Direction3d.unwrap (Frame3d.zDirection frame)

        p0 =
            Point3d.unwrap (Frame3d.originPoint frame)
    in
    { ix = i.x
    , iy = i.y
    , iz = i.z
    , jx = j.x
    , jy = j.y
    , jz = j.z
    , kx = k.x
    , ky = k.y
    , kz = k.z
    , px = p0.x
    , py = p0.y
    , pz = p0.z
    , scale = 1
    , isRightHanded = Frame3d.isRightHanded frame
    }


compose : Transformation -> Transformation -> Transformation
compose t1 t2 =
    -- t2 * t1 in matrix form
    { ix = t1.ix * t2.ix + t1.iy * t2.jx + t1.iz * t2.kx
    , iy = t1.ix * t2.iy + t1.iy * t2.jy + t1.iz * t2.ky
    , iz = t1.ix * t2.iz + t1.iy * t2.jz + t1.iz * t2.kz
    , jx = t1.jx * t2.ix + t1.jy * t2.jx + t1.jz * t2.kx
    , jy = t1.jx * t2.iy + t1.jy * t2.jy + t1.jz * t2.ky
    , jz = t1.jx * t2.iz + t1.jy * t2.jz + t1.jz * t2.kz
    , kx = t1.kx * t2.ix + t1.ky * t2.jx + t1.kz * t2.kx
    , ky = t1.kx * t2.iy + t1.ky * t2.jy + t1.kz * t2.ky
    , kz = t1.kx * t2.iz + t1.ky * t2.jz + t1.kz * t2.kz
    , px = t2.px + (t1.px * t2.ix + t1.py * t2.jx + t1.pz * t2.kx) * t2.scale
    , py = t2.py + (t1.px * t2.iy + t1.py * t2.jy + t1.pz * t2.ky) * t2.scale
    , pz = t2.pz + (t1.px * t2.iz + t1.py * t2.jz + t1.pz * t2.kz) * t2.scale
    , scale = t1.scale * t2.scale
    , isRightHanded = t1.isRightHanded == t2.isRightHanded
    }
