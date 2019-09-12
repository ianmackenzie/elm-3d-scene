module Shaft exposing (Shaft, body, fromEndpoints)

import OpenSolid.Arc2d as Arc2d exposing (Arc2d)
import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.Body3d as Body3d exposing (Body3d)
import OpenSolid.BoundaryType as BoundaryType exposing (BoundaryType)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Curve2d as Curve2d
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Region2d as Region2d exposing (Region2d)
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type Shaft
    = Shaft
        { frame : Frame3d
        , radius : Float
        , cylinderLength : Float
        , flatLength : Float
        , flatThickness : Float
        }


fromEndpoints : { startPoint : Point3d, endPoint : Point3d, diameter : Float, flatLength : Float, flatThickness : Float } -> Maybe Shaft
fromEndpoints { startPoint, endPoint, diameter, flatLength, flatThickness } =
    case Vector3d.lengthAndDirection (Vector3d.from startPoint endPoint) of
        Just ( length, direction ) ->
            if diameter <= 0 then
                Nothing
            else if flatLength <= 0 then
                Nothing
            else if flatLength >= length then
                Nothing
            else if flatThickness <= 0 then
                Nothing
            else if flatThickness >= diameter then
                Nothing
            else
                let
                    frame =
                        Frame3d.with
                            { originPoint = startPoint
                            , zDirection = direction
                            }
                in
                Just <|
                    Shaft
                        { frame = frame
                        , radius = diameter / 2
                        , cylinderLength = length - flatLength
                        , flatLength = flatLength
                        , flatThickness = flatThickness
                        }

        Nothing ->
            Nothing


body : Shaft -> Body3d
body (Shaft { frame, radius, cylinderLength, flatLength, flatThickness }) =
    let
        circularArc =
            Circle2d.with
                { centerPoint = Point2d.origin
                , radius = radius
                }
                |> Circle2d.toArc
                |> Curve2d.arc

        circularRegion =
            Region2d.fanWith
                { start = BoundaryType.interior
                , end = BoundaryType.interior
                , curve = BoundaryType.exterior
                }
                Point2d.origin
                circularArc

        xySketchPlane =
            Frame3d.xySketchPlane frame

        cylindricalBody =
            Body3d.extrusion circularRegion xySketchPlane cylinderLength

        joinSketchPlane =
            xySketchPlane |> SketchPlane3d.offsetBy cylinderLength

        halfThickness =
            flatThickness / 2

        flatHalfAngle =
            asin (halfThickness / radius)

        flatLowerRight =
            Point2d.fromPolarCoordinates ( radius, -flatHalfAngle )

        flatRightArc =
            Curve2d.arc <|
                Arc2d.with
                    { centerPoint = Point2d.origin
                    , startPoint = flatLowerRight
                    , sweptAngle = 2 * flatHalfAngle
                    }

        flatLeftArc =
            flatRightArc |> Curve2d.mirrorAcross Axis2d.y |> Curve2d.reverse

        fan =
            Region2d.fanWith
                { start = BoundaryType.interior
                , end = BoundaryType.interior
                , curve = BoundaryType.exterior
                }
                Point2d.origin

        flatTop =
            Curve2d.lineSegment <|
                LineSegment2d.fromEndpoints
                    ( Curve2d.endPoint flatRightArc
                    , Curve2d.startPoint flatLeftArc
                    )

        flatBottom =
            Curve2d.lineSegment <|
                LineSegment2d.fromEndpoints
                    ( Curve2d.endPoint flatLeftArc
                    , Curve2d.startPoint flatRightArc
                    )

        flatRegion =
            Region2d.fuse
                [ fan flatRightArc
                , fan flatTop
                , fan flatLeftArc
                , fan flatBottom
                ]

        flatBody =
            Body3d.extrusion flatRegion joinSketchPlane flatLength
    in
    Body3d.fromSurfaces <|
        List.concat
            [ Body3d.surfaces cylindricalBody
            , Body3d.surfaces flatBody
            ]
