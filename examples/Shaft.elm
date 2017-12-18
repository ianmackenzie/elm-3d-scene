module Shaft exposing (Shaft, with)

import OpenSolid.Body3d as Body3d exposing (Body3d)
import OpenSolid.BoundaryType as BoundaryType exposing (BoundaryType)
import OpenSolid.Circle2d as Circle2d exposing (Circle2d)
import OpenSolid.Curve2d as Curve2d exposing (Curve2d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Region2d as Region2d exposing (Region2d)
import OpenSolid.Vector3d as Vector3d exposing (Vector3d)


type Shaft
    = Shaft
        { frame : Frame3d
        , radius : Float
        , cylinderLength : Float
        , flatLength : Float
        , flatThickness : Float
        }


with : { startPoint : Point3d, endPoint : Point3d, diameter : Float, flatLength : Float, flatThickness : Float } -> Maybe Shaft
with { startPoint, endPoint, diameter, flatLength, flatThickness } =
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
body (Shaft properties) =
    let
        circularArc =
            Circle2d.with
                { centerPoint = Point2d.origin
                , radius = properties.radius
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
    in
    Body3d.fromSurfaces []
