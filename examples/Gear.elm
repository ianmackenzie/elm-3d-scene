module Gear exposing (Gear, centeredOn)

import OpenSolid.Body3d as Body3d exposing (Body3d)
import OpenSolid.Direction2d as Direction2d exposing (Direction2d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Scalar as Scalar
import OpenSolid.SketchPlane3d as SketchPlane3d exposing (SketchPlane3d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


type Gear
    = Gear
        { sketchPlane : SketchPlane3d
        , numTeeth : Int
        , faceWidth : Float
        , module_ : Float
        , bore : Float
        , pressureAngle : Float
        }


centeredOn : SketchPlane3d -> { numTeeth : Int, faceWidth : Float, module_ : Float, bore : Float, pressureAngle : Float } -> Maybe Gear
centeredOn sketchPlane { numTeeth, faceWidth, module_, bore, pressureAngle } =
    if numTeeth <= 0 then
        Nothing
    else if faceWidth <= 0 then
        Nothing
    else if module_ <= 0 then
        Nothing
    else if bore <= 0 then
        Nothing
    else if pressureAngle <= 0 then
        Nothing
    else
        Just <|
            Gear
                { sketchPlane = sketchPlane
                , numTeeth = numTeeth
                , faceWidth = faceWidth
                , module_ = module_
                , bore = bore
                , pressureAngle = pressureAngle
                }


baseAngle : Float -> Float -> Float -> Float
baseAngle a b c =
    acos ((a * a + b * b - c * c) / (2 * a * b))


body : Gear -> Body3d
body (Gear { sketchPlane, numTeeth, faceWidth, module_, bore, pressureAngle }) =
    let
        pitchRadius =
            module_ * toFloat numTeeth / 2

        addendum =
            module_

        outsideRadius =
            pitchRadius + addendum

        dedendum =
            1.25 * module_

        filletRadius =
            0.25 * module_

        rootRadius =
            pitchRadius - dedendum

        baseRadius =
            pitchRadius * cos pressureAngle

        pointA =
            Point2d.fromCoordinates ( 0, outsideRadius )

        pointB =
            Point2d.fromCoordinates ( 0, baseRadius )

        radiusC =
            Scalar.interpolateFrom outsideRadius baseRadius (1 / 3)

        pointC =
            Point2d.fromCoordinates ( 0, radiusC )

        angleD =
            pi / 2 + acos (baseRadius / radiusC)

        pointD =
            Point2d.fromPolarCoordinates ( baseRadius, angleD )

        pointE =
            Point2d.interpolateFrom pointD pointC (1 / 4)

        toothThickness =
            pi * module_ / 2

        angleF =
            pi / 2 + baseAngle pitchRadius radiusC toothThickness

        pointF =
            Point2d.fromPolarCoordinates ( pitchRadius, angleF )

        profileRadius =
            Point2d.distanceFrom pointE pointC

        angleFG =
            2 * asin (profileRadius / (2 * pitchRadius))

        pointG =
            Point2d.fromPolarCoordinates ( pitchRadius, angleF - angleFG )

        -- going off script
        halfToothAngle =
            pi / (2 * toFloat numTeeth)

        rightContact =
            Point2d.fromPolarCoordinates
                ( pitchRadius, pi / 2 - halfToothAngle )

        rightCenterOffset =
            Vector2d.with
                { length = profileRadius
                , direction =
                    Direction2d.fromAngle (pi + pressureAngle - halfToothAngle)
                }

        rightCenter =
            rightContact |> Point2d.translateBy rightCenterOffset
    in
    Body3d.fromSurfaces []
