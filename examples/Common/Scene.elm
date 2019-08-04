module Common.Scene exposing (ModelCoordinates, WorldCoordinates, view)

import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes as Attributes
import Length exposing (Meters, meters)
import Materials
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3)
import Physics.Body as Body exposing (Body)
import Physics.Debug as Debug
import Physics.World as World exposing (RaycastResult, World)
import Pixels exposing (Pixels, inPixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Light as Light
import WebGL exposing (Entity)


type ModelCoordinates
    = ModelCoordinates


type WorldCoordinates
    = WorldCoordinates


type alias Params =
    { world : World (Drawable Meters ModelCoordinates)
    , camera : Camera3d Meters WorldCoordinates
    , screenWidth : Quantity Float Pixels
    , screenHeight : Quantity Float Pixels
    , floorOffset : Maybe { x : Float, y : Float, z : Float }
    }


view : Params -> Html msg
view { world, floorOffset, camera, screenWidth, screenHeight } =
    let
        lightDirection =
            Vec3.normalize (Vec3.vec3 -1 -1 -1)

        drawables =
            List.map getTransformedDrawable (World.getBodies world)

        intensity =
            25

        distance =
            5

        lights =
            [ Light.point (Point3d.fromTuple meters ( distance, distance, 1.5 * distance )) ( intensity, intensity, intensity )
            , Light.point (Point3d.fromTuple meters ( -distance, distance, 1.5 * distance )) ( intensity, intensity, intensity )
            , Light.point (Point3d.fromTuple meters ( -distance, -distance, 1.5 * distance )) ( intensity, intensity, intensity )
            , Light.point (Point3d.fromTuple meters ( distance, -distance, 1.5 * distance )) ( intensity, intensity, intensity )
            , Light.point (Point3d.fromTuple meters ( 0, 0, 5 * distance )) ( 6 * intensity, 6 * intensity, 6 * intensity )
            ]
    in
    WebGL.toHtmlWith
        [ WebGL.depth 1
        , WebGL.alpha True
        , WebGL.antialias
        , WebGL.clearColor 0.7 0.7 0.7 1
        ]
        [ Attributes.width (round (inPixels screenWidth))
        , Attributes.height (round (inPixels screenHeight))
        , Attributes.style "position" "absolute"
        , Attributes.style "top" "0"
        , Attributes.style "left" "0"
        ]
        (Scene3d.toEntities lights camera ( screenWidth, screenHeight ) (Drawable.group drawables))


toFrame :
    { m11 : Float
    , m21 : Float
    , m31 : Float
    , m41 : Float
    , m12 : Float
    , m22 : Float
    , m32 : Float
    , m42 : Float
    , m13 : Float
    , m23 : Float
    , m33 : Float
    , m43 : Float
    , m14 : Float
    , m24 : Float
    , m34 : Float
    , m44 : Float
    }
    -> Frame3d Meters WorldCoordinates { defines : ModelCoordinates }
toFrame t =
    Frame3d.unsafe
        { originPoint =
            Point3d.unsafe { x = t.m14, y = t.m24, z = t.m34 }
        , xDirection =
            Direction3d.unsafe { x = t.m11, y = t.m21, z = t.m31 }
        , yDirection =
            Direction3d.unsafe { x = t.m12, y = t.m22, z = t.m32 }
        , zDirection =
            Direction3d.unsafe { x = t.m13, y = t.m23, z = t.m33 }
        }


getTransformedDrawable : Body (Drawable Meters ModelCoordinates) -> Drawable Meters WorldCoordinates
getTransformedDrawable body =
    Body.getData body
        |> Drawable.placeIn
            (toFrame (Body.getTransformation body))
