module Common.Scene exposing (BodyCoordinates, WorldCoordinates, view)

import Angle
import Axis3d exposing (Axis3d)
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes as Attributes
import Illuminance
import Length exposing (Meters, meters)
import Luminance
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
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Exposure as Exposure
import Scene3d.Light as Light
import WebGL exposing (Entity)


type BodyCoordinates
    = BodyCoordinates


type WorldCoordinates
    = WorldCoordinates


type alias Params =
    { world : World (Drawable BodyCoordinates)
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

        sunlight =
            Light.directional Chromaticity.daylight
                (Illuminance.lux 10000)
                (Direction3d.xyZ (Angle.degrees 45) (Angle.degrees -60))

        ambientLighting =
            Light.overcast
                { zenithDirection = Direction3d.z
                , chromaticity = Chromaticity.daylight
                , zenithLuminance = Luminance.nits 5000
                }
    in
    Scene3d.render []
        { width = screenWidth
        , height = screenHeight
        , camera = camera
        , lights =
            Scene3d.oneLight sunlight { castsShadows = True }
        , ambientLighting =
            Just ambientLighting
        , exposure =
            Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Chromaticity.daylight
        }
        drawables


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
    -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
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


getTransformedDrawable : Body (Drawable BodyCoordinates) -> Drawable WorldCoordinates
getTransformedDrawable body =
    Body.getData body
        |> Drawable.placeIn
            (toFrame (Body.getTransformation body))
