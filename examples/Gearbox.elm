module Gearbox exposing (..)

import Dict exposing (Dict)
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Direction3d as Direction3d exposing (Direction3d)
import OpenSolid.Frame3d as Frame3d exposing (Frame3d)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Point3d as Point3d exposing (Point3d)
import OpenSolid.Scene.Drawable as Drawable exposing (Drawable)
import OpenSolid.Viewpoint as Viewpoint exposing (Viewpoint)
import Time exposing (Time)


type Msg
    = StartRotatingAt Point2d
    | PointerMovedTo Point2d
    | StopRotating
    | NumTeeth1Changed Int
    | NumTeeth2Changed Int
    | Tick Time


type alias GearDrawable =
    { numTeeth : Int
    , cache : Dict Int (() -> Drawable)
    , rotationAngle : Float
    }


type alias Model =
    { dragPoint : Maybe Point2d
    , placementFrame : Frame3d
    , gear1Drawable : GearDrawable
    , gear2Drawable : GearDrawable
    }



-- Rendering


screenWidth : Int
screenWidth =
    1024


screenHeight : Int
screenHeight =
    768


camera : Camera
camera =
    Camera.perspective
        { viewpoint =
            Viewpoint.lookAt
                { eyePoint = Point3d.fromCoordinates ( 6, 0, 3 )
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.z
                }
        , verticalFieldOfView = degrees 30
        , screenWidth = toFloat screenWidth
        , screenHeight = toFloat screenHeight
        , nearClipDistance = 0.1
        , farClipDistance = 100
        }
