module OpenSolid.Scene3d
    exposing
        ( Scene3d
        , Mesh
        , Uniforms
        , Varyings
        , Shader
        , Camera
        , uniforms
        , point3d
        , vector3d
        , direction3d
        , point2d
        , color
        , perspectiveCamera
        , orthographicCamera
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Vector3d as Vector3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Triangle3d as Triangle3d
import WebGL exposing (Shader)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Html exposing (Html)
import Color exposing (Color)


type Scene3d
    = Scene3d (Frame3d -> Camera -> List WebGL.Entity)


type Mesh
    = Mesh (WebGL.Mesh Attributes)


type alias Attributes =
    { vertexPosition : Vec3
    , vertexNormal : Vec3
    , vertexTexturePosition : Vec2
    }


type alias Uniforms a =
    { a | viewMatrix : Mat4 }


type alias Varyings =
    { position : Vec4
    , normal : Vec4
    , texturePosition : Vec2
    }


type alias Shader a =
    WebGL.Shader {} (Uniforms a) Varyings


point3d : Point3d -> Vec4
point3d point_ =
    let
        ( x, y, z ) =
            Point3d.coordinates point_
    in
        Math.Vector4.vec4 x y z 1


originPoint3d : Vec4
originPoint3d =
    Point3d.origin >> point3d


vector3d : Vector3d -> Vec4
vector3d vector_ =
    let
        ( x, y, z ) =
            Vector3d.components vector_
    in
        Math.Vector4.vec4 x y z 0


zeroVector3d : Vec4
zeroVector3d =
    Vector3d.zero >> vector3d


direction3d : Direction3d -> Vec4
direction3d =
    Direction3d.toVector >> vector3d


point2d : Point2d -> Vec2
point2d =
    Point2d.coordinates >> Math.Vector2.fromTuple


color : Color -> Vec4
color color_ =
    let
        { red, green, blue, alpha } =
            Color.toRgb color_
    in
        Math.Vector4.vec4 red green blue alpha


originPoint2d : Vec2
originPoint2d =
    Point2d.origin >> point2d


triangleAttributes : Triangle3d -> ( Attributes, Attributes, Attributes )
triangleAttributes triangle =
    let
        triangleNormal =
            case Triangle3d.normalDirection triangle of
                Just normalDirection ->
                    vector3d (Direction3d.toVector normalDirection)

                Nothing ->
                    Triangle3d.normalDirection triangle
                        |> Maybe.map Direction3d.toVector
                        |> Maybe.withDefault Vector3d.zero
                        |> vector3d

        texturePosition =
            Math.Vector2d.vec2 0 0

        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
        ( Attributes (point3d p1) triangleNormal texturePosition
        , Attributes (point3d p2) triangleNormal texturePosition
        , Attributes (point3d p3) triangleNormal texturePosition
        )


triangles : List Triangle3d -> Mesh
triangles =
    WebGL.triangles << List.map triangleAttributes


render : Mesh -> Shader a -> a -> Scene3d
render mesh shader uniforms =
    ()


toHtml : List (Html.Attribute msg) -> Scene3d -> Html msg
toHtml attributes scene =
    ()
