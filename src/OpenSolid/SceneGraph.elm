module OpenSolid.SceneGraph
    exposing
        ( PositionOnly
        , WithNormals
        , WithNormalsAndTextureCoordinates
        , triangles
        , trianglesWithNormals
        , triangleFan
        , mesh
        , meshWithNormals
        , lines
        , polyline
        , points
        , Node
        , colored
        , group
        , rotateAround
        , translateBy
        , mirrorAcross
        , relativeTo
        , placeIn
        , Projection
        , perspectiveProjection
        , orthographicProjection
        , toEntities
        )

import OpenSolid.SceneGraph.Internal.Shader as Shader
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Color as Color
import Math.Vector2 as Vector2 exposing (Vec2)
import Math.Vector3 as Vector3 exposing (Vec3)
import Math.Vector4 as Vector4 exposing (Vec4)
import Math.Matrix4 as Matrix4 exposing (Mat4)
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest
import Color exposing (Color)


type alias PositionOnly =
    { vertexPosition : Vec3
    }


type alias WithNormals =
    { vertexPosition : Vec3
    , vertexNormal : Vec3
    }


type alias WithNormalsAndTextureCoordinates =
    { vertexPosition : Vec3
    , vertexNormal : Vec3
    , vertexTextureCoordinates : Vec2
    }


vertexPosition : Point3d -> PositionOnly
vertexPosition point =
    { vertexPosition = Point3d.toVec3 point }


lineSegment : LineSegment3d -> ( PositionOnly, PositionOnly )
lineSegment lineSegment_ =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment_
    in
        ( vertexPosition p1, vertexPosition p2 )


triangle : Triangle3d -> ( PositionOnly, PositionOnly, PositionOnly )
triangle triangle_ =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle_
    in
        ( vertexPosition p1, vertexPosition p2, vertexPosition p3 )


triangles : List Triangle3d -> WebGL.Mesh PositionOnly
triangles triangles_ =
    WebGL.triangles (List.map triangle triangles_)


triangleWithNormals : Triangle3d -> ( WithNormals, WithNormals, WithNormals )
triangleWithNormals triangle =
    let
        normalVector =
            case Triangle3d.normalDirection triangle of
                Just direction ->
                    Direction3d.toVec3 direction

                Nothing ->
                    Vector3.vec3 0 0 0

        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
        ( { vertexPosition = Point3d.toVec3 p1, vertexNormal = normalVector }
        , { vertexPosition = Point3d.toVec3 p2, vertexNormal = normalVector }
        , { vertexPosition = Point3d.toVec3 p3, vertexNormal = normalVector }
        )


triangleFan : List Point3d -> WebGL.Mesh PositionOnly
triangleFan points =
    WebGL.triangleFan (List.map vertexPosition points)


trianglesWithNormals : List Triangle3d -> WebGL.Mesh WithNormals
trianglesWithNormals triangles =
    WebGL.triangles (List.map triangleWithNormals triangles)


mesh : List Point3d -> List ( Int, Int, Int ) -> WebGL.Mesh PositionOnly
mesh vertices faces =
    WebGL.indexedTriangles (List.map vertexPosition vertices) faces


positionAndNormal : ( Point3d, Direction3d ) -> WithNormals
positionAndNormal ( point, normalDirection ) =
    { vertexPosition = Point3d.toVec3 point
    , vertexNormal = Direction3d.toVec3 normalDirection
    }


meshWithNormals : List ( Point3d, Direction3d ) -> List ( Int, Int, Int ) -> WebGL.Mesh WithNormals
meshWithNormals vertices faces =
    WebGL.indexedTriangles (List.map positionAndNormal vertices) faces


lines : List LineSegment3d -> WebGL.Mesh PositionOnly
lines lineSegments =
    WebGL.lines (List.map lineSegment lineSegments)


polyline : Polyline3d -> WebGL.Mesh PositionOnly
polyline polyline_ =
    WebGL.lineStrip (List.map vertexPosition (Polyline3d.vertices polyline_))


points : List Point3d -> WebGL.Mesh PositionOnly
points points_ =
    WebGL.points (List.map vertexPosition points_)


type Entity
    = Colored (WebGL.Mesh PositionOnly) Color


type Node
    = Leaf Frame3d Entity
    | Group Frame3d (List Node)


leafNode : Entity -> Node
leafNode entity =
    Leaf Frame3d.xyz entity


colored : Color -> WebGL.Mesh PositionOnly -> Node
colored color mesh =
    leafNode (Colored mesh color)


group : List Node -> Node
group nodes =
    Group Frame3d.xyz nodes


transformBy : (Frame3d -> Frame3d) -> Node -> Node
transformBy frameTransformation node =
    case node of
        Leaf frame entity ->
            Leaf (frameTransformation frame) entity

        Group frame nodes ->
            Group (frameTransformation frame) nodes


rotateAround : Axis3d -> Float -> Node -> Node
rotateAround axis angle node =
    transformBy (Frame3d.rotateAround axis angle) node


translateBy : Vector3d -> Node -> Node
translateBy displacement node =
    transformBy (Frame3d.translateBy displacement) node


mirrorAcross : Plane3d -> Node -> Node
mirrorAcross plane node =
    transformBy (Frame3d.mirrorAcross plane) node


relativeTo : Frame3d -> Node -> Node
relativeTo frame node =
    transformBy (Frame3d.relativeTo frame) node


placeIn : Frame3d -> Node -> Node
placeIn frame node =
    transformBy (Frame3d.placeIn frame) node


type Projection
    = Projection Mat4


perspectiveProjection : { verticalFov : Float, aspectRatio : Float, zNear : Float, zFar : Float } -> Projection
perspectiveProjection { verticalFov, aspectRatio, zNear, zFar } =
    let
        fovInDegrees =
            verticalFov / degrees 1
    in
        Projection (Matrix4.makePerspective fovInDegrees aspectRatio zNear zFar)


orthographicProjection : { height : Float, aspectRatio : Float, zNear : Float, zFar : Float } -> Projection
orthographicProjection { height, aspectRatio, zNear, zFar } =
    let
        width =
            aspectRatio * height

        left =
            -width / 2

        right =
            width / 2

        bottom =
            -height / 2

        top =
            height / 2
    in
        Projection (Matrix4.makeOrtho left right bottom top zNear zFar)


type alias Camera =
    { eyeFrame : Frame3d
    , projectionMatrix : Mat4
    }


toEntity : Camera -> Frame3d -> Entity -> WebGL.Entity
toEntity camera modelFrame entity =
    let
        modelMatrix =
            Frame3d.modelMatrix modelFrame

        modelViewMatrix =
            Frame3d.modelViewMatrix camera.eyeFrame modelFrame

        modelViewProjectionMatrix =
            Matrix4.mul camera.projectionMatrix modelViewMatrix
    in
        case entity of
            Colored mesh color ->
                let
                    cullSetting =
                        if Frame3d.isRightHanded modelFrame then
                            WebGL.Settings.back
                        else
                            WebGL.Settings.front

                    settings =
                        [ WebGL.Settings.DepthTest.default
                        , WebGL.Settings.cullFace cullSetting
                        ]

                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewMatrix = modelViewMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , color = Color.toVec4 color
                        }
                in
                    WebGL.entityWith settings
                        Shader.positionOnlyVertexShader
                        Shader.solidColorShader
                        mesh
                        uniforms


collectEntities : Camera -> Frame3d -> Node -> List WebGL.Entity -> List WebGL.Entity
collectEntities camera placementFrame node accumulated =
    case node of
        Leaf frame entity ->
            let
                modelFrame =
                    Frame3d.placeIn placementFrame frame
            in
                toEntity camera modelFrame entity :: accumulated

        Group frame nodes ->
            let
                localFrame =
                    Frame3d.placeIn placementFrame frame
            in
                List.foldl
                    (\childNode accumulated ->
                        collectEntities camera localFrame childNode accumulated
                    )
                    accumulated
                    nodes


toEntities : Frame3d -> Projection -> Node -> List WebGL.Entity
toEntities eyeFrame projection rootNode =
    let
        (Projection projectionMatrix) =
            projection

        camera =
            { eyeFrame = eyeFrame
            , projectionMatrix = projectionMatrix
            }
    in
        collectEntities camera Frame3d.xyz rootNode []
