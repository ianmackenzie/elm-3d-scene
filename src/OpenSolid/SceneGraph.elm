module OpenSolid.SceneGraph
    exposing
        ( Geometry
        , VertexPosition
        , VertexPositionAnd
        , VertexNormal
        , VertexNormalAnd
        , VertexTextureCoordinates
        , VertexTextureCoordinatesAnd
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
import OpenSolid.BoundingBox3d as BoundingBox3d
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


type Geometry a
    = Geometry (Maybe BoundingBox3d) (WebGL.Mesh a)


type alias VertexPosition =
    VertexPositionAnd {}


type alias VertexPositionAnd a =
    { a | vertexPosition : Vec3 }


type alias VertexNormal =
    VertexNormalAnd {}


type alias VertexNormalAnd a =
    { a | vertexNormal : Vec3 }


type alias VertexTextureCoordinates =
    VertexTextureCoordinatesAnd {}


type alias VertexTextureCoordinatesAnd a =
    { a | vertexTextureCoordinates : Vec2 }


vertexPosition : Point3d -> VertexPosition
vertexPosition point =
    { vertexPosition = Point3d.toVec3 point }


lineSegmentPositions : LineSegment3d -> ( VertexPosition, VertexPosition )
lineSegmentPositions lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
        ( vertexPosition p1, vertexPosition p2 )


trianglePositions : Triangle3d -> ( VertexPosition, VertexPosition, VertexPosition )
trianglePositions triangle_ =
    let
        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle_
    in
        ( vertexPosition p1, vertexPosition p2, vertexPosition p3 )


trianglePositionsAndNormals : Triangle3d -> ( VertexPositionAnd VertexNormal, VertexPositionAnd VertexNormal, VertexPositionAnd VertexNormal )
trianglePositionsAndNormals triangle =
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


trianglesWith : (Triangle3d -> ( a, a, a )) -> List Triangle3d -> Geometry a
trianglesWith toAttributes triangles =
    let
        boundingBox =
            BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles)

        mesh =
            WebGL.triangles (List.map toAttributes triangles)
    in
        Geometry boundingBox mesh


triangles : List Triangle3d -> Geometry VertexPosition
triangles =
    trianglesWith trianglePositions


trianglesWithNormals : List Triangle3d -> Geometry (VertexPositionAnd VertexNormal)
trianglesWithNormals =
    trianglesWith trianglePositionsAndNormals


triangleFan : List Point3d -> Geometry VertexPosition
triangleFan points =
    let
        boundingBox =
            BoundingBox3d.containing points

        mesh =
            WebGL.triangleFan (List.map vertexPosition points)
    in
        Geometry boundingBox mesh


mesh : List Point3d -> List ( Int, Int, Int ) -> Geometry VertexPosition
mesh vertices faces =
    let
        boundingBox =
            BoundingBox3d.containing vertices

        vertexPositions =
            List.map vertexPosition vertices

        mesh =
            WebGL.indexedTriangles vertexPositions faces
    in
        Geometry boundingBox mesh


vertexPositionAndNormal : ( Point3d, Direction3d ) -> VertexPositionAnd VertexNormal
vertexPositionAndNormal ( point, normalDirection ) =
    { vertexPosition = Point3d.toVec3 point
    , vertexNormal = Direction3d.toVec3 normalDirection
    }


meshWithNormals : List ( Point3d, Direction3d ) -> List ( Int, Int, Int ) -> Geometry (VertexPositionAnd VertexNormal)
meshWithNormals vertices faces =
    let
        boundingBox =
            BoundingBox3d.containing (List.map Tuple.first vertices)

        attributes =
            List.map vertexPositionAndNormal vertices

        mesh =
            WebGL.indexedTriangles attributes faces
    in
        Geometry boundingBox mesh


lines : List LineSegment3d -> Geometry VertexPosition
lines lineSegments =
    let
        segmentBoundingBoxes =
            List.map LineSegment3d.boundingBox lineSegments

        boundingBox =
            BoundingBox3d.hullOf segmentBoundingBoxes

        mesh =
            WebGL.lines (List.map lineSegmentPositions lineSegments)
    in
        Geometry boundingBox mesh


polyline : Polyline3d -> Geometry VertexPosition
polyline polyline_ =
    let
        boundingBox =
            Polyline3d.boundingBox polyline_

        vertexPositions =
            List.map vertexPosition (Polyline3d.vertices polyline_)

        mesh =
            WebGL.lineStrip vertexPositions
    in
        Geometry boundingBox mesh


points : List Point3d -> Geometry VertexPosition
points points_ =
    let
        boundingBox =
            BoundingBox3d.containing points_

        mesh =
            WebGL.points (List.map vertexPosition points_)
    in
        Geometry boundingBox mesh


type Drawable
    = Colored Color (Geometry VertexPosition)


type Node
    = Leaf Frame3d Drawable
    | Group Frame3d (List Node)


leafNode : Drawable -> Node
leafNode drawable =
    Leaf Frame3d.xyz drawable


colored : Color -> Geometry VertexPosition -> Node
colored color geometry =
    leafNode (Colored color geometry)


group : List Node -> Node
group nodes =
    Group Frame3d.xyz nodes


transformBy : (Frame3d -> Frame3d) -> Node -> Node
transformBy frameTransformation node =
    case node of
        Leaf frame drawable ->
            Leaf (frameTransformation frame) drawable

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


toEntity : Camera -> Frame3d -> Drawable -> WebGL.Entity
toEntity camera modelFrame drawable =
    let
        modelMatrix =
            Frame3d.modelMatrix modelFrame

        modelViewMatrix =
            Frame3d.modelViewMatrix camera.eyeFrame modelFrame

        modelViewProjectionMatrix =
            Matrix4.mul camera.projectionMatrix modelViewMatrix

        cullSetting =
            if Frame3d.isRightHanded modelFrame then
                WebGL.Settings.back
            else
                WebGL.Settings.front

        settings =
            [ WebGL.Settings.DepthTest.default
            , WebGL.Settings.cullFace cullSetting
            ]
    in
        case drawable of
            Colored color (Geometry boundingBox mesh) ->
                let
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
        Leaf frame drawable ->
            let
                modelFrame =
                    Frame3d.placeIn placementFrame frame
            in
                toEntity camera modelFrame drawable :: accumulated

        Group frame childNodes ->
            let
                localFrame =
                    Frame3d.placeIn placementFrame frame
            in
                List.foldl
                    (\childNode accumulated ->
                        collectEntities camera localFrame childNode accumulated
                    )
                    accumulated
                    childNodes


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
