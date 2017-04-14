module OpenSolid.SceneGraph
    exposing
        ( Mesh
        , Lines
        , Points
        , triangles
        , mesh
        , lines
        , polyline
        , points
        , Node
        , meshWith
        , linesWith
        , pointsWith
        , group
        , rotateAround
        , translateBy
        , mirrorAcross
        , placeIn
        , Projection
        , perspectiveProjection
        , orthographicProjection
        , toEntities
        )

import OpenSolid.SceneGraph.Internal.Shader as Shader
import OpenSolid.SceneGraph.Internal.Material as Material
import OpenSolid.SceneGraph.Material as Material exposing (Material)
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Color as Color
import Math.Vector3 as Vector3 exposing (Vec3)
import Math.Vector4 as Vector4 exposing (Vec4)
import Math.Matrix4 as Matrix4 exposing (Mat4)
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest
import Color exposing (Color)


type Mesh
    = Mesh (WebGL.Mesh Shader.MeshAttributes)


type Lines
    = Lines (WebGL.Mesh Shader.VertexPosition)


type Points
    = Points (WebGL.Mesh Shader.VertexPosition)


triangleAttributes : Triangle3d -> ( Shader.MeshAttributes, Shader.MeshAttributes, Shader.MeshAttributes )
triangleAttributes triangle =
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


triangles : List Triangle3d -> Mesh
triangles triangles_ =
    Mesh (WebGL.triangles (List.map triangleAttributes triangles_))


meshVertexAttributes : ( Point3d, Direction3d ) -> Shader.MeshAttributes
meshVertexAttributes ( point, normalDirection ) =
    { vertexPosition = Point3d.toVec3 point
    , vertexNormal = Direction3d.toVec3 normalDirection
    }


mesh : List ( Point3d, Direction3d ) -> List ( Int, Int, Int ) -> Mesh
mesh vertices faces =
    Mesh (WebGL.indexedTriangles (List.map meshVertexAttributes vertices) faces)


lineSegmentAttributes : LineSegment3d -> ( Shader.VertexPosition, Shader.VertexPosition )
lineSegmentAttributes lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
        ( { vertexPosition = Point3d.toVec3 p1 }
        , { vertexPosition = Point3d.toVec3 p2 }
        )


lines : List LineSegment3d -> Lines
lines lineSegments =
    Lines (WebGL.lines (List.map lineSegmentAttributes lineSegments))


vertexPosition : Point3d -> Shader.VertexPosition
vertexPosition point =
    { vertexPosition = Point3d.toVec3 point }


polyline : Polyline3d -> Lines
polyline polyline_ =
    let
        vertices =
            Polyline3d.vertices polyline_
    in
        Lines (WebGL.lineStrip (List.map vertexPosition vertices))


points : List Point3d -> Points
points points_ =
    Points (WebGL.points (List.map vertexPosition points_))


type Entity
    = MeshEntity Mesh Material
    | LinesEntity Lines Color
    | PointsEntity Points Color Float


type Node
    = Leaf Frame3d Entity
    | Group Frame3d (List Node)


meshWith : { material : Material } -> Mesh -> Node
meshWith { material } mesh =
    Leaf Frame3d.xyz (MeshEntity mesh material)


linesWith : { color : Color } -> Lines -> Node
linesWith { color } lines =
    Leaf Frame3d.xyz (LinesEntity lines color)


pointsWith : { color : Color, size : Float } -> Points -> Node
pointsWith { color, size } points =
    Leaf Frame3d.xyz (PointsEntity points color size)


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
            MeshEntity (Mesh mesh) material ->
                case material of
                    Material.Solid color ->
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
                                , modelViewProjectionMatrix =
                                    modelViewProjectionMatrix
                                , color = Color.toVec4 color
                                }
                        in
                            WebGL.entityWith settings
                                Shader.meshVertexShader
                                Shader.solidFragmentShader
                                mesh
                                uniforms

            LinesEntity (Lines lineMesh) color ->
                WebGL.entity
                    Shader.primitiveVertexShader
                    Shader.primitiveFragmentShader
                    lineMesh
                    { modelViewProjectionMatrix = modelViewProjectionMatrix
                    , color = Color.toVec4 color
                    , size = 1.0
                    }

            PointsEntity (Points pointMesh) color size ->
                WebGL.entity
                    Shader.primitiveVertexShader
                    Shader.primitiveFragmentShader
                    pointMesh
                    { modelViewProjectionMatrix = modelViewProjectionMatrix
                    , color = Color.toVec4 color
                    , size = size
                    }


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
