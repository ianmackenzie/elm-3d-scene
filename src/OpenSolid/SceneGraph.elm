module OpenSolid.SceneGraph
    exposing
        ( Geometry
        , triangles
        , trianglesWithNormals
        , indexedTriangles
        , indexedTrianglesWithNormals
        , triangleFan
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
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Color as Color
import OpenSolid.WebGL.Triangle3d as Triangle3d
import OpenSolid.WebGL.LineSegment3d as LineSegment3d
import OpenSolid.WebGL.Polyline3d as Polyline3d
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


trianglesWith : (Triangle3d -> ( a, a, a )) -> List Triangle3d -> Geometry a
trianglesWith toAttributes triangles =
    let
        boundingBox =
            BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles)

        mesh =
            WebGL.triangles (List.map toAttributes triangles)
    in
        Geometry boundingBox mesh


triangles : List Triangle3d -> Geometry { vertexPosition : Vec3 }
triangles =
    trianglesWith Triangle3d.vertexPositions


trianglesWithNormals : List Triangle3d -> Geometry { vertexPosition : Vec3, vertexNormal : Vec3 }
trianglesWithNormals =
    trianglesWith Triangle3d.vertexPositionsAndNormals


indexedTriangles : List Point3d -> List ( Int, Int, Int ) -> Geometry { vertexPosition : Vec3 }
indexedTriangles vertices faces =
    let
        boundingBox =
            BoundingBox3d.containing vertices

        vertexPositions =
            List.map Point3d.toVertexPosition vertices

        mesh =
            WebGL.indexedTriangles vertexPositions faces
    in
        Geometry boundingBox mesh


indexedTrianglesWithNormals : List ( Point3d, Direction3d ) -> List ( Int, Int, Int ) -> Geometry { vertexPosition : Vec3, vertexNormal : Vec3 }
indexedTrianglesWithNormals vertices faces =
    let
        vertexPoints =
            List.map Tuple.first vertices

        boundingBox =
            BoundingBox3d.containing vertexPoints

        toAttributes ( point, normalDirection ) =
            { vertexPosition = Point3d.toVec3 point
            , vertexNormal = Direction3d.toVec3 normalDirection
            }

        vertexAttributes =
            List.map toAttributes vertices

        mesh =
            WebGL.indexedTriangles vertexAttributes faces
    in
        Geometry boundingBox mesh


triangleFan : List Point3d -> Geometry { vertexPosition : Vec3 }
triangleFan points =
    let
        boundingBox =
            BoundingBox3d.containing points

        mesh =
            WebGL.triangleFan (List.map Point3d.toVertexPosition points)
    in
        Geometry boundingBox mesh


lines : List LineSegment3d -> Geometry { vertexPosition : Vec3 }
lines lineSegments =
    let
        segmentBoundingBoxes =
            List.map LineSegment3d.boundingBox lineSegments

        boundingBox =
            BoundingBox3d.hullOf segmentBoundingBoxes

        mesh =
            WebGL.lines (List.map LineSegment3d.vertexPositions lineSegments)
    in
        Geometry boundingBox mesh


polyline : Polyline3d -> Geometry { vertexPosition : Vec3 }
polyline polyline_ =
    let
        boundingBox =
            Polyline3d.boundingBox polyline_

        mesh =
            WebGL.lineStrip (Polyline3d.vertexPositions polyline_)
    in
        Geometry boundingBox mesh


points : List Point3d -> Geometry { vertexPosition : Vec3 }
points points_ =
    let
        boundingBox =
            BoundingBox3d.containing points_

        mesh =
            WebGL.points (List.map Point3d.toVertexPosition points_)
    in
        Geometry boundingBox mesh


type Drawable
    = Colored Color (Geometry { vertexPosition : Vec3 })


type Node
    = Leaf Frame3d Drawable
    | Group Frame3d (List Node)


leafNode : Drawable -> Node
leafNode drawable =
    Leaf Frame3d.xyz drawable


colored : Color -> Geometry { vertexPosition : Vec3 } -> Node
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
