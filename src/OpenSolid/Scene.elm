module OpenSolid.Scene
    exposing
        ( Geometry
        , Light
        , Lighting
        , Material
        , Node
        , colored
        , group
        , indexedTriangles
        , indexedTrianglesWithNormals
        , lines
        , mirrorAcross
        , placeIn
        , points
        , polyline
        , relativeTo
        , rotateAround
        , shaded
        , toEntities
        , translateBy
        , triangleFan
        , triangles
        , trianglesWithNormals
        )

import Color exposing (Color)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import OpenSolid.BoundingBox3d as BoundingBox3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.Scene.Lighting as Lighting
import OpenSolid.Scene.Shader as Shader
import OpenSolid.Scene.Types as Types
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.WebGL.Camera as Camera exposing (Camera)
import OpenSolid.WebGL.Color as Color
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.LineSegment3d as LineSegment3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Polyline3d as Polyline3d
import OpenSolid.WebGL.Triangle3d as Triangle3d
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest


type alias Geometry a =
    Types.Geometry a


type alias Material =
    Types.Material


type alias Light =
    Types.Light


type alias Lighting =
    Types.Lighting


type alias Drawable =
    Types.Drawable


type alias Node =
    Types.Node


trianglesWith : (Triangle3d -> ( a, a, a )) -> List Triangle3d -> Geometry a
trianglesWith toAttributes triangles =
    let
        boundingBox =
            BoundingBox3d.hullOf (List.map Triangle3d.boundingBox triangles)

        mesh =
            WebGL.triangles (List.map toAttributes triangles)
    in
    Types.Geometry boundingBox mesh


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
    Types.Geometry boundingBox mesh


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
    Types.Geometry boundingBox mesh


triangleFan : List Point3d -> Geometry { vertexPosition : Vec3 }
triangleFan points =
    let
        boundingBox =
            BoundingBox3d.containing points

        mesh =
            WebGL.triangleFan (List.map Point3d.toVertexPosition points)
    in
    Types.Geometry boundingBox mesh


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
    Types.Geometry boundingBox mesh


polyline : Polyline3d -> Geometry { vertexPosition : Vec3 }
polyline polyline_ =
    let
        boundingBox =
            Polyline3d.boundingBox polyline_

        mesh =
            WebGL.lineStrip (Polyline3d.vertexPositions polyline_)
    in
    Types.Geometry boundingBox mesh


points : List Point3d -> Geometry { vertexPosition : Vec3 }
points points_ =
    let
        boundingBox =
            BoundingBox3d.containing points_

        mesh =
            WebGL.points (List.map Point3d.toVertexPosition points_)
    in
    Types.Geometry boundingBox mesh


leafNode : Drawable -> Node
leafNode drawable =
    Types.LeafNode Frame3d.xyz drawable


colored : Color -> Geometry { vertexPosition : Vec3 } -> Node
colored color geometry =
    leafNode (Types.ColoredGeometry color geometry)


shaded : Material -> Lighting -> Geometry { vertexPosition : Vec3, vertexNormal : Vec3 } -> Node
shaded material lighting geometry =
    leafNode (Types.ShadedGeometry material lighting geometry)


group : List Node -> Node
group nodes =
    Types.GroupNode Frame3d.xyz nodes


transformBy : (Frame3d -> Frame3d) -> Node -> Node
transformBy frameTransformation node =
    case node of
        Types.LeafNode frame drawable ->
            Types.LeafNode (frameTransformation frame) drawable

        Types.GroupNode frame nodes ->
            Types.GroupNode (frameTransformation frame) nodes


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


toEntity : Camera -> Frame3d -> Drawable -> WebGL.Entity
toEntity camera modelFrame drawable =
    let
        modelMatrix =
            Frame3d.modelMatrix modelFrame

        cameraFrame =
            Camera.frame camera

        eyePoint =
            Frame3d.originPoint cameraFrame

        modelViewMatrix =
            Frame3d.modelViewMatrix (Camera.frame camera) modelFrame

        projectionMatrix =
            Camera.projectionMatrix camera

        modelViewProjectionMatrix =
            Math.Matrix4.mul projectionMatrix modelViewMatrix

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
        Types.ColoredGeometry color geometry ->
            let
                (Types.Geometry boundingBox mesh) =
                    geometry

                uniforms =
                    { modelMatrix = modelMatrix
                    , modelViewMatrix = modelViewMatrix
                    , modelViewProjectionMatrix = modelViewProjectionMatrix
                    , color = Color.toVec3 color
                    }
            in
            WebGL.entityWith settings
                Shader.positionOnlyVertexShader
                Shader.solidColorShader
                mesh
                uniforms

        Types.ShadedGeometry material lighting geometry ->
            let
                (Types.PhysicallyBasedMaterial { baseColor, roughness, metallic }) =
                    material

                (Types.SingleLight (Types.DirectionalLight lightColor lightDirection)) =
                    lighting

                (Types.Geometry boundingBox mesh) =
                    geometry

                uniforms =
                    { modelMatrix = modelMatrix
                    , modelViewMatrix = modelViewMatrix
                    , modelViewProjectionMatrix = modelViewProjectionMatrix
                    , baseColor = Color.toVec3 baseColor
                    , roughness = roughness
                    , metallic = Shader.flag metallic
                    , lightColor = Color.toVec3 lightColor
                    , lightDirection =
                        Direction3d.toVec3 (Direction3d.flip lightDirection)
                    , eyePoint = Point3d.toVec3 eyePoint
                    }
            in
            WebGL.entityWith settings
                Shader.positionAndNormalVertexShader
                Shader.physicallyBasedDirectionalLightShader
                mesh
                uniforms


collectEntities : Camera -> Frame3d -> Node -> List WebGL.Entity -> List WebGL.Entity
collectEntities camera placementFrame node accumulated =
    case node of
        Types.LeafNode frame drawable ->
            let
                modelFrame =
                    Frame3d.placeIn placementFrame frame

                entity =
                    toEntity camera modelFrame drawable
            in
            entity :: accumulated

        Types.GroupNode frame childNodes ->
            let
                localFrame =
                    Frame3d.placeIn placementFrame frame

                accumulate childNode accumulated =
                    collectEntities camera localFrame childNode accumulated
            in
            List.foldl accumulate accumulated childNodes


toEntities : Camera -> Node -> List WebGL.Entity
toEntities camera rootNode =
    collectEntities camera Frame3d.xyz rootNode []
