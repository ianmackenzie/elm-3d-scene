module OpenSolid.Scene.Node
    exposing
        ( Node
        , colored
        , group
        , mirrorAcross
        , placeIn
        , relativeTo
        , rotateAround
        , shaded
        , toEntities
        , translateBy
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
import OpenSolid.Scene.Geometry exposing (Geometry)
import OpenSolid.Scene.Light exposing (Light)
import OpenSolid.Scene.Lighting exposing (Lighting)
import OpenSolid.Scene.Material exposing (Material)
import OpenSolid.Scene.Shader as Shader
import OpenSolid.Scene.SimpleGeometry exposing (SimpleGeometry)
import OpenSolid.Scene.Types as Types
import OpenSolid.WebGL.Camera as Camera exposing (Camera)
import OpenSolid.WebGL.Color as Color
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Point3d as Point3d
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest


type alias Node =
    Types.Node


leaf : Types.Drawable -> Node
leaf drawable =
    Types.LeafNode Frame3d.xyz drawable


colored : Color -> SimpleGeometry -> Node
colored color geometry =
    leaf (Types.ColoredGeometry (Color.toVec3 color) geometry)


shaded : Material -> Lighting -> Geometry -> Node
shaded material lighting geometry =
    leaf (Types.ShadedGeometry material lighting geometry)


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


toEntity : Camera -> Frame3d -> Types.Drawable -> WebGL.Entity
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
                    , color = color
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

                (Types.Geometry boundingBox mesh) =
                    geometry

                (Types.SingleLight light) =
                    lighting
            in
            case light of
                Types.AmbientLight ambientLight ->
                    let
                        uniforms =
                            { modelMatrix = modelMatrix
                            , modelViewMatrix = modelViewMatrix
                            , modelViewProjectionMatrix = modelViewProjectionMatrix
                            , baseColor = baseColor
                            , roughness = roughness
                            , metallic = metallic
                            , lightColor = ambientLight.color
                            , lookupTexture = ambientLight.lookupTexture
                            , eyePoint = Point3d.toVec3 eyePoint
                            }
                    in
                    WebGL.entityWith settings
                        Shader.positionAndNormalVertexShader
                        Shader.physicallyBasedAmbientLightShader
                        mesh
                        uniforms

                Types.DirectionalLight directionalLight ->
                    let
                        uniforms =
                            { modelMatrix = modelMatrix
                            , modelViewMatrix = modelViewMatrix
                            , modelViewProjectionMatrix = modelViewProjectionMatrix
                            , baseColor = baseColor
                            , roughness = roughness
                            , metallic = metallic
                            , lightColor = directionalLight.color
                            , lightDirection = directionalLight.direction
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
