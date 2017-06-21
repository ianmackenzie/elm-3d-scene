module OpenSolid.Scene
    exposing
        ( toEntities
        )

import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Scene.Light exposing (Light)
import OpenSolid.Scene.Node exposing (Node)
import OpenSolid.Scene.Shader as Shader
import OpenSolid.Scene.Types as Types
import OpenSolid.WebGL.Camera as Camera exposing (Camera)
import OpenSolid.WebGL.Frame3d as Frame3d
import OpenSolid.WebGL.Point3d as Point3d
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest


type alias AmbientProperties =
    { color : Vec3
    , lookupTexture : WebGL.Texture
    }


type alias LightProperties =
    { lightType : Int
    , lightColor : Vec3
    , lightVector : Vec3
    , lightRadius : Float
    }


type alias MaterialProperties =
    { baseColor : Vec3
    , roughness : Float
    , metallic : Float
    }


type alias PhysicallyBasedRenderer =
    List WebGL.Settings.Setting
    -> Vec3
    -> Mat4
    -> Mat4
    -> MaterialProperties
    -> WebGL.Mesh { vertexPosition : Vec3, vertexNormal : Vec3 }
    -> WebGL.Entity


type PhysicallyBasedLighting
    = AmbientOnlyLighting AmbientProperties
    | AmbientLighting1 AmbientProperties LightProperties
    | AmbientLighting2 AmbientProperties LightProperties LightProperties
    | AmbientLighting3 AmbientProperties LightProperties LightProperties LightProperties
    | AmbientLighting4 AmbientProperties LightProperties LightProperties LightProperties LightProperties
    | NoAmbientLighting1 LightProperties
    | NoAmbientLighting2 LightProperties LightProperties
    | NoAmbientLighting3 LightProperties LightProperties LightProperties
    | NoAmbientLighting4 LightProperties LightProperties LightProperties LightProperties
    | DummyLighting


type alias RenderProperties =
    { cameraFrame : Frame3d
    , eyePoint : Vec3
    , projectionMatrix : Mat4
    , physicallyBasedRenderer : PhysicallyBasedRenderer
    }


physicallyBasedLighting : List Light -> PhysicallyBasedLighting
physicallyBasedLighting lights =
    let
        updateLightingState light currentState =
            case light of
                Types.AmbientLight ambientLight ->
                    { currentState
                        | ambientLightColor =
                            Vector3.add ambientLight.color
                                currentState.ambientLightColor
                        , ambientLookupTexture = Just ambientLight.lookupTexture
                    }

                Types.DirectionalLight directionalLight ->
                    let
                        thisLight =
                            { lightType = 1
                            , lightColor = directionalLight.color
                            , lightVector = directionalLight.direction
                            , lightRadius = 0
                            }
                    in
                    { currentState
                        | lights = thisLight :: currentState.lights
                    }

        initialLightingState =
            { ambientLightColor = Vector3.vec3 0 0 0
            , ambientLookupTexture = Nothing
            , lights = []
            }

        lightingState =
            List.foldl updateLightingState initialLightingState lights
    in
    case lightingState.ambientLookupTexture of
        Just lookupTexture ->
            let
                ambientProperties =
                    { color = lightingState.ambientLightColor
                    , lookupTexture = lookupTexture
                    }
            in
            case lightingState.lights of
                [] ->
                    AmbientOnlyLighting ambientProperties

                [ light1 ] ->
                    AmbientLighting1 ambientProperties light1

                [ light1, light2 ] ->
                    AmbientLighting2 ambientProperties light1 light2

                [ light1, light2, light3 ] ->
                    AmbientLighting3 ambientProperties light1 light2 light3

                [ light1, light2, light3, light4 ] ->
                    AmbientLighting4 ambientProperties light1 light2 light3 light4

                _ ->
                    DummyLighting

        Nothing ->
            case lightingState.lights of
                [] ->
                    DummyLighting

                [ light1 ] ->
                    NoAmbientLighting1 light1

                [ light1, light2 ] ->
                    NoAmbientLighting2 light1 light2

                [ light1, light2, light3 ] ->
                    NoAmbientLighting3 light1 light2 light3

                [ light1, light2, light3, light4 ] ->
                    NoAmbientLighting4 light1 light2 light3 light4

                _ ->
                    DummyLighting


physicallyBasedRendererFor : List Light -> PhysicallyBasedRenderer
physicallyBasedRendererFor lights =
    case physicallyBasedLighting lights of
        AmbientOnlyLighting ambientProperties ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , ambientLightColor = ambientProperties.color
                        , ambientLookupTexture = ambientProperties.lookupTexture
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.ambientOnly mesh uniforms

        AmbientLighting1 ambientProperties light1 ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , ambientLightColor = ambientProperties.color
                        , ambientLookupTexture = ambientProperties.lookupTexture
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.ambient1 mesh uniforms

        AmbientLighting2 ambientProperties light1 light2 ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , ambientLightColor = ambientProperties.color
                        , ambientLookupTexture = ambientProperties.lookupTexture
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        , lightType2 = light2.lightType
                        , lightColor2 = light2.lightColor
                        , lightVector2 = light2.lightVector
                        , lightRadius2 = light2.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.ambient2 mesh uniforms

        AmbientLighting3 ambientProperties light1 light2 light3 ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , ambientLightColor = ambientProperties.color
                        , ambientLookupTexture = ambientProperties.lookupTexture
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        , lightType2 = light2.lightType
                        , lightColor2 = light2.lightColor
                        , lightVector2 = light2.lightVector
                        , lightRadius2 = light2.lightRadius
                        , lightType3 = light3.lightType
                        , lightColor3 = light3.lightColor
                        , lightVector3 = light3.lightVector
                        , lightRadius3 = light3.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.ambient3 mesh uniforms

        AmbientLighting4 ambientProperties light1 light2 light3 light4 ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , ambientLightColor = ambientProperties.color
                        , ambientLookupTexture = ambientProperties.lookupTexture
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        , lightType2 = light2.lightType
                        , lightColor2 = light2.lightColor
                        , lightVector2 = light2.lightVector
                        , lightRadius2 = light2.lightRadius
                        , lightType3 = light3.lightType
                        , lightColor3 = light3.lightColor
                        , lightVector3 = light3.lightVector
                        , lightRadius3 = light3.lightRadius
                        , lightType4 = light4.lightType
                        , lightColor4 = light4.lightColor
                        , lightVector4 = light4.lightVector
                        , lightRadius4 = light4.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.ambient4 mesh uniforms

        NoAmbientLighting1 light1 ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.noAmbient1 mesh uniforms

        NoAmbientLighting2 light1 light2 ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        , lightType2 = light2.lightType
                        , lightColor2 = light2.lightColor
                        , lightVector2 = light2.lightVector
                        , lightRadius2 = light2.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.noAmbient2 mesh uniforms

        NoAmbientLighting3 light1 light2 light3 ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        , lightType2 = light2.lightType
                        , lightColor2 = light2.lightColor
                        , lightVector2 = light2.lightVector
                        , lightRadius2 = light2.lightRadius
                        , lightType3 = light3.lightType
                        , lightColor3 = light3.lightColor
                        , lightVector3 = light3.lightVector
                        , lightRadius3 = light3.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.noAmbient3 mesh uniforms

        NoAmbientLighting4 light1 light2 light3 light4 ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , baseColor = material.baseColor
                        , roughness = material.roughness
                        , metallic = material.metallic
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        , lightType2 = light2.lightType
                        , lightColor2 = light2.lightColor
                        , lightVector2 = light2.lightVector
                        , lightRadius2 = light2.lightRadius
                        , lightType3 = light3.lightType
                        , lightColor3 = light3.lightColor
                        , lightVector3 = light3.lightVector
                        , lightRadius3 = light3.lightRadius
                        , lightType4 = light4.lightType
                        , lightColor4 = light4.lightColor
                        , lightVector4 = light4.lightVector
                        , lightRadius4 = light4.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.noAmbient4 mesh uniforms

        DummyLighting ->
            \settings eyePoint modelMatrix modelViewProjectionMatrix material mesh ->
                let
                    uniforms =
                        { modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        }
                in
                WebGL.entityWith settings Shader.vertex Shader.dummy mesh uniforms


toEntity : RenderProperties -> Frame3d -> Types.Drawable -> WebGL.Entity
toEntity renderProperties modelFrame drawable =
    let
        modelMatrix =
            Frame3d.modelMatrix modelFrame

        modelViewMatrix =
            Frame3d.modelViewMatrix renderProperties.cameraFrame modelFrame

        projectionMatrix =
            renderProperties.projectionMatrix

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
                (Types.SimpleGeometry boundingBox mesh) =
                    geometry

                uniforms =
                    { modelMatrix = modelMatrix
                    , modelViewProjectionMatrix = modelViewProjectionMatrix
                    , color = color
                    }
            in
            WebGL.entityWith settings
                Shader.simpleVertex
                Shader.simple
                mesh
                uniforms

        Types.ShadedGeometry material geometry ->
            let
                (Types.PhysicallyBasedMaterial materialProperties) =
                    material

                (Types.Geometry boundingBox mesh) =
                    geometry
            in
            renderProperties.physicallyBasedRenderer
                settings
                renderProperties.eyePoint
                modelMatrix
                modelViewProjectionMatrix
                materialProperties
                mesh


collectEntities : RenderProperties -> Frame3d -> Node -> List WebGL.Entity -> List WebGL.Entity
collectEntities renderProperties placementFrame node accumulated =
    case node of
        Types.LeafNode frame drawable ->
            let
                modelFrame =
                    Frame3d.placeIn placementFrame frame

                entity =
                    toEntity renderProperties modelFrame drawable
            in
            entity :: accumulated

        Types.GroupNode frame childNodes ->
            let
                localFrame =
                    Frame3d.placeIn placementFrame frame

                accumulate childNode accumulated =
                    collectEntities renderProperties
                        localFrame
                        childNode
                        accumulated
            in
            List.foldl accumulate accumulated childNodes


toEntities : List Light -> Camera -> Node -> List WebGL.Entity
toEntities lights camera rootNode =
    let
        cameraFrame =
            Camera.frame camera

        renderProperties =
            { cameraFrame = cameraFrame
            , eyePoint = Point3d.toVec3 (Frame3d.originPoint cameraFrame)
            , projectionMatrix = Camera.projectionMatrix camera
            , physicallyBasedRenderer = physicallyBasedRendererFor lights
            }
    in
    collectEntities renderProperties Frame3d.xyz rootNode []
