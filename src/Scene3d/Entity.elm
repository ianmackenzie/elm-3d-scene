module Scene3d.Entity exposing
    ( Entity
    , Material
    , block
    , cylinder
    , empty
    , group
    , mesh
    , mirrorAcross
    , placeIn
    , quad
    , relativeTo
    , rotateAround
    , scaleAbout
    , shadow
    , sphere
    , translateBy
    , translateIn
    )

import Angle exposing (Angle)
import Array
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color)
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import Float.Extra as Float
import Frame3d exposing (Frame3d)
import Length exposing (Length, Meters)
import LineSegment3d exposing (LineSegment3d)
import Luminance exposing (Luminance)
import Math.Matrix4 exposing (Mat4)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity(..), Unitless)
import Scene3d.Chromaticity as Chromaticity exposing (Chromaticity)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh exposing (Mesh, Shadow)
import Scene3d.Primitives as Primitives
import Scene3d.Shaders as Shaders
import Scene3d.Transformation as Transformation exposing (Transformation)
import Scene3d.Types as Types
    exposing
        ( BackFaceSetting(..)
        , Bounds
        , LinearRgb(..)
        , Material(..)
        , Node(..)
        , NormalMap(..)
        , PlainVertex
        , Texture(..)
        , VertexWithNormal
        , VertexWithUv
        )
import Sphere3d exposing (Sphere3d)
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL
import WebGL.Settings
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture


type alias Entity coordinates =
    Types.Entity coordinates


type alias Material coordinates attributes =
    Types.Material coordinates attributes


empty : Entity coordinates
empty =
    Types.Entity EmptyNode


mesh : Material coordinates attributes -> Mesh coordinates attributes -> Entity coordinates
mesh givenMaterial givenMesh =
    case givenMaterial of
        Types.UnlitMaterial (Types.Constant color) ->
            case givenMesh of
                Types.EmptyMesh ->
                    empty

                Types.Triangles _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.Facets _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.Indexed _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.MeshWithNormals _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.MeshWithUvs _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.MeshWithNormalsAndUvs _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.MeshWithTangents _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.LineSegments _ _ webGLMesh ->
                    constantMesh color webGLMesh KeepBackFaces

                Types.Polyline _ _ webGLMesh ->
                    constantMesh color webGLMesh KeepBackFaces

                Types.Points _ radius _ webGLMesh ->
                    constantPointMesh color radius webGLMesh

        Types.UnlitMaterial (Types.Texture { data }) ->
            case givenMesh of
                Types.EmptyMesh ->
                    empty

                Types.Triangles _ _ webGLMesh backFaceSetting ->
                    empty

                Types.Facets _ _ webGLMesh backFaceSetting ->
                    empty

                Types.Indexed _ _ webGLMesh backFaceSetting ->
                    empty

                Types.MeshWithNormals _ _ webGLMesh backFaceSetting ->
                    empty

                Types.MeshWithUvs _ _ webGLMesh backFaceSetting ->
                    colorTextureMesh data webGLMesh backFaceSetting

                Types.MeshWithNormalsAndUvs _ _ webGLMesh backFaceSetting ->
                    colorTextureMesh data webGLMesh backFaceSetting

                Types.MeshWithTangents _ _ webGLMesh backFaceSetting ->
                    colorTextureMesh data webGLMesh backFaceSetting

                Types.LineSegments _ _ webGLMesh ->
                    empty

                Types.Polyline _ _ webGLMesh ->
                    empty

                Types.Points _ radius _ webGLMesh ->
                    empty

        Types.EmissiveMaterial (Types.Constant (LinearRgb emissiveColor)) backlight ->
            case givenMesh of
                Types.EmptyMesh ->
                    empty

                Types.Triangles _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor backlight webGLMesh backFaceSetting

                Types.Facets _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor backlight webGLMesh backFaceSetting

                Types.Indexed _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor backlight webGLMesh backFaceSetting

                Types.MeshWithNormals _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor backlight webGLMesh backFaceSetting

                Types.MeshWithUvs _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor backlight webGLMesh backFaceSetting

                Types.MeshWithNormalsAndUvs _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor backlight webGLMesh backFaceSetting

                Types.MeshWithTangents _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor backlight webGLMesh backFaceSetting

                Types.LineSegments _ _ webGLMesh ->
                    emissiveMesh emissiveColor backlight webGLMesh KeepBackFaces

                Types.Polyline _ _ webGLMesh ->
                    emissiveMesh emissiveColor backlight webGLMesh KeepBackFaces

                Types.Points _ radius _ webGLMesh ->
                    emissivePointMesh emissiveColor radius webGLMesh

        Types.EmissiveMaterial (Types.Texture { data }) backlight ->
            case givenMesh of
                Types.EmptyMesh ->
                    empty

                Types.Triangles _ _ webGLMesh backFaceSetting ->
                    empty

                Types.Facets _ _ webGLMesh backFaceSetting ->
                    empty

                Types.Indexed _ _ webGLMesh backFaceSetting ->
                    empty

                Types.MeshWithNormals _ _ webGLMesh backFaceSetting ->
                    empty

                Types.MeshWithUvs _ _ webGLMesh backFaceSetting ->
                    texturedEmissiveMesh data backlight webGLMesh backFaceSetting

                Types.MeshWithNormalsAndUvs _ _ webGLMesh backFaceSetting ->
                    texturedEmissiveMesh data backlight webGLMesh backFaceSetting

                Types.MeshWithTangents _ _ webGLMesh backFaceSetting ->
                    texturedEmissiveMesh data backlight webGLMesh backFaceSetting

                Types.LineSegments _ _ webGLMesh ->
                    empty

                Types.Polyline _ _ webGLMesh ->
                    empty

                Types.Points _ radius _ webGLMesh ->
                    empty

        Types.LambertianMaterial materialColorTexture normalMapTexture ->
            case resolveLambertian materialColorTexture normalMapTexture of
                ConstantLambertianMaterial (LinearRgb materialColor) ->
                    case givenMesh of
                        Types.EmptyMesh ->
                            empty

                        Types.Triangles _ _ _ _ ->
                            empty

                        Types.Facets _ _ webGLMesh cullBackFaces ->
                            lambertianMesh materialColor webGLMesh cullBackFaces

                        Types.Indexed _ _ _ _ ->
                            empty

                        Types.MeshWithNormals _ _ webGLMesh cullBackFaces ->
                            lambertianMesh materialColor webGLMesh cullBackFaces

                        Types.MeshWithUvs _ _ _ _ ->
                            empty

                        Types.MeshWithNormalsAndUvs _ _ webGLMesh cullBackFaces ->
                            lambertianMesh materialColor webGLMesh cullBackFaces

                        Types.MeshWithTangents _ _ webGLMesh cullBackFaces ->
                            lambertianMesh materialColor webGLMesh cullBackFaces

                        Types.LineSegments _ _ _ ->
                            empty

                        Types.Polyline _ _ _ ->
                            empty

                        Types.Points _ _ _ _ ->
                            empty

                TexturedLambertianMaterial ( materialColorData, constantMaterialColor ) ( normalMapData, useNormalMap ) ->
                    case givenMesh of
                        Types.EmptyMesh ->
                            empty

                        Types.Triangles _ _ _ _ ->
                            empty

                        Types.Facets _ _ webGLMesh cullBackFaces ->
                            empty

                        Types.Indexed _ _ _ _ ->
                            empty

                        Types.MeshWithNormals _ _ webGLMesh cullBackFaces ->
                            empty

                        Types.MeshWithUvs _ _ _ _ ->
                            empty

                        Types.MeshWithNormalsAndUvs _ _ webGLMesh cullBackFaces ->
                            texturedLambertianMesh materialColorData webGLMesh cullBackFaces

                        Types.MeshWithTangents _ _ webGLMesh cullBackFaces ->
                            normalMappedLambertianMesh materialColorData normalMapData useNormalMap webGLMesh cullBackFaces

                        Types.LineSegments _ _ _ ->
                            empty

                        Types.Polyline _ _ _ ->
                            empty

                        Types.Points _ _ _ _ ->
                            empty

        Types.PbrMaterial baseColorTexture roughnessTexture metallicTexture normalMapTexture ->
            case resolvePbr baseColorTexture roughnessTexture metallicTexture normalMapTexture of
                ConstantPbrMaterial (LinearRgb baseColor) roughness metallic ->
                    case givenMesh of
                        Types.EmptyMesh ->
                            empty

                        Types.Triangles _ _ _ _ ->
                            empty

                        Types.Facets _ _ webGLMesh backFaceSetting ->
                            physicalMesh
                                baseColor
                                roughness
                                metallic
                                webGLMesh
                                backFaceSetting

                        Types.Indexed _ _ _ _ ->
                            empty

                        Types.MeshWithNormals _ _ webGLMesh backFaceSetting ->
                            physicalMesh
                                baseColor
                                roughness
                                metallic
                                webGLMesh
                                backFaceSetting

                        Types.MeshWithUvs _ _ _ _ ->
                            empty

                        Types.MeshWithNormalsAndUvs _ _ webGLMesh backFaceSetting ->
                            physicalMesh
                                baseColor
                                roughness
                                metallic
                                webGLMesh
                                backFaceSetting

                        Types.MeshWithTangents _ _ webGLMesh backFaceSetting ->
                            physicalMesh
                                baseColor
                                roughness
                                metallic
                                webGLMesh
                                backFaceSetting

                        Types.LineSegments _ _ _ ->
                            empty

                        Types.Polyline _ _ _ ->
                            empty

                        Types.Points _ _ _ _ ->
                            empty

                TexturedPbrMaterial ( baseColorData, constantBaseColor ) ( roughnessData, roughnessChannel ) ( metallicData, metallicChannel ) ( normalMapData, useNormalMap ) ->
                    case givenMesh of
                        Types.EmptyMesh ->
                            empty

                        Types.Triangles _ _ _ _ ->
                            empty

                        Types.Facets _ _ webGLMesh backFaceSetting ->
                            empty

                        Types.Indexed _ _ _ _ ->
                            empty

                        Types.MeshWithNormals _ _ webGLMesh backFaceSetting ->
                            empty

                        Types.MeshWithUvs _ _ _ _ ->
                            empty

                        Types.MeshWithNormalsAndUvs _ _ webGLMesh backFaceSetting ->
                            texturedPhysicalMesh
                                baseColorData
                                constantBaseColor
                                roughnessData
                                roughnessChannel
                                metallicData
                                metallicChannel
                                webGLMesh
                                backFaceSetting

                        Types.MeshWithTangents _ _ webGLMesh backFaceSetting ->
                            normalMappedPhysicalMesh
                                baseColorData
                                constantBaseColor
                                roughnessData
                                roughnessChannel
                                metallicData
                                metallicChannel
                                normalMapData
                                useNormalMap
                                webGLMesh
                                backFaceSetting

                        Types.LineSegments _ _ _ ->
                            empty

                        Types.Polyline _ _ _ ->
                            empty

                        Types.Points _ _ _ _ ->
                            empty


type ResolvedLambertianMaterial
    = ConstantLambertianMaterial LinearRgb
    | TexturedLambertianMaterial ( WebGL.Texture.Texture, Vec4 ) ( WebGL.Texture.Texture, Float )


type ResolvedPbrMaterial
    = ConstantPbrMaterial LinearRgb Float Float
    | TexturedPbrMaterial ( WebGL.Texture.Texture, Vec4 ) ( WebGL.Texture.Texture, Vec4 ) ( WebGL.Texture.Texture, Vec4 ) ( WebGL.Texture.Texture, Float )


zeroVec4 : Vec4
zeroVec4 =
    Math.Vector4.vec4 0 0 0 0


enabledVec3 : Vec3 -> Vec4
enabledVec3 vector =
    Math.Vector4.vec4
        (Math.Vector3.getX vector)
        (Math.Vector3.getY vector)
        (Math.Vector3.getZ vector)
        1


vec3Tuple : WebGL.Texture.Texture -> Texture LinearRgb -> ( WebGL.Texture.Texture, Vec4 )
vec3Tuple fallbackData texture =
    case texture of
        Types.Constant (LinearRgb baseColor) ->
            ( fallbackData, enabledVec3 baseColor )

        Types.Texture { data } ->
            ( data, zeroVec4 )


channelTuple : WebGL.Texture.Texture -> Texture Float -> ( WebGL.Texture.Texture, Vec4 )
channelTuple fallbackData texture =
    case texture of
        Types.Constant value ->
            ( fallbackData, Math.Vector4.vec4 0 0 0 value )

        Types.Texture { data, channel } ->
            ( data, channel )


normalMapTuple : WebGL.Texture.Texture -> Texture NormalMap -> ( WebGL.Texture.Texture, Float )
normalMapTuple fallbackData channel =
    case channel of
        Types.Constant Types.VerticalNormal ->
            ( fallbackData, 0.0 )

        Types.Texture { data } ->
            ( data, 1.0 )


type Tuple4 a b c d
    = Tuple4 a b c d


resolvePbr : Texture LinearRgb -> Texture Float -> Texture Float -> Texture NormalMap -> ResolvedPbrMaterial
resolvePbr baseColorTexture roughnessTexture metallicTexture normalMapTexture =
    case Tuple4 baseColorTexture roughnessTexture metallicTexture normalMapTexture of
        Tuple4 (Types.Constant baseColor) (Types.Constant roughness) (Types.Constant metallic) (Types.Constant Types.VerticalNormal) ->
            ConstantPbrMaterial baseColor roughness metallic

        Tuple4 (Types.Texture { data }) _ _ _ ->
            TexturedPbrMaterial
                ( data, zeroVec4 )
                (channelTuple data roughnessTexture)
                (channelTuple data metallicTexture)
                (normalMapTuple data normalMapTexture)

        Tuple4 _ (Types.Texture { data, channel }) _ _ ->
            TexturedPbrMaterial
                (vec3Tuple data baseColorTexture)
                ( data, channel )
                (channelTuple data metallicTexture)
                (normalMapTuple data normalMapTexture)

        Tuple4 _ _ (Types.Texture { data, channel }) _ ->
            TexturedPbrMaterial
                (vec3Tuple data baseColorTexture)
                (channelTuple data roughnessTexture)
                ( data, channel )
                (normalMapTuple data normalMapTexture)

        Tuple4 _ _ _ (Types.Texture { data }) ->
            TexturedPbrMaterial
                (vec3Tuple data baseColorTexture)
                (channelTuple data roughnessTexture)
                (channelTuple data metallicTexture)
                ( data, 1.0 )


resolveLambertian : Texture LinearRgb -> Texture NormalMap -> ResolvedLambertianMaterial
resolveLambertian materialColorTexture normalMapTexture =
    case ( materialColorTexture, normalMapTexture ) of
        ( Types.Constant materialColor, Types.Constant Types.VerticalNormal ) ->
            ConstantLambertianMaterial materialColor

        ( Types.Texture { data }, _ ) ->
            TexturedLambertianMaterial
                ( data, zeroVec4 )
                (normalMapTuple data normalMapTexture)

        ( _, Types.Texture { data } ) ->
            TexturedLambertianMaterial
                (vec3Tuple data materialColorTexture)
                (normalMapTuple data normalMapTexture)


quad :
    Bool
    -> Material.NormalMapped coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Entity coordinates
quad castsShadow givenMaterial firstPoint secondPoint thirdPoint fourthPoint =
    let
        meshEntity =
            quadMesh givenMaterial firstPoint secondPoint thirdPoint fourthPoint
    in
    if castsShadow then
        group [ meshEntity, quadShadow firstPoint secondPoint thirdPoint fourthPoint ]

    else
        meshEntity


quadVertices : WebGL.Mesh { quadVertex : Vec3 }
quadVertices =
    WebGL.triangleFan
        [ { quadVertex = Math.Vector3.vec3 0 0 0 }
        , { quadVertex = Math.Vector3.vec3 1 0 1 }
        , { quadVertex = Math.Vector3.vec3 1 1 2 }
        , { quadVertex = Math.Vector3.vec3 0 1 3 }
        ]


quadVertexPositions :
    Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Mat4
quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint =
    let
        p1 =
            Point3d.toMeters firstPoint

        p2 =
            Point3d.toMeters secondPoint

        p3 =
            Point3d.toMeters thirdPoint

        p4 =
            Point3d.toMeters fourthPoint
    in
    Math.Matrix4.fromRecord
        { m11 = p1.x
        , m21 = p1.y
        , m31 = p1.z
        , m41 = 0
        , m12 = p2.x
        , m22 = p2.y
        , m32 = p2.z
        , m42 = 0
        , m13 = p3.x
        , m23 = p3.y
        , m33 = p3.z
        , m43 = 0
        , m14 = p4.x
        , m24 = p4.y
        , m34 = p4.z
        , m44 = 0
        }


quadMesh :
    Material.NormalMapped coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Entity coordinates
quadMesh givenMaterial firstPoint secondPoint thirdPoint fourthPoint =
    Types.Entity <|
        MeshNode <|
            case givenMaterial of
                Types.UnlitMaterial (Types.Constant color) ->
                    \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                        WebGL.entityWith
                            (meshSettings isRightHanded Types.KeepBackFaces settings)
                            Shaders.quadVertex
                            Shaders.constantFragment
                            quadVertices
                            { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                            , constantColor = color
                            , sceneProperties = sceneProperties
                            , modelScale = modelScale
                            , modelMatrix = modelMatrix
                            , viewMatrix = viewMatrix
                            }

                Types.UnlitMaterial (Types.Texture { data }) ->
                    \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                        WebGL.entityWith
                            (meshSettings isRightHanded Types.KeepBackFaces settings)
                            Shaders.quadVertex
                            Shaders.colorTextureFragment
                            quadVertices
                            { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                            , colorTexture = data
                            , sceneProperties = sceneProperties
                            , modelScale = modelScale
                            , modelMatrix = modelMatrix
                            , viewMatrix = viewMatrix
                            }

                Types.EmissiveMaterial (Types.Constant (LinearRgb emissiveColor)) backlight ->
                    \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                        WebGL.entityWith
                            (meshSettings isRightHanded Types.KeepBackFaces settings)
                            Shaders.quadVertex
                            Shaders.emissiveFragment
                            quadVertices
                            { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                            , backlight = backlight
                            , emissiveColor = emissiveColor
                            , sceneProperties = sceneProperties
                            , modelScale = modelScale
                            , modelMatrix = modelMatrix
                            , viewMatrix = viewMatrix
                            }

                Types.EmissiveMaterial (Types.Texture { data }) backlight ->
                    \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                        WebGL.entityWith
                            (meshSettings isRightHanded Types.KeepBackFaces settings)
                            Shaders.quadVertex
                            Shaders.emissiveTextureFragment
                            quadVertices
                            { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                            , backlight = backlight
                            , colorTexture = data
                            , sceneProperties = sceneProperties
                            , modelScale = modelScale
                            , modelMatrix = modelMatrix
                            , viewMatrix = viewMatrix
                            }

                Types.LambertianMaterial materialColorTexture normalMapTexture ->
                    case resolveLambertian materialColorTexture normalMapTexture of
                        ConstantLambertianMaterial (LinearRgb materialColor) ->
                            \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                                WebGL.entityWith
                                    (meshSettings isRightHanded Types.KeepBackFaces settings)
                                    Shaders.quadVertex
                                    Shaders.lambertianFragment
                                    quadVertices
                                    { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                                    , environmentalLighting = environmentalLighting
                                    , lightSources12 = lightSources.lightSources12
                                    , lightSources34 = lightSources.lightSources34
                                    , lightSources56 = lightSources.lightSources56
                                    , lightSources78 = lightSources.lightSources78
                                    , materialColor = materialColor
                                    , sceneProperties = sceneProperties
                                    , modelScale = modelScale
                                    , modelMatrix = modelMatrix
                                    , viewMatrix = viewMatrix
                                    }

                        TexturedLambertianMaterial ( materialColorData, constantMaterialColor ) ( normalMapData, useNormalMap ) ->
                            \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                                WebGL.entityWith
                                    (meshSettings isRightHanded Types.KeepBackFaces settings)
                                    Shaders.quadVertex
                                    Shaders.lambertianTextureFragment
                                    quadVertices
                                    { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                                    , materialColorTexture = materialColorData
                                    , normalMapTexture = normalMapData
                                    , useNormalMap = useNormalMap
                                    , environmentalLighting = environmentalLighting
                                    , lightSources12 = lightSources.lightSources12
                                    , lightSources34 = lightSources.lightSources34
                                    , lightSources56 = lightSources.lightSources56
                                    , lightSources78 = lightSources.lightSources78
                                    , sceneProperties = sceneProperties
                                    , modelScale = modelScale
                                    , modelMatrix = modelMatrix
                                    , viewMatrix = viewMatrix
                                    }

                Types.PbrMaterial baseColorTexture roughnessTexture metallicTexture normalMapTexture ->
                    case resolvePbr baseColorTexture roughnessTexture metallicTexture normalMapTexture of
                        ConstantPbrMaterial (LinearRgb baseColor) roughness metallic ->
                            \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                                WebGL.entityWith
                                    (meshSettings isRightHanded Types.KeepBackFaces settings)
                                    Shaders.quadVertex
                                    Shaders.physicalFragment
                                    quadVertices
                                    { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                                    , environmentalLighting = environmentalLighting
                                    , lightSources12 = lightSources.lightSources12
                                    , lightSources34 = lightSources.lightSources34
                                    , lightSources56 = lightSources.lightSources56
                                    , lightSources78 = lightSources.lightSources78
                                    , baseColor = baseColor
                                    , roughness = roughness
                                    , metallic = metallic
                                    , sceneProperties = sceneProperties
                                    , modelScale = modelScale
                                    , modelMatrix = modelMatrix
                                    , viewMatrix = viewMatrix
                                    }

                        TexturedPbrMaterial ( baseColorData, constantBaseColor ) ( roughnessData, roughnessChannel ) ( metallicData, metallicChannel ) ( normalMapData, useNormalMap ) ->
                            \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                                WebGL.entityWith
                                    (meshSettings isRightHanded Types.KeepBackFaces settings)
                                    Shaders.quadVertex
                                    Shaders.physicalTexturesFragment
                                    quadVertices
                                    { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                                    , environmentalLighting = environmentalLighting
                                    , lightSources12 = lightSources.lightSources12
                                    , lightSources34 = lightSources.lightSources34
                                    , lightSources56 = lightSources.lightSources56
                                    , lightSources78 = lightSources.lightSources78
                                    , baseColorTexture = baseColorData
                                    , constantBaseColor = constantBaseColor
                                    , roughnessTexture = roughnessData
                                    , roughnessChannel = roughnessChannel
                                    , metallicTexture = metallicData
                                    , metallicChannel = metallicChannel
                                    , normalMapTexture = normalMapData
                                    , useNormalMap = useNormalMap
                                    , sceneProperties = sceneProperties
                                    , modelScale = modelScale
                                    , modelMatrix = modelMatrix
                                    , viewMatrix = viewMatrix
                                    }


sphere : Bool -> Material.NormalMapped coordinates -> Sphere3d Meters coordinates -> Entity coordinates
sphere castsShadow givenMaterial givenSphere =
    let
        (Quantity r) =
            Sphere3d.radius givenSphere

        baseEntity =
            mesh givenMaterial Primitives.sphere

        untransformedEntity =
            if castsShadow then
                group [ baseEntity, sphereShadow givenSphere ]

            else
                baseEntity
    in
    untransformedEntity
        |> transformBy (Transformation.preScale r r r)
        |> translateBy (Vector3d.from Point3d.origin (Sphere3d.centerPoint givenSphere))


sphereShadow : Sphere3d Meters coordinates -> Entity coordinates
sphereShadow givenSphere =
    Types.Entity <|
        Types.ShadowNode <|
            \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                -- Note that the way the sphere shadow mesh is constructed in the vertex shaders, it
                -- will always form proper right-handed triangles regardless of the current model
                -- matrix handedness
                WebGL.entityWith (shadowSettings True settings)
                    Shaders.sphereShadowVertex
                    Shaders.shadowFragment
                    sphereOutlineMesh
                    { sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    , shadowLightSource = lightSources.lightSources12
                    , constantColor = Math.Vector3.vec3 0 0 1
                    }


sphereOutlineMesh : WebGL.Mesh { angle : Float, offsetScale : Float }
sphereOutlineMesh =
    WebGL.triangleStrip (buildSphereOutline numOutlineSegments [])


numOutlineSegments : number
numOutlineSegments =
    72


buildSphereOutline : Float -> List { angle : Float, offsetScale : Float } -> List { angle : Float, offsetScale : Float }
buildSphereOutline index accumulated =
    let
        angle =
            Float.interpolateFrom 0 (2 * pi) (index / numOutlineSegments)

        left =
            { angle = angle, offsetScale = 0 }

        right =
            { angle = angle, offsetScale = 1 }

        updated =
            left :: right :: accumulated
    in
    if index == 0 then
        updated

    else
        buildSphereOutline (index - 1) updated


block : Bool -> Material.Uniform coordinates -> Block3d Meters coordinates -> Entity coordinates
block castsShadow givenMaterial givenBlock =
    let
        ( Quantity scaleX, Quantity scaleY, Quantity scaleZ ) =
            Block3d.dimensions givenBlock

        baseEntity =
            mesh givenMaterial Primitives.block

        untransformedEntity =
            if castsShadow then
                group [ baseEntity, shadow Primitives.blockShadow ]

            else
                baseEntity
    in
    untransformedEntity
        |> transformBy (Transformation.preScale scaleX scaleY scaleZ)
        |> placeIn (Block3d.axes givenBlock)


cylinder : Bool -> Material.Uniform coordinates -> Cylinder3d Meters coordinates -> Entity coordinates
cylinder castsShadow givenMaterial givenCylinder =
    let
        (Quantity radius) =
            Cylinder3d.radius givenCylinder

        (Quantity length) =
            Cylinder3d.length givenCylinder

        centerFrame =
            Frame3d.fromZAxis (Cylinder3d.axis givenCylinder)

        baseEntity =
            mesh givenMaterial Primitives.cylinder

        untransformedEntity =
            if castsShadow then
                group [ baseEntity, shadow Primitives.cylinderShadow ]

            else
                baseEntity
    in
    untransformedEntity
        |> transformBy (Transformation.preScale radius radius length)
        |> placeIn centerFrame


shadow : Shadow coordinates -> Entity coordinates
shadow givenShadow =
    case shadowDrawFunction givenShadow of
        Just drawFunction ->
            Types.Entity (ShadowNode drawFunction)

        Nothing ->
            empty


shadowDrawFunction : Types.Shadow coordinates -> Maybe Types.DrawFunction
shadowDrawFunction givenShadow =
    case givenShadow of
        Types.EmptyShadow ->
            Nothing

        Types.Shadow _ webGLMesh ->
            -- TODO take handedness into account?
            Just <|
                \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                    WebGL.entityWith (shadowSettings isRightHanded settings)
                        Shaders.shadowVertex
                        Shaders.shadowFragment
                        webGLMesh
                        { sceneProperties = sceneProperties
                        , modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , viewMatrix = viewMatrix
                        , shadowLightSource = lightSources.lightSources12
                        }


quadShadowVertices : WebGL.Mesh { quadShadowVertex : Vec2 }
quadShadowVertices =
    WebGL.triangles
        [ ( { quadShadowVertex = Math.Vector2.vec2 0 -1 }
          , { quadShadowVertex = Math.Vector2.vec2 1 -1 }
          , { quadShadowVertex = Math.Vector2.vec2 1 1 }
          )
        , ( { quadShadowVertex = Math.Vector2.vec2 1 1 }
          , { quadShadowVertex = Math.Vector2.vec2 0 1 }
          , { quadShadowVertex = Math.Vector2.vec2 0 -1 }
          )
        , ( { quadShadowVertex = Math.Vector2.vec2 1 -1 }
          , { quadShadowVertex = Math.Vector2.vec2 2 -1 }
          , { quadShadowVertex = Math.Vector2.vec2 2 1 }
          )
        , ( { quadShadowVertex = Math.Vector2.vec2 2 1 }
          , { quadShadowVertex = Math.Vector2.vec2 1 1 }
          , { quadShadowVertex = Math.Vector2.vec2 1 -1 }
          )
        , ( { quadShadowVertex = Math.Vector2.vec2 2 -1 }
          , { quadShadowVertex = Math.Vector2.vec2 3 -1 }
          , { quadShadowVertex = Math.Vector2.vec2 3 1 }
          )
        , ( { quadShadowVertex = Math.Vector2.vec2 3 1 }
          , { quadShadowVertex = Math.Vector2.vec2 2 1 }
          , { quadShadowVertex = Math.Vector2.vec2 2 -1 }
          )
        , ( { quadShadowVertex = Math.Vector2.vec2 3 -1 }
          , { quadShadowVertex = Math.Vector2.vec2 0 -1 }
          , { quadShadowVertex = Math.Vector2.vec2 0 1 }
          )
        , ( { quadShadowVertex = Math.Vector2.vec2 0 1 }
          , { quadShadowVertex = Math.Vector2.vec2 3 1 }
          , { quadShadowVertex = Math.Vector2.vec2 3 -1 }
          )
        ]


quadShadow :
    Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Entity coordinates
quadShadow firstPoint secondPoint thirdPoint fourthPoint =
    Types.Entity <|
        Types.ShadowNode <|
            \sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith (shadowSettings isRightHanded settings)
                    Shaders.quadShadowVertex
                    Shaders.shadowFragment
                    quadShadowVertices
                    { quadVertexPositions = quadVertexPositions firstPoint secondPoint thirdPoint fourthPoint
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    , shadowLightSource = lightSources.lightSources12
                    }


cullBackFaceSetting : WebGL.Settings.Setting
cullBackFaceSetting =
    WebGL.Settings.cullFace WebGL.Settings.back


cullFrontFaceSetting : WebGL.Settings.Setting
cullFrontFaceSetting =
    WebGL.Settings.cullFace WebGL.Settings.front


meshSettings : Bool -> BackFaceSetting -> List WebGL.Settings.Setting -> List WebGL.Settings.Setting
meshSettings isRightHanded backFaceSetting settings =
    case backFaceSetting of
        CullBackFaces ->
            if isRightHanded then
                cullBackFaceSetting :: settings

            else
                cullFrontFaceSetting :: settings

        KeepBackFaces ->
            settings


rightHandedStencilTest : WebGL.Settings.Setting
rightHandedStencilTest =
    StencilTest.testSeparate
        { ref = 1
        , mask = 0xFF
        , writeMask = 0xFF
        }
        { test = StencilTest.always
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.incrementWrap
        }
        { test = StencilTest.always
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.decrementWrap
        }


leftHandedStencilTest : WebGL.Settings.Setting
leftHandedStencilTest =
    StencilTest.testSeparate
        { ref = 1
        , mask = 0xFF
        , writeMask = 0xFF
        }
        { test = StencilTest.always
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.decrementWrap
        }
        { test = StencilTest.always
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.incrementWrap
        }


shadowSettings : Bool -> List WebGL.Settings.Setting -> List WebGL.Settings.Setting
shadowSettings isRightHanded settings =
    if isRightHanded then
        rightHandedStencilTest :: settings

    else
        leftHandedStencilTest :: settings


constantMesh : Vec3 -> WebGL.Mesh { a | position : Vec3 } -> BackFaceSetting -> Entity coordinates
constantMesh color webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.plainVertex
                    Shaders.constantFragment
                    webGLMesh
                    { constantColor = color
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


colorTextureMesh : WebGL.Texture.Texture -> WebGL.Mesh { a | position : Vec3, uv : Vec2 } -> BackFaceSetting -> Entity coordinates
colorTextureMesh data webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.unlitVertex
                    Shaders.colorTextureFragment
                    webGLMesh
                    { colorTexture = data
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


constantPointMesh : Vec3 -> Float -> WebGL.Mesh { a | position : Vec3 } -> Entity coordinates
constantPointMesh color radius webGLMesh =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (WebGL.Settings.sampleAlphaToCoverage :: settings)
                    Shaders.pointVertex
                    Shaders.constantPointFragment
                    webGLMesh
                    { constantColor = color
                    , pointRadius = radius
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


emissiveMesh : Vec3 -> Float -> WebGL.Mesh { a | position : Vec3 } -> BackFaceSetting -> Entity coordinates
emissiveMesh color backlight webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.plainVertex
                    Shaders.emissiveFragment
                    webGLMesh
                    { emissiveColor = Math.Vector3.scale backlight color
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


texturedEmissiveMesh : WebGL.Texture.Texture -> Float -> WebGL.Mesh { a | position : Vec3, uv : Vec2 } -> BackFaceSetting -> Entity coordinates
texturedEmissiveMesh colorData backlight webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.unlitVertex
                    Shaders.emissiveTextureFragment
                    webGLMesh
                    { colorTexture = colorData
                    , backlight = backlight
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


emissivePointMesh : Vec3 -> Float -> WebGL.Mesh { a | position : Vec3 } -> Entity coordinates
emissivePointMesh color radius webGLMesh =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (WebGL.Settings.sampleAlphaToCoverage :: settings)
                    Shaders.pointVertex
                    Shaders.emissivePointFragment
                    webGLMesh
                    { emissiveColor = color
                    , pointRadius = radius
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


lambertianMesh : Vec3 -> WebGL.Mesh { a | position : Vec3, normal : Vec3 } -> BackFaceSetting -> Entity coordinates
lambertianMesh color webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.uniformVertex
                    Shaders.lambertianFragment
                    webGLMesh
                    { materialColor = color
                    , sceneProperties = sceneProperties
                    , environmentalLighting = environmentalLighting
                    , lightSources12 = lightSources.lightSources12
                    , lightSources34 = lightSources.lightSources34
                    , lightSources56 = lightSources.lightSources56
                    , lightSources78 = lightSources.lightSources78
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


texturedLambertianMesh : WebGL.Texture.Texture -> WebGL.Mesh { a | position : Vec3, normal : Vec3, uv : Vec2 } -> BackFaceSetting -> Entity coordinates
texturedLambertianMesh materialColorData webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.texturedVertex
                    Shaders.lambertianTextureFragment
                    webGLMesh
                    { materialColorTexture = materialColorData
                    , normalMapTexture = materialColorData
                    , useNormalMap = 0.0
                    , sceneProperties = sceneProperties
                    , environmentalLighting = environmentalLighting
                    , lightSources12 = lightSources.lightSources12
                    , lightSources34 = lightSources.lightSources34
                    , lightSources56 = lightSources.lightSources56
                    , lightSources78 = lightSources.lightSources78
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


normalMappedLambertianMesh : WebGL.Texture.Texture -> WebGL.Texture.Texture -> Float -> WebGL.Mesh { a | position : Vec3, normal : Vec3, uv : Vec2, tangent : Vec3 } -> BackFaceSetting -> Entity coordinates
normalMappedLambertianMesh materialColorData normalMapData useNormalMap webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.normalMappedVertex
                    Shaders.lambertianTextureFragment
                    webGLMesh
                    { materialColorTexture = materialColorData
                    , normalMapTexture = normalMapData
                    , useNormalMap = useNormalMap
                    , sceneProperties = sceneProperties
                    , environmentalLighting = environmentalLighting
                    , lightSources12 = lightSources.lightSources12
                    , lightSources34 = lightSources.lightSources34
                    , lightSources56 = lightSources.lightSources56
                    , lightSources78 = lightSources.lightSources78
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


physicalMesh : Vec3 -> Float -> Float -> WebGL.Mesh { a | position : Vec3, normal : Vec3 } -> BackFaceSetting -> Entity coordinates
physicalMesh color roughness metallic webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.uniformVertex
                    Shaders.physicalFragment
                    webGLMesh
                    { baseColor = color
                    , roughness = roughness
                    , metallic = metallic
                    , sceneProperties = sceneProperties
                    , environmentalLighting = environmentalLighting
                    , lightSources12 = lightSources.lightSources12
                    , lightSources34 = lightSources.lightSources34
                    , lightSources56 = lightSources.lightSources56
                    , lightSources78 = lightSources.lightSources78
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


texturedPhysicalMesh : WebGL.Texture.Texture -> Vec4 -> WebGL.Texture.Texture -> Vec4 -> WebGL.Texture.Texture -> Vec4 -> WebGL.Mesh { a | position : Vec3, normal : Vec3, uv : Vec2 } -> BackFaceSetting -> Entity coordinates
texturedPhysicalMesh baseColorData constantBaseColor roughnessData roughnessChannel metallicData metallicChannel webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.texturedVertex
                    Shaders.physicalTexturesFragment
                    webGLMesh
                    { baseColorTexture = baseColorData
                    , constantBaseColor = constantBaseColor
                    , roughnessTexture = roughnessData
                    , roughnessChannel = roughnessChannel
                    , metallicTexture = metallicData
                    , metallicChannel = metallicChannel
                    , normalMapTexture = baseColorData
                    , useNormalMap = 0.0
                    , sceneProperties = sceneProperties
                    , environmentalLighting = environmentalLighting
                    , lightSources12 = lightSources.lightSources12
                    , lightSources34 = lightSources.lightSources34
                    , lightSources56 = lightSources.lightSources56
                    , lightSources78 = lightSources.lightSources78
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


normalMappedPhysicalMesh : WebGL.Texture.Texture -> Vec4 -> WebGL.Texture.Texture -> Vec4 -> WebGL.Texture.Texture -> Vec4 -> WebGL.Texture.Texture -> Float -> WebGL.Mesh { a | position : Vec3, normal : Vec3, uv : Vec2, tangent : Vec3 } -> BackFaceSetting -> Entity coordinates
normalMappedPhysicalMesh baseColorData constantBaseColor roughnessData roughnessChannel metallicData metallicChannel normalMapData useNormalMap webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.normalMappedVertex
                    Shaders.physicalTexturesFragment
                    webGLMesh
                    { baseColorTexture = baseColorData
                    , constantBaseColor = constantBaseColor
                    , roughnessTexture = roughnessData
                    , roughnessChannel = roughnessChannel
                    , metallicTexture = metallicData
                    , metallicChannel = metallicChannel
                    , normalMapTexture = normalMapData
                    , useNormalMap = useNormalMap
                    , sceneProperties = sceneProperties
                    , environmentalLighting = environmentalLighting
                    , lightSources12 = lightSources.lightSources12
                    , lightSources34 = lightSources.lightSources34
                    , lightSources56 = lightSources.lightSources56
                    , lightSources78 = lightSources.lightSources78
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


collectNodes : List (Entity coordinates) -> List Node -> List Node
collectNodes drawables accumulated =
    case drawables of
        [] ->
            accumulated

        (Types.Entity node) :: rest ->
            collectNodes rest (node :: accumulated)


group : List (Entity coordinates) -> Entity coordinates
group drawables =
    Types.Entity (Group (collectNodes drawables []))


transformBy : Transformation -> Entity coordinates1 -> Entity coordinates2
transformBy transformation (Types.Entity node) =
    case node of
        EmptyNode ->
            empty

        Transformed existingTransformation underlyingNode ->
            let
                compositeTransformation =
                    Transformation.compose existingTransformation transformation
            in
            Types.Entity (Transformed compositeTransformation underlyingNode)

        MeshNode _ ->
            Types.Entity (Transformed transformation node)

        ShadowNode _ ->
            Types.Entity (Transformed transformation node)

        Group _ ->
            Types.Entity (Transformed transformation node)


rotateAround : Axis3d Meters coordinates -> Angle -> Entity coordinates -> Entity coordinates
rotateAround axis angle givenDrawable =
    transformBy (Transformation.rotateAround axis angle) givenDrawable


translateBy : Vector3d Meters coordinates -> Entity coordinates -> Entity coordinates
translateBy displacement givenDrawable =
    transformBy (Transformation.translateBy displacement) givenDrawable


translateIn : Direction3d coordinates -> Length -> Entity coordinates -> Entity coordinates
translateIn direction distance drawable =
    translateBy (Vector3d.withLength distance direction) drawable


mirrorAcross : Plane3d Meters coordinates -> Entity coordinates -> Entity coordinates
mirrorAcross plane givenDrawable =
    transformBy (Transformation.mirrorAcross plane) givenDrawable


relativeTo : Frame3d Meters globalCoordinates { defines : localCoordinates } -> Entity globalCoordinates -> Entity localCoordinates
relativeTo frame givenDrawable =
    transformBy (Transformation.relativeTo frame) givenDrawable


placeIn : Frame3d Meters globalCoordinates { defines : localCoordinates } -> Entity localCoordinates -> Entity globalCoordinates
placeIn frame givenDrawable =
    transformBy (Transformation.placeIn frame) givenDrawable


scaleAbout : Point3d Meters coordinates -> Float -> Entity coordinates -> Entity coordinates
scaleAbout point scale givenDrawable =
    transformBy (Transformation.scaleAbout point scale) givenDrawable
