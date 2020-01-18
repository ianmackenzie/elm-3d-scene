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
import Frame3d exposing (Frame3d)
import Length exposing (Length, Meters)
import LineSegment3d exposing (LineSegment3d)
import Luminance exposing (Luminance)
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
        , Channel(..)
        , LinearRgb(..)
        , Material(..)
        , Node(..)
        , PlainVertex
        , VertexWithNormal
        , VertexWithUv
        )
import Sphere3d exposing (Sphere3d)
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL
import WebGL.Settings
import WebGL.Texture exposing (Texture)


type alias Entity coordinates =
    Types.Entity coordinates


type alias Material properties =
    Types.Material properties


empty : Entity coordinates
empty =
    Types.Entity EmptyNode


mesh : Mesh coordinates properties -> Material properties -> Entity coordinates
mesh givenMesh givenMaterial =
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

                Types.LineSegments _ _ webGLMesh ->
                    constantMesh color webGLMesh KeepBackFaces

                Types.Polyline _ _ webGLMesh ->
                    constantMesh color webGLMesh KeepBackFaces

                Types.Points _ radius _ webGLMesh ->
                    constantPointMesh color radius webGLMesh

        Types.UnlitMaterial (Types.Textured { data }) ->
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
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.Facets _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.Indexed _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.MeshWithNormals _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.MeshWithUvs _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.MeshWithNormalsAndUvs _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.LineSegments _ _ webGLMesh ->
                    emissiveMesh emissiveColor webGLMesh KeepBackFaces

                Types.Polyline _ _ webGLMesh ->
                    emissiveMesh emissiveColor webGLMesh KeepBackFaces

                Types.Points _ radius _ webGLMesh ->
                    emissivePointMesh emissiveColor radius webGLMesh

        Types.EmissiveMaterial (Types.Textured { data }) backlight ->
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

                Types.LineSegments _ _ webGLMesh ->
                    empty

                Types.Polyline _ _ webGLMesh ->
                    empty

                Types.Points _ radius _ webGLMesh ->
                    empty

        Types.LambertianMaterial (Types.Constant (LinearRgb materialColor)) ->
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

                Types.LineSegments _ _ _ ->
                    empty

                Types.Polyline _ _ _ ->
                    empty

                Types.Points _ _ _ _ ->
                    empty

        Types.LambertianMaterial (Types.Textured { data }) ->
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
                    texturedLambertianMesh data webGLMesh cullBackFaces

                Types.LineSegments _ _ _ ->
                    empty

                Types.Polyline _ _ _ ->
                    empty

                Types.Points _ _ _ _ ->
                    empty

        Types.PbrMaterial baseColorChannel roughnessChannel metallicChannel ->
            case resolve baseColorChannel roughnessChannel metallicChannel of
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

                        Types.LineSegments _ _ _ ->
                            empty

                        Types.Polyline _ _ _ ->
                            empty

                        Types.Points _ _ _ _ ->
                            empty

                TexturedPbrMaterial ( baseColorTexture, constantBaseColor ) ( roughnessTexture, constantRoughness ) ( metallicTexture, constantMetallic ) ->
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
                                baseColorTexture
                                constantBaseColor
                                roughnessTexture
                                constantRoughness
                                metallicTexture
                                constantMetallic
                                webGLMesh
                                backFaceSetting

                        Types.LineSegments _ _ _ ->
                            empty

                        Types.Polyline _ _ _ ->
                            empty

                        Types.Points _ _ _ _ ->
                            empty


type ResolvedPbrMaterial
    = ConstantPbrMaterial LinearRgb Float Float
    | TexturedPbrMaterial ( Texture, Vec4 ) ( Texture, Vec2 ) ( Texture, Vec2 )


zeroVec2 : Vec2
zeroVec2 =
    Math.Vector2.vec2 0 0


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


enabledFloat : Float -> Vec2
enabledFloat givenValue =
    Math.Vector2.vec2 givenValue 1


vec3Tuple : Texture -> Channel LinearRgb -> ( Texture, Vec4 )
vec3Tuple fallbackTexture channel =
    case channel of
        Types.Constant (LinearRgb baseColor) ->
            ( fallbackTexture, enabledVec3 baseColor )

        Types.Textured { data } ->
            ( data, zeroVec4 )


floatTuple : Texture -> Channel Float -> ( Texture, Vec2 )
floatTuple fallbackTexture channel =
    case channel of
        Types.Constant value ->
            ( fallbackTexture, enabledFloat value )

        Types.Textured { data } ->
            ( data, zeroVec2 )


resolve : Channel LinearRgb -> Channel Float -> Channel Float -> ResolvedPbrMaterial
resolve baseColorChannel roughnessChannel metallicChannel =
    case ( baseColorChannel, roughnessChannel, metallicChannel ) of
        ( Types.Constant baseColor, Types.Constant roughness, Types.Constant metallic ) ->
            ConstantPbrMaterial baseColor roughness metallic

        ( Types.Textured { data }, _, _ ) ->
            TexturedPbrMaterial
                ( data, zeroVec4 )
                (floatTuple data roughnessChannel)
                (floatTuple data metallicChannel)

        ( _, Types.Textured { data }, _ ) ->
            TexturedPbrMaterial
                (vec3Tuple data baseColorChannel)
                ( data, zeroVec2 )
                (floatTuple data metallicChannel)

        ( _, _, Types.Textured { data } ) ->
            TexturedPbrMaterial
                (vec3Tuple data baseColorChannel)
                (floatTuple data roughnessChannel)
                ( data, zeroVec2 )


sphere : Sphere3d Meters coordinates -> Material.ForMeshWithNormalsAndUvs -> Bool -> Entity coordinates
sphere givenSphere givenMaterial castsShadow =
    let
        (Quantity r) =
            Sphere3d.radius givenSphere

        baseEntity =
            mesh Primitives.sphere givenMaterial

        untransformedEntity =
            if castsShadow then
                group [ baseEntity, shadow Primitives.sphereShadow ]

            else
                baseEntity
    in
    untransformedEntity
        |> transformBy (Transformation.preScale r r r)
        |> translateBy (Vector3d.from Point3d.origin (Sphere3d.centerPoint givenSphere))


block : Block3d Meters coordinates -> Material.ForMeshWithNormals -> Bool -> Entity coordinates
block givenBlock givenMaterial castsShadow =
    let
        ( Quantity scaleX, Quantity scaleY, Quantity scaleZ ) =
            Block3d.dimensions givenBlock

        baseEntity =
            mesh Primitives.block givenMaterial

        untransformedEntity =
            if castsShadow then
                group [ baseEntity, shadow Primitives.blockShadow ]

            else
                baseEntity
    in
    untransformedEntity
        |> transformBy (Transformation.preScale scaleX scaleY scaleZ)
        |> placeIn (Block3d.axes givenBlock)


cylinder : Cylinder3d Meters coordinates -> Material.ForMeshWithNormals -> Bool -> Entity coordinates
cylinder givenCylinder givenMaterial castsShadow =
    let
        (Quantity radius) =
            Cylinder3d.radius givenCylinder

        (Quantity length) =
            Cylinder3d.length givenCylinder

        centerFrame =
            Frame3d.fromZAxis (Cylinder3d.axis givenCylinder)

        baseEntity =
            mesh Primitives.cylinder givenMaterial

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
                    WebGL.entityWith settings
                        Shaders.shadowVertex
                        Shaders.shadowFragment
                        webGLMesh
                        { sceneProperties = sceneProperties
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


colorTextureMesh : Texture -> WebGL.Mesh { a | position : Vec3, uv : Vec2 } -> BackFaceSetting -> Entity coordinates
colorTextureMesh texture webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.texturedVertex
                    Shaders.colorTextureFragment
                    webGLMesh
                    { colorTexture = texture
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


emissiveMesh : Vec3 -> WebGL.Mesh { a | position : Vec3 } -> BackFaceSetting -> Entity coordinates
emissiveMesh color webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.plainVertex
                    Shaders.emissiveFragment
                    webGLMesh
                    { emissiveColor = color
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


texturedEmissiveMesh : Texture -> Float -> WebGL.Mesh { a | position : Vec3, uv : Vec2 } -> BackFaceSetting -> Entity coordinates
texturedEmissiveMesh colorTexture backlight webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.texturedVertex
                    Shaders.emissiveTextureFragment
                    webGLMesh
                    { colorTexture = colorTexture
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
                    Shaders.smoothVertex
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


texturedLambertianMesh : Texture -> WebGL.Mesh { a | position : Vec3, normal : Vec3, uv : Vec2 } -> BackFaceSetting -> Entity coordinates
texturedLambertianMesh texture webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.smoothTexturedVertex
                    Shaders.lambertianTextureFragment
                    webGLMesh
                    { materialColorTexture = texture
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
                    Shaders.smoothVertex
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


texturedPhysicalMesh : Texture -> Vec4 -> Texture -> Vec2 -> Texture -> Vec2 -> WebGL.Mesh { a | position : Vec3, normal : Vec3, uv : Vec2 } -> BackFaceSetting -> Entity coordinates
texturedPhysicalMesh baseColorTexture constantBaseColor roughnessTexture constantRoughness metallicTexture constantMetallic webGLMesh backFaceSetting =
    Types.Entity <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix environmentalLighting lightSources settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded backFaceSetting settings)
                    Shaders.smoothTexturedVertex
                    Shaders.physicalTexturesFragment
                    webGLMesh
                    { baseColorTexture = baseColorTexture
                    , constantBaseColor = constantBaseColor
                    , roughnessTexture = roughnessTexture
                    , constantRoughness = constantRoughness
                    , metallicTexture = metallicTexture
                    , constantMetallic = constantMetallic
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
