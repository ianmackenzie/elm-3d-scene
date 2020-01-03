module Scene3d.Drawable exposing
    ( Entity
    , Material
    , colored
    , emissive
    , empty
    , group
    , lambertian
    , mirrorAcross
    , physical
    , placeIn
    , relativeTo
    , rotateAround
    , scaleAbout
    , shadow
    , translateBy
    , translateIn
    , withShadow
    )

import Angle exposing (Angle)
import Array
import Axis3d exposing (Axis3d)
import BoundingBox3d exposing (BoundingBox3d)
import Color exposing (Color)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Length exposing (Length, Meters)
import LineSegment3d exposing (LineSegment3d)
import Luminance exposing (Luminance)
import Math.Vector3 exposing (Vec3)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity(..), Unitless)
import Scene3d.Chromaticity as Chromaticity exposing (Chromaticity)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Mesh exposing (Mesh, Shadow, Yes)
import Scene3d.Shaders as Shaders
import Scene3d.Transformation as Transformation exposing (Transformation)
import Scene3d.Types as Types exposing (BackFaceSetting(..), Bounds, LinearRgb(..), Node(..), PlainVertex, SmoothVertex)
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL
import WebGL.Settings


type alias Entity coordinates =
    Types.Entity coordinates


type alias Material =
    { baseColor : Color
    , roughness : Float
    , metallic : Bool
    }


empty : Entity coordinates
empty =
    Types.Entity EmptyNode


colored : Color -> Mesh coordinates primitives -> Entity coordinates
colored givenColor givenMesh =
    let
        ( red, green, blue ) =
            Color.toRGB givenColor

        colorVec =
            Math.Vector3.vec3 (red / 255) (green / 255) (blue / 255)
    in
    case givenMesh of
        Types.EmptyMesh ->
            empty

        Types.Triangles _ _ webGLMesh backFaceSetting ->
            constantMesh colorVec webGLMesh backFaceSetting

        Types.Facets _ _ webGLMesh backFaceSetting ->
            constantMesh colorVec webGLMesh backFaceSetting

        Types.Indexed _ _ webGLMesh backFaceSetting ->
            constantMesh colorVec webGLMesh backFaceSetting

        Types.Smooth _ _ webGLMesh backFaceSetting ->
            constantMesh colorVec webGLMesh backFaceSetting

        Types.LineSegments _ _ webGLMesh ->
            constantMesh colorVec webGLMesh KeepBackFaces

        Types.Polyline _ _ webGLMesh ->
            constantMesh colorVec webGLMesh KeepBackFaces

        Types.Points _ radius _ webGLMesh ->
            constantPointMesh colorVec radius webGLMesh


emissive : Chromaticity -> Luminance -> Mesh coordinates primitives -> Entity coordinates
emissive givenChromaticity givenLuminance givenMesh =
    let
        (LinearRgb chromaticityInLinearRgb) =
            ColorConversions.chromaticityToLinearRgb givenChromaticity

        nits =
            Luminance.inNits givenLuminance

        linearColor =
            Math.Vector3.scale nits chromaticityInLinearRgb
    in
    case givenMesh of
        Types.EmptyMesh ->
            empty

        Types.Triangles _ _ webGLMesh backFaceSetting ->
            emissiveMesh linearColor webGLMesh backFaceSetting

        Types.Facets _ _ webGLMesh backFaceSetting ->
            emissiveMesh linearColor webGLMesh backFaceSetting

        Types.Indexed _ _ webGLMesh backFaceSetting ->
            emissiveMesh linearColor webGLMesh backFaceSetting

        Types.Smooth _ _ webGLMesh backFaceSetting ->
            emissiveMesh linearColor webGLMesh backFaceSetting

        Types.LineSegments _ _ webGLMesh ->
            emissiveMesh linearColor webGLMesh KeepBackFaces

        Types.Polyline _ _ webGLMesh ->
            emissiveMesh linearColor webGLMesh KeepBackFaces

        Types.Points _ radius _ webGLMesh ->
            emissivePointMesh linearColor radius webGLMesh


toLinear : Color -> Vec3
toLinear color =
    let
        (LinearRgb rgb) =
            ColorConversions.colorToLinearRgb color
    in
    rgb


lambertian : Color -> Mesh coordinates { a | hasNormals : Yes } -> Entity coordinates
lambertian givenColor givenMesh =
    let
        linearColor =
            toLinear givenColor
    in
    case givenMesh of
        Types.EmptyMesh ->
            empty

        Types.Triangles _ _ _ _ ->
            empty

        Types.Facets _ _ webGLMesh cullBackFaces ->
            lambertianMesh linearColor webGLMesh cullBackFaces

        Types.Indexed _ _ _ _ ->
            empty

        Types.Smooth _ _ webGLMesh cullBackFaces ->
            lambertianMesh linearColor webGLMesh cullBackFaces

        Types.LineSegments _ _ _ ->
            empty

        Types.Polyline _ _ _ ->
            empty

        Types.Points _ _ _ _ ->
            empty


physical : Material -> Mesh coordinates { a | hasNormals : Yes } -> Entity coordinates
physical givenMaterial givenMesh =
    let
        linearColor =
            toLinear givenMaterial.baseColor

        roughness =
            clamp 0 1 givenMaterial.roughness

        metallic =
            if givenMaterial.metallic then
                1

            else
                0
    in
    case givenMesh of
        Types.EmptyMesh ->
            empty

        Types.Triangles _ _ _ _ ->
            empty

        Types.Facets _ _ webGLMesh backFaceSetting ->
            physicalMesh
                linearColor
                roughness
                metallic
                webGLMesh
                backFaceSetting

        Types.Indexed _ _ _ _ ->
            empty

        Types.Smooth _ _ webGLMesh backFaceSetting ->
            physicalMesh
                linearColor
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


shadow : Shadow coordinates -> Entity coordinates
shadow givenShadow =
    case shadowDrawFunction givenShadow of
        Just drawFunction ->
            Types.Entity (ShadowNode drawFunction)

        Nothing ->
            empty


withShadow : Shadow coordinates -> Entity coordinates -> Entity coordinates
withShadow givenShadow drawable =
    group [ drawable, shadow givenShadow ]


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
