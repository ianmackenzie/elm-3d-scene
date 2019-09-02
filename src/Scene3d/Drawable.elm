module Scene3d.Drawable exposing
    ( Drawable
    , Material
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
    , withColor
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
import Scene3d.Color exposing (LinearRgb(..))
import Scene3d.Mesh exposing (LineSegments, Mesh, Points, ShadowsEnabled, Triangles, WithNormals, WithTangents, WithUV)
import Scene3d.Shader as Shader
import Scene3d.Transformation as Transformation exposing (Transformation)
import Scene3d.Types as Types exposing (Bounds, Node(..), PlainVertex, SmoothVertex)
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL
import WebGL.Settings


type alias Drawable coordinates =
    Types.Drawable coordinates


type alias Material =
    { baseColor : Color
    , roughness : Float
    , metallic : Bool
    }


empty : Drawable coordinates
empty =
    Types.Drawable EmptyNode


withColor : Color -> Mesh coordinates primitives -> Drawable coordinates
withColor givenColor givenMesh =
    let
        { red, green, blue } =
            Color.toRgba givenColor

        colorVec =
            Math.Vector3.vec3 red green blue
    in
    case givenMesh of
        Types.EmptyMesh ->
            empty

        Types.Mesh meshData maybeShadow ->
            case meshData of
                Types.Triangles _ _ webGLMesh cullBackFaces ->
                    constantMesh colorVec webGLMesh cullBackFaces

                Types.Facets _ _ webGLMesh cullBackFaces ->
                    constantMesh colorVec webGLMesh cullBackFaces

                Types.Indexed _ _ webGLMesh cullBackFaces ->
                    constantMesh colorVec webGLMesh cullBackFaces

                Types.Smooth _ _ webGLMesh cullBackFaces ->
                    constantMesh colorVec webGLMesh cullBackFaces

                Types.LineSegments _ _ webGLMesh ->
                    constantMesh colorVec webGLMesh False

                Types.Polyline _ _ webGLMesh ->
                    constantMesh colorVec webGLMesh False

                Types.Points _ _ webGLMesh ->
                    constantPointMesh colorVec webGLMesh


emissive : Chromaticity -> Luminance -> Mesh coordinates primitives -> Drawable coordinates
emissive givenChromaticity givenLuminance givenMesh =
    let
        (LinearRgb r g b) =
            Chromaticity.toLinearRgb givenChromaticity

        nits =
            Luminance.inNits givenLuminance

        linearColor =
            Math.Vector3.vec3 (r * nits) (g * nits) (b * nits)
    in
    case givenMesh of
        Types.EmptyMesh ->
            empty

        Types.Mesh meshData maybeShadow ->
            case meshData of
                Types.Triangles _ _ webGLMesh cullBackFaces ->
                    emissiveMesh linearColor webGLMesh cullBackFaces

                Types.Facets _ _ webGLMesh cullBackFaces ->
                    emissiveMesh linearColor webGLMesh cullBackFaces

                Types.Indexed _ _ webGLMesh cullBackFaces ->
                    emissiveMesh linearColor webGLMesh cullBackFaces

                Types.Smooth _ _ webGLMesh cullBackFaces ->
                    emissiveMesh linearColor webGLMesh cullBackFaces

                Types.LineSegments _ _ webGLMesh ->
                    emissiveMesh linearColor webGLMesh False

                Types.Polyline _ _ webGLMesh ->
                    emissiveMesh linearColor webGLMesh False

                Types.Points _ _ webGLMesh ->
                    emissivePointMesh linearColor webGLMesh


toLinear : Color -> Vec3
toLinear color =
    -- TODO use actual sRGB formula
    let
        { red, green, blue } =
            Color.toRgba color
    in
    Math.Vector3.vec3 (red ^ 2.2) (green ^ 2.2) (blue ^ 2.2)


lambertian : Color -> Mesh coordinates (Triangles WithNormals uv tangents shadows) -> Drawable coordinates
lambertian givenColor givenMesh =
    let
        linearColor =
            toLinear givenColor
    in
    case givenMesh of
        Types.EmptyMesh ->
            empty

        Types.Mesh meshData maybeShadow ->
            case meshData of
                Types.Triangles _ _ _ _ ->
                    Debug.log "triangles/lambertian" empty

                Types.Facets _ _ webGLMesh cullBackFaces ->
                    lambertianMesh linearColor webGLMesh cullBackFaces

                Types.Indexed _ _ _ _ ->
                    Debug.log "indexed/lambertian" empty

                Types.Smooth _ _ webGLMesh cullBackFaces ->
                    lambertianMesh linearColor webGLMesh cullBackFaces

                Types.LineSegments _ _ _ ->
                    Debug.log "lineSegments/lambertian" empty

                Types.Polyline _ _ _ ->
                    Debug.log "polyline/lambertian" empty

                Types.Points _ _ _ ->
                    Debug.log "points/lambertian" empty


physical : Material -> Mesh coordinates (Triangles WithNormals uv tangents shadows) -> Drawable coordinates
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

        Types.Mesh meshData maybeShadow ->
            case meshData of
                Types.Triangles _ _ _ _ ->
                    Debug.log "triangles/physical" empty

                Types.Facets _ _ webGLMesh cullBackFaces ->
                    physicalMesh
                        linearColor
                        roughness
                        metallic
                        webGLMesh
                        cullBackFaces

                Types.Indexed _ _ _ _ ->
                    Debug.log "indexed/physical" empty

                Types.Smooth _ _ webGLMesh cullBackFaces ->
                    physicalMesh
                        linearColor
                        roughness
                        metallic
                        webGLMesh
                        cullBackFaces

                Types.LineSegments _ _ _ ->
                    Debug.log "lineSegments/physical" empty

                Types.Polyline _ _ _ ->
                    Debug.log "polyline/physical" empty

                Types.Points _ _ _ ->
                    Debug.log "points/physical" empty


shadow : Mesh coordinates (Triangles normals uv tangents ShadowsEnabled) -> Drawable coordinates
shadow givenMesh =
    case givenMesh of
        Types.EmptyMesh ->
            empty

        Types.Mesh meshData maybeShadow ->
            case shadowDrawFunction maybeShadow of
                Just drawFunction ->
                    Types.Drawable (ShadowNode drawFunction)

                Nothing ->
                    empty


withShadow : Mesh coordinates (Triangles normals uv tangents ShadowsEnabled) -> Drawable coordinates -> Drawable coordinates
withShadow shadowMesh drawable =
    group [ drawable, shadow shadowMesh ]


shadowDrawFunction : Maybe (Types.Shadow coordinates) -> Maybe Types.DrawFunction
shadowDrawFunction maybeShadow =
    case maybeShadow of
        Nothing ->
            Nothing

        Just givenShadow ->
            case givenShadow of
                Types.EmptyShadow ->
                    Nothing

                Types.Shadow _ webGLMesh ->
                    -- TODO take handedness into account?
                    Just <|
                        \sceneProperties modelScale modelMatrix isRightHanded viewMatrix ambientLighting lights settings ->
                            WebGL.entityWith settings
                                Shader.shadowVertex
                                Shader.shadowFragment
                                webGLMesh
                                { sceneProperties = sceneProperties
                                , modelScale = modelScale
                                , modelMatrix = modelMatrix
                                , viewMatrix = viewMatrix
                                , lights = lights.lights12
                                }


cullBackFaceSetting : WebGL.Settings.Setting
cullBackFaceSetting =
    WebGL.Settings.cullFace WebGL.Settings.back


cullFrontFaceSetting : WebGL.Settings.Setting
cullFrontFaceSetting =
    WebGL.Settings.cullFace WebGL.Settings.front


meshSettings : Bool -> Bool -> List WebGL.Settings.Setting -> List WebGL.Settings.Setting
meshSettings isRightHanded cullBackFaces settings =
    if cullBackFaces then
        if isRightHanded then
            cullBackFaceSetting :: settings

        else
            cullFrontFaceSetting :: settings

    else
        settings


constantMesh : Vec3 -> WebGL.Mesh { a | position : Vec3 } -> Bool -> Drawable coordinates
constantMesh color webGLMesh cullBackFaces =
    Types.Drawable <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix ambientLighting lights settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded cullBackFaces settings)
                    Shader.plainVertex
                    Shader.constantFragment
                    webGLMesh
                    { color = color
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


constantPointMesh : Vec3 -> WebGL.Mesh { a | position : Vec3 } -> Drawable coordinates
constantPointMesh color webGLMesh =
    Types.Drawable <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix ambientLighting lights settings ->
                WebGL.entityWith
                    (WebGL.Settings.sampleAlphaToCoverage :: settings)
                    Shader.pointVertex
                    Shader.constantPointFragment
                    webGLMesh
                    { color = color
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


emissiveMesh : Vec3 -> WebGL.Mesh { a | position : Vec3 } -> Bool -> Drawable coordinates
emissiveMesh color webGLMesh cullBackFaces =
    Types.Drawable <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix ambientLighting lights settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded cullBackFaces settings)
                    Shader.plainVertex
                    Shader.emissiveFragment
                    webGLMesh
                    { color = color
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


emissivePointMesh : Vec3 -> WebGL.Mesh { a | position : Vec3 } -> Drawable coordinates
emissivePointMesh color webGLMesh =
    Types.Drawable <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix ambientLighting lights settings ->
                WebGL.entityWith
                    (WebGL.Settings.sampleAlphaToCoverage :: settings)
                    Shader.pointVertex
                    Shader.emissivePointFragment
                    webGLMesh
                    { color = color
                    , sceneProperties = sceneProperties
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


lambertianMesh : Vec3 -> WebGL.Mesh { a | position : Vec3, normal : Vec3 } -> Bool -> Drawable coordinates
lambertianMesh color webGLMesh cullBackFaces =
    Types.Drawable <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix ambientLighting lights settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded cullBackFaces settings)
                    Shader.smoothVertex
                    Shader.lambertianFragment
                    webGLMesh
                    { color = color
                    , sceneProperties = sceneProperties
                    , lights12 = lights.lights12
                    , lights34 = lights.lights34
                    , lights56 = lights.lights56
                    , lights78 = lights.lights78
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


physicalMesh : Vec3 -> Float -> Float -> WebGL.Mesh { a | position : Vec3, normal : Vec3 } -> Bool -> Drawable coordinates
physicalMesh color roughness metallic webGLMesh cullBackFaces =
    Types.Drawable <|
        MeshNode
            (\sceneProperties modelScale modelMatrix isRightHanded viewMatrix ambientLighting lights settings ->
                WebGL.entityWith
                    (meshSettings isRightHanded cullBackFaces settings)
                    Shader.smoothVertex
                    Shader.physicalFragment
                    webGLMesh
                    { color = color
                    , roughness = roughness
                    , metallic = metallic
                    , sceneProperties = sceneProperties
                    , ambientLighting = ambientLighting
                    , lights12 = lights.lights12
                    , lights34 = lights.lights34
                    , lights56 = lights.lights56
                    , lights78 = lights.lights78
                    , modelScale = modelScale
                    , modelMatrix = modelMatrix
                    , viewMatrix = viewMatrix
                    }
            )


collectNodes : List (Drawable coordinates) -> List Node -> List Node
collectNodes drawables accumulated =
    case drawables of
        [] ->
            accumulated

        (Types.Drawable node) :: rest ->
            collectNodes rest (node :: accumulated)


group : List (Drawable coordinates) -> Drawable coordinates
group drawables =
    Types.Drawable (Group (collectNodes drawables []))


transformBy : Transformation -> Drawable coordinates1 -> Drawable coordinates2
transformBy transformation (Types.Drawable node) =
    case node of
        EmptyNode ->
            empty

        Transformed existingTransformation underlyingNode ->
            let
                compositeTransformation =
                    Transformation.compose existingTransformation transformation
            in
            Types.Drawable (Transformed compositeTransformation underlyingNode)

        MeshNode _ ->
            Types.Drawable (Transformed transformation node)

        ShadowNode _ ->
            Types.Drawable (Transformed transformation node)

        Group _ ->
            Types.Drawable (Transformed transformation node)


rotateAround : Axis3d Meters coordinates -> Angle -> Drawable coordinates -> Drawable coordinates
rotateAround axis angle givenDrawable =
    transformBy (Transformation.rotateAround axis angle) givenDrawable


translateBy : Vector3d Meters coordinates -> Drawable coordinates -> Drawable coordinates
translateBy displacement givenDrawable =
    transformBy (Transformation.translateBy displacement) givenDrawable


translateIn : Direction3d coordinates -> Length -> Drawable coordinates -> Drawable coordinates
translateIn direction distance drawable =
    translateBy (Vector3d.withLength distance direction) drawable


mirrorAcross : Plane3d Meters coordinates -> Drawable coordinates -> Drawable coordinates
mirrorAcross plane givenDrawable =
    transformBy (Transformation.mirrorAcross plane) givenDrawable


relativeTo : Frame3d Meters globalCoordinates { defines : localCoordinates } -> Drawable globalCoordinates -> Drawable localCoordinates
relativeTo frame givenDrawable =
    transformBy (Transformation.relativeTo frame) givenDrawable


placeIn : Frame3d Meters globalCoordinates { defines : localCoordinates } -> Drawable localCoordinates -> Drawable globalCoordinates
placeIn frame givenDrawable =
    transformBy (Transformation.placeIn frame) givenDrawable


scaleAbout : Point3d Meters coordinates -> Float -> Drawable coordinates -> Drawable coordinates
scaleAbout point scale givenDrawable =
    transformBy (Transformation.scaleAbout point scale) givenDrawable
