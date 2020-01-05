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
import Math.Vector3 exposing (Vec3)
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
import Scene3d.Types as Types exposing (BackFaceSetting(..), Bounds, LinearRgb(..), Material(..), Node(..), PlainVertex, SmoothVertex)
import Sphere3d exposing (Sphere3d)
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL
import WebGL.Settings


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
        Types.ConstantMaterial color ->
            case givenMesh of
                Types.EmptyMesh ->
                    empty

                Types.Triangles _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.Facets _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.Indexed _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.Smooth _ _ webGLMesh backFaceSetting ->
                    constantMesh color webGLMesh backFaceSetting

                Types.LineSegments _ _ webGLMesh ->
                    constantMesh color webGLMesh KeepBackFaces

                Types.Polyline _ _ webGLMesh ->
                    constantMesh color webGLMesh KeepBackFaces

                Types.Points _ radius _ webGLMesh ->
                    constantPointMesh color radius webGLMesh

        Types.EmissiveMaterial (LinearRgb emissiveColor) ->
            case givenMesh of
                Types.EmptyMesh ->
                    empty

                Types.Triangles _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.Facets _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.Indexed _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.Smooth _ _ webGLMesh backFaceSetting ->
                    emissiveMesh emissiveColor webGLMesh backFaceSetting

                Types.LineSegments _ _ webGLMesh ->
                    emissiveMesh emissiveColor webGLMesh KeepBackFaces

                Types.Polyline _ _ webGLMesh ->
                    emissiveMesh emissiveColor webGLMesh KeepBackFaces

                Types.Points _ radius _ webGLMesh ->
                    emissivePointMesh emissiveColor radius webGLMesh

        Types.LambertianMaterial (LinearRgb materialColor) ->
            case givenMesh of
                Types.EmptyMesh ->
                    empty

                Types.Triangles _ _ _ _ ->
                    empty

                Types.Facets _ _ webGLMesh cullBackFaces ->
                    lambertianMesh materialColor webGLMesh cullBackFaces

                Types.Indexed _ _ _ _ ->
                    empty

                Types.Smooth _ _ webGLMesh cullBackFaces ->
                    lambertianMesh materialColor webGLMesh cullBackFaces

                Types.LineSegments _ _ _ ->
                    empty

                Types.Polyline _ _ _ ->
                    empty

                Types.Points _ _ _ _ ->
                    empty

        Types.PbrMaterial (LinearRgb baseColor) roughness metallic ->
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

                Types.Smooth _ _ webGLMesh backFaceSetting ->
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


sphere : Sphere3d Meters coordinates -> Material.ForMeshWithNormals -> Bool -> Entity coordinates
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
