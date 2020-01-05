module Scene3d exposing
    ( toHtml
    , Entity
    , nothing, block, sphere, cylinder, mesh, group
    , shadow, withShadow
    , rotateAround, translateBy, translateIn, scaleAbout, mirrorAcross
    , placeIn, relativeTo
    , transparentBackground, whiteBackground, blackBackground, defaultExposure, defaultWhiteBalance
    , DirectLighting, LightSource, directionalLight, pointLight, noDirectLighting, oneLightSource, twoLightSources, threeLightSources, fourLightSources, fiveLightSources, sixLightSources, sevenLightSources, eightLightSources
    , EnvironmentalLighting, noEnvironmentalLighting, softLighting
    , toWebGLEntities
    )

{-|

@docs toHtml

@docs Entity

@docs nothing, block, sphere, cylinder, mesh, group

@docs shadow, withShadow

@docs rotateAround, translateBy, translateIn, scaleAbout, mirrorAcross

@docs placeIn, relativeTo

@docs transparentBackground, whiteBackground, blackBackground, defaultExposure, defaultWhiteBalance

@docs DirectLighting, LightSource, directionalLight, pointLight, noDirectLighting, oneLightSource, twoLightSources, threeLightSources, fourLightSources, fiveLightSources, sixLightSources, sevenLightSources, eightLightSources

@docs EnvironmentalLighting, noEnvironmentalLighting, softLighting

@docs toWebGLEntities

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Color.Transparent
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes
import Illuminance exposing (Illuminance)
import Length exposing (Length, Meters)
import Luminance exposing (Luminance)
import LuminousFlux exposing (LuminousFlux)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Pixels exposing (Pixels, inPixels)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Rectangle2d
import Scene3d.Chromaticity as Chromaticity exposing (Chromaticity)
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Entity as Entity
import Scene3d.Exposure as Exposure exposing (Exposure)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Scene3d.Transformation as Transformation exposing (Transformation)
import Scene3d.Types as Types exposing (DrawFunction, LightMatrices, LinearRgb(..), Material(..), Node(..))
import Sphere3d exposing (Sphere3d)
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture exposing (Texture)



----- ENTITIES -----


type alias Entity coordinates =
    Types.Entity coordinates


nothing : Entity coordinates
nothing =
    Entity.empty


sphere : Sphere3d Meters coordinates -> Material.ForMeshWithNormals -> { castsShadow : Bool } -> Entity coordinates
sphere givenSphere givenMaterial { castsShadow } =
    Entity.sphere givenSphere givenMaterial castsShadow


block : Block3d Meters coordinates -> Material.ForMeshWithNormals -> { castsShadow : Bool } -> Entity coordinates
block givenBlock givenMaterial { castsShadow } =
    Entity.block givenBlock givenMaterial castsShadow


cylinder : Cylinder3d Meters coordinates -> Material.ForMeshWithNormals -> { castsShadow : Bool } -> Entity coordinates
cylinder givenCylinder givenMaterial { castsShadow } =
    Entity.cylinder givenCylinder givenMaterial castsShadow


mesh : Mesh coordinates properties -> Material properties -> Entity coordinates
mesh givenMesh givenMaterial =
    Entity.mesh givenMesh givenMaterial


group : List (Entity coordinates) -> Entity coordinates
group entities =
    Entity.group entities


shadow : Mesh.Shadow coordinates -> Entity coordinates
shadow givenShadow =
    Entity.shadow givenShadow


withShadow : Mesh.Shadow coordinates -> Entity coordinates -> Entity coordinates
withShadow givenShadow givenEntity =
    group [ givenEntity, shadow givenShadow ]


rotateAround : Axis3d Meters coordinates -> Angle -> Entity coordinates -> Entity coordinates
rotateAround axis angle entity =
    Entity.rotateAround axis angle entity


translateBy : Vector3d Meters coordinates -> Entity coordinates -> Entity coordinates
translateBy displacement entity =
    Entity.translateBy displacement entity


translateIn : Direction3d coordinates -> Length -> Entity coordinates -> Entity coordinates
translateIn direction distance entity =
    Entity.translateIn direction distance entity


scaleAbout : Point3d Meters coordinates -> Float -> Entity coordinates -> Entity coordinates
scaleAbout centerPoint scale entity =
    Entity.scaleAbout centerPoint scale entity


mirrorAcross : Plane3d Meters coordinates -> Entity coordinates -> Entity coordinates
mirrorAcross plane entity =
    Entity.mirrorAcross plane entity


placeIn : Frame3d Meters coordinates { defines : localCoordinates } -> Entity localCoordinates -> Entity coordinates
placeIn frame entity =
    Entity.placeIn frame entity


relativeTo : Frame3d Meters coordinates { defines : localCoordinates } -> Entity coordinates -> Entity localCoordinates
relativeTo frame entity =
    Entity.relativeTo frame entity



----- LIGHT SOURCES -----
--
-- type:
--   0 : disabled
--   1 : directional (XYZ is direction to light, i.e. reversed light direction)
--   2 : point (XYZ is light position)
--
-- radius is unused for now (will hopefully add sphere lights in the future)
--
-- [ x_i     r_i       x_j     r_j      ]
-- [ y_i     g_i       y_j     g_j      ]
-- [ z_i     b_i       z_j     b_j      ]
-- [ type_i  radius_i  type_j  radius_j ]


type alias LightSource coordinates =
    Types.LightSource coordinates


type DirectLighting coordinates
    = SingleUnshadowedPass LightMatrices
    | SingleShadowedPass LightMatrices
    | TwoPasses LightMatrices LightMatrices


disabledLightSource : LightSource coordinates
disabledLightSource =
    Types.LightSource
        { type_ = 0
        , x = 0
        , y = 0
        , z = 0
        , r = 0
        , g = 0
        , b = 0
        , radius = 0
        }


lightSourcePair : LightSource coordinates -> LightSource coordinates -> Mat4
lightSourcePair (Types.LightSource first) (Types.LightSource second) =
    Math.Matrix4.fromRecord
        { m11 = first.x
        , m21 = first.y
        , m31 = first.z
        , m41 = first.type_
        , m12 = first.r
        , m22 = first.g
        , m32 = first.b
        , m42 = first.radius
        , m13 = second.x
        , m23 = second.y
        , m33 = second.z
        , m43 = second.type_
        , m14 = second.r
        , m24 = second.g
        , m34 = second.b
        , m44 = second.radius
        }


lightingDisabled : LightMatrices
lightingDisabled =
    { lightSources12 = lightSourcePair disabledLightSource disabledLightSource
    , lightSources34 = lightSourcePair disabledLightSource disabledLightSource
    , lightSources56 = lightSourcePair disabledLightSource disabledLightSource
    , lightSources78 = lightSourcePair disabledLightSource disabledLightSource
    }


noDirectLighting : DirectLighting coordinates
noDirectLighting =
    SingleUnshadowedPass lightingDisabled


oneLightSource : LightSource coordinates -> { castsShadows : Bool } -> DirectLighting coordinates
oneLightSource lightSource { castsShadows } =
    let
        lightMatrices =
            { lightSources12 = lightSourcePair lightSource disabledLightSource
            , lightSources34 = lightSourcePair disabledLightSource disabledLightSource
            , lightSources56 = lightSourcePair disabledLightSource disabledLightSource
            , lightSources78 = lightSourcePair disabledLightSource disabledLightSource
            }
    in
    if castsShadows then
        SingleShadowedPass lightMatrices

    else
        SingleUnshadowedPass lightMatrices


twoLightSources :
    ( LightSource coordinates, { castsShadows : Bool } )
    -> LightSource coordinates
    -> DirectLighting coordinates
twoLightSources first second =
    eightLightSources
        first
        second
        disabledLightSource
        disabledLightSource
        disabledLightSource
        disabledLightSource
        disabledLightSource
        disabledLightSource


threeLightSources :
    ( LightSource coordinates, { castsShadows : Bool } )
    -> LightSource coordinates
    -> LightSource coordinates
    -> DirectLighting coordinates
threeLightSources first second third =
    eightLightSources
        first
        second
        third
        disabledLightSource
        disabledLightSource
        disabledLightSource
        disabledLightSource
        disabledLightSource


fourLightSources :
    ( LightSource coordinates, { castsShadows : Bool } )
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> DirectLighting coordinates
fourLightSources first second third fourth =
    eightLightSources
        first
        second
        third
        fourth
        disabledLightSource
        disabledLightSource
        disabledLightSource
        disabledLightSource


fiveLightSources :
    ( LightSource coordinates, { castsShadows : Bool } )
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> DirectLighting coordinates
fiveLightSources first second third fourth fifth =
    eightLightSources
        first
        second
        third
        fourth
        fifth
        disabledLightSource
        disabledLightSource
        disabledLightSource


sixLightSources :
    ( LightSource coordinates, { castsShadows : Bool } )
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> DirectLighting coordinates
sixLightSources first second third fourth fifth sixth =
    eightLightSources
        first
        second
        third
        fourth
        fifth
        sixth
        disabledLightSource
        disabledLightSource


sevenLightSources :
    ( LightSource coordinates, { castsShadows : Bool } )
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> DirectLighting coordinates
sevenLightSources first second third fourth fifth sixth seventh =
    eightLightSources
        first
        second
        third
        fourth
        fifth
        sixth
        seventh
        disabledLightSource


eightLightSources :
    ( LightSource coordinates, { castsShadows : Bool } )
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> LightSource coordinates
    -> DirectLighting coordinates
eightLightSources ( first, { castsShadows } ) second third fourth fifth sixth seventh eigth =
    if castsShadows then
        TwoPasses
            { lightSources12 = lightSourcePair first second
            , lightSources34 = lightSourcePair third fourth
            , lightSources56 = lightSourcePair fifth sixth
            , lightSources78 = lightSourcePair seventh eigth
            }
            { lightSources12 = lightSourcePair second third
            , lightSources34 = lightSourcePair fourth fifth
            , lightSources56 = lightSourcePair sixth seventh
            , lightSources78 = lightSourcePair eigth disabledLightSource
            }

    else
        SingleUnshadowedPass
            { lightSources12 = lightSourcePair first second
            , lightSources34 = lightSourcePair third fourth
            , lightSources56 = lightSourcePair fifth sixth
            , lightSources78 = lightSourcePair seventh eigth
            }


directionalLight : Chromaticity -> Illuminance -> Direction3d coordinates -> LightSource coordinates
directionalLight chromaticity illuminance direction =
    let
        { x, y, z } =
            Direction3d.unwrap direction

        (LinearRgb rgb) =
            ColorConversions.chromaticityToLinearRgb chromaticity

        lux =
            Illuminance.inLux illuminance
    in
    Types.LightSource
        { type_ = 1
        , x = -x
        , y = -y
        , z = -z
        , r = lux * Math.Vector3.getX rgb
        , g = lux * Math.Vector3.getY rgb
        , b = lux * Math.Vector3.getZ rgb
        , radius = 0
        }


pointLight : Chromaticity -> LuminousFlux -> Point3d Meters coordinates -> LightSource coordinates
pointLight chromaticity luminousFlux position =
    let
        (LinearRgb rgb) =
            ColorConversions.chromaticityToLinearRgb chromaticity

        lumens =
            LuminousFlux.inLumens luminousFlux

        { x, y, z } =
            Point3d.unwrap position
    in
    Types.LightSource
        { type_ = 2
        , x = x
        , y = y
        , z = z
        , r = lumens * Math.Vector3.getX rgb
        , g = lumens * Math.Vector3.getY rgb
        , b = lumens * Math.Vector3.getZ rgb
        , radius = 0
        }



----- ENVIRONMENTAL LIGHTING ------


type alias EnvironmentalLighting coordinates =
    Types.EnvironmentalLighting coordinates


environmentalLightingDisabled : Mat4
environmentalLightingDisabled =
    Math.Matrix4.fromRecord
        { m11 = 0
        , m21 = 0
        , m31 = 0
        , m41 = 0
        , m12 = 0
        , m22 = 0
        , m32 = 0
        , m42 = 0
        , m13 = 0
        , m23 = 0
        , m33 = 0
        , m43 = 0
        , m14 = 0
        , m24 = 0
        , m34 = 0
        , m44 = 0
        }


noEnvironmentalLighting : EnvironmentalLighting coordinates
noEnvironmentalLighting =
    Types.NoEnvironmentalLighting


{-| Good default value for max luminance: 5000 nits
-}
softLighting :
    { upDirection : Direction3d coordinates
    , above : ( Luminance, Chromaticity )
    , below : ( Luminance, Chromaticity )
    }
    -> EnvironmentalLighting coordinates
softLighting { upDirection, above, below } =
    let
        { x, y, z } =
            Direction3d.unwrap upDirection

        ( aboveLuminance, aboveChromaticity ) =
            above

        ( belowLuminance, belowChromaticity ) =
            below

        aboveNits =
            Luminance.inNits aboveLuminance

        belowNits =
            Luminance.inNits belowLuminance

        (LinearRgb aboveRgb) =
            ColorConversions.chromaticityToLinearRgb aboveChromaticity

        (LinearRgb belowRgb) =
            ColorConversions.chromaticityToLinearRgb belowChromaticity
    in
    Types.SoftLighting <|
        Math.Matrix4.fromRecord
            { m11 = x
            , m21 = y
            , m31 = z
            , m41 = 1
            , m12 = aboveNits * Math.Vector3.getX aboveRgb
            , m22 = aboveNits * Math.Vector3.getY aboveRgb
            , m32 = aboveNits * Math.Vector3.getZ aboveRgb
            , m42 = 0
            , m13 = belowNits * Math.Vector3.getX belowRgb
            , m23 = belowNits * Math.Vector3.getY belowRgb
            , m33 = belowNits * Math.Vector3.getZ belowRgb
            , m43 = 0
            , m14 = 0
            , m24 = 0
            , m34 = 0
            , m44 = 0
            }



----- DEFAULTS -----


transparentBackground : Color.Transparent.Color
transparentBackground =
    Color.Transparent.fromRGBA
        { red = 0
        , green = 0
        , blue = 0
        , alpha = Color.Transparent.transparent
        }


blackBackground : Color.Transparent.Color
blackBackground =
    Color.Transparent.fromRGBA
        { red = 0
        , green = 0
        , blue = 0
        , alpha = Color.Transparent.opaque
        }


whiteBackground : Color.Transparent.Color
whiteBackground =
    Color.Transparent.fromRGBA
        { red = 255
        , green = 255
        , blue = 255
        , alpha = Color.Transparent.opaque
        }


defaultExposure : Exposure
defaultExposure =
    Exposure.srgb


defaultWhiteBalance : Chromaticity
defaultWhiteBalance =
    Chromaticity.d65



----- RENDERING -----


type alias RenderPass =
    LightMatrices -> List WebGL.Settings.Setting -> WebGL.Entity


type alias RenderPasses =
    { meshes : List RenderPass
    , shadows : List RenderPass
    }


createRenderPass : Mat4 -> Mat4 -> Mat4 -> Transformation -> DrawFunction -> RenderPass
createRenderPass sceneProperties viewMatrix ambientLightingMatrix transformation drawFunction =
    drawFunction
        sceneProperties
        (Math.Vector3.vec3 transformation.scaleX transformation.scaleY transformation.scaleZ)
        (Transformation.modelMatrix transformation)
        transformation.isRightHanded
        viewMatrix
        ambientLightingMatrix


collectRenderPasses : Mat4 -> Mat4 -> Mat4 -> Transformation -> Node -> RenderPasses -> RenderPasses
collectRenderPasses sceneProperties viewMatrix ambientLightingMatrix currentTransformation node accumulated =
    case node of
        EmptyNode ->
            accumulated

        Transformed transformation childNode ->
            collectRenderPasses
                sceneProperties
                viewMatrix
                ambientLightingMatrix
                (Transformation.compose transformation currentTransformation)
                childNode
                accumulated

        MeshNode meshDrawFunction ->
            let
                updatedMeshes =
                    createRenderPass
                        sceneProperties
                        viewMatrix
                        ambientLightingMatrix
                        currentTransformation
                        meshDrawFunction
                        :: accumulated.meshes
            in
            { meshes = updatedMeshes
            , shadows = accumulated.shadows
            }

        ShadowNode shadowDrawFunction ->
            let
                updatedShadows =
                    createRenderPass
                        sceneProperties
                        viewMatrix
                        ambientLightingMatrix
                        currentTransformation
                        shadowDrawFunction
                        :: accumulated.shadows
            in
            { meshes = accumulated.meshes
            , shadows = updatedShadows
            }

        Group childNodes ->
            List.foldl
                (collectRenderPasses
                    sceneProperties
                    viewMatrix
                    ambientLightingMatrix
                    currentTransformation
                )
                accumulated
                childNodes



-- ## Overall scene Properties
--
-- projectionType:
--   0: perspective (camera XYZ is eye position)
--   1: orthographic (camera XYZ is direction to screen)
--
-- [ clipDistance  cameraX         whiteR  * ]
-- [ aspectRatio   cameraY         whiteG  * ]
-- [ kc            cameraZ         whiteB  * ]
-- [ kz            projectionType  *       * ]


depthTestDefault : List WebGL.Settings.Setting
depthTestDefault =
    [ DepthTest.default ]


outsideStencil : List WebGL.Settings.Setting
outsideStencil =
    [ DepthTest.lessOrEqual { write = True, near = 0, far = 1 }
    , StencilTest.test
        { ref = 0
        , mask = 0xFF
        , test = StencilTest.equal
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.keep
        , writeMask = 0x00
        }
    ]


insideStencil : List WebGL.Settings.Setting
insideStencil =
    [ DepthTest.lessOrEqual { write = True, near = 0, far = 1 }
    , StencilTest.test
        { ref = 0
        , mask = 0xFF
        , test = StencilTest.notEqual
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.keep
        , writeMask = 0x00
        }
    ]


createShadowStencil : List WebGL.Settings.Setting
createShadowStencil =
    [ DepthTest.less { write = False, near = 0, far = 1 }
    , WebGL.Settings.colorMask False False False False
    , StencilTest.testSeparate
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
    ]


call : List RenderPass -> LightMatrices -> List WebGL.Settings.Setting -> List WebGL.Entity
call renderPasses lightMatrices settings =
    renderPasses
        |> List.map (\renderPass -> renderPass lightMatrices settings)


toWebGLEntities :
    { directLighting : DirectLighting coordinates
    , environmentalLighting : EnvironmentalLighting coordinates
    , camera : Camera3d Meters coordinates
    , exposure : Exposure
    , whiteBalance : Chromaticity
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , backgroundColor : Color.Transparent.Color
    }
    -> List (Entity coordinates)
    -> List WebGL.Entity
toWebGLEntities arguments drawables =
    let
        aspectRatio =
            Quantity.ratio arguments.width arguments.height

        projectionParameters =
            Camera3d.projectionParameters { screenAspectRatio = aspectRatio }
                arguments.camera

        clipDistance =
            Math.Vector4.getX projectionParameters

        kc =
            Math.Vector4.getZ projectionParameters

        kz =
            Math.Vector4.getW projectionParameters

        eyePoint =
            Camera3d.viewpoint arguments.camera
                |> Viewpoint3d.eyePoint
                |> Point3d.unwrap

        projectionType =
            if kz == 0 then
                0

            else
                1

        (LinearRgb linearRgb) =
            ColorConversions.chromaticityToLinearRgb arguments.whiteBalance

        maxLuminance =
            Luminance.inNits (Exposure.maxLuminance arguments.exposure)

        sceneProperties =
            Math.Matrix4.fromRecord
                { m11 = clipDistance
                , m21 = aspectRatio
                , m31 = kc
                , m41 = kz
                , m12 = eyePoint.x
                , m22 = eyePoint.y
                , m32 = eyePoint.z
                , m42 = projectionType
                , m13 = maxLuminance * Math.Vector3.getX linearRgb
                , m23 = maxLuminance * Math.Vector3.getY linearRgb
                , m33 = maxLuminance * Math.Vector3.getZ linearRgb
                , m43 = 0
                , m14 = 0
                , m24 = 0
                , m34 = 0
                , m44 = 0
                }

        viewMatrix =
            Camera3d.viewMatrix arguments.camera

        environmentalLightingMatrix =
            case arguments.environmentalLighting of
                Types.SoftLighting matrix ->
                    matrix

                Types.NoEnvironmentalLighting ->
                    environmentalLightingDisabled

        (Types.Entity rootNode) =
            Entity.group drawables

        renderPasses =
            collectRenderPasses
                sceneProperties
                viewMatrix
                environmentalLightingMatrix
                Transformation.identity
                rootNode
                { meshes = []
                , shadows = []
                }
    in
    case arguments.directLighting of
        SingleUnshadowedPass lightMatrices ->
            call renderPasses.meshes lightMatrices depthTestDefault

        SingleShadowedPass lightMatrices ->
            List.concat
                [ call renderPasses.meshes lightingDisabled depthTestDefault
                , call renderPasses.shadows lightMatrices createShadowStencil
                , call renderPasses.meshes lightMatrices outsideStencil
                ]

        TwoPasses allLightMatrices unshadowedLightMatrices ->
            List.concat
                [ call renderPasses.meshes allLightMatrices depthTestDefault
                , call renderPasses.shadows allLightMatrices createShadowStencil
                , call renderPasses.meshes unshadowedLightMatrices insideStencil
                ]


toHtml :
    { directLighting : DirectLighting coordinates
    , environmentalLighting : EnvironmentalLighting coordinates
    , camera : Camera3d Meters coordinates
    , exposure : Exposure
    , whiteBalance : Chromaticity
    , width : Quantity Float Pixels
    , height : Quantity Float Pixels
    , backgroundColor : Color.Transparent.Color
    }
    -> List (Entity coordinates)
    -> Html msg
toHtml arguments drawables =
    let
        widthInPixels =
            inPixels arguments.width

        heightInPixels =
            inPixels arguments.height

        { red, green, blue, alpha } =
            Color.Transparent.toRGBA arguments.backgroundColor

        webGLOptions =
            [ WebGL.depth 1
            , WebGL.stencil 0
            , WebGL.alpha True
            , WebGL.antialias
            , WebGL.clearColor
                (red / 255)
                (green / 255)
                (blue / 255)
                (Color.Transparent.opacityToFloat alpha)
            ]
    in
    WebGL.toHtmlWith webGLOptions
        [ Html.Attributes.width (round widthInPixels)
        , Html.Attributes.height (round heightInPixels)
        , Html.Attributes.style "width" (String.fromFloat widthInPixels ++ "px")
        , Html.Attributes.style "height" (String.fromFloat heightInPixels ++ "px")
        , Html.Attributes.style "display" "block"
        ]
        (toWebGLEntities arguments drawables)
