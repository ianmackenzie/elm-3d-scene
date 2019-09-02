module Scene3d exposing
    ( render, unlit, toEntities
    , Lights, noLights, oneLight, twoLights, threeLights, fourLights
    , Option, clearColor, devicePixelRatio, pointSize
    )

{-|

@docs render, unlit, toEntities

@docs Lights, noLights, oneLight, twoLights, threeLights, fourLights, fiveLights, sixLights, sevenLights, eightLights

@docs Option, clearColor, devicePixelRatio, pointSize

-}

import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes
import Length exposing (Meters)
import Luminance
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Pixels exposing (Pixels, inPixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Rectangle2d
import Scene3d.Chromaticity as Chromaticity exposing (Chromaticity)
import Scene3d.Color exposing (LinearRgb(..))
import Scene3d.Drawable exposing (Drawable)
import Scene3d.Exposure as Exposure exposing (Exposure)
import Scene3d.Light exposing (AmbientLighting, Light)
import Scene3d.Transformation as Transformation exposing (Transformation)
import Scene3d.Types as Types exposing (DrawFunction, LightMatrices, Node(..))
import Viewpoint3d
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture exposing (Texture)



----- AMBIENT LIGHTING -----


ambientLightingDisabled : Mat4
ambientLightingDisabled =
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



----- LIGHTS -----
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


type Lights coordinates
    = SingleUnshadowedPass LightMatrices
    | SingleShadowedPass LightMatrices
    | TwoPasses LightMatrices LightMatrices


disabledLight : Light coordinates
disabledLight =
    Types.Light
        { type_ = 0
        , x = 0
        , y = 0
        , z = 0
        , r = 0
        , g = 0
        , b = 0
        , radius = 0
        }


lightPair : Light coordinates -> Light coordinates -> Mat4
lightPair (Types.Light first) (Types.Light second) =
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
    { lights12 = lightPair disabledLight disabledLight
    , lights34 = lightPair disabledLight disabledLight
    , lights56 = lightPair disabledLight disabledLight
    , lights78 = lightPair disabledLight disabledLight
    }


noLights : Lights coordinates
noLights =
    SingleUnshadowedPass lightingDisabled


oneLight : Light coordinates -> { castsShadows : Bool } -> Lights coordinates
oneLight light { castsShadows } =
    let
        lightMatrices =
            { lights12 = lightPair light disabledLight
            , lights34 = lightPair disabledLight disabledLight
            , lights56 = lightPair disabledLight disabledLight
            , lights78 = lightPair disabledLight disabledLight
            }
    in
    if castsShadows then
        SingleShadowedPass lightMatrices

    else
        SingleUnshadowedPass lightMatrices


twoLights :
    ( Light coordinates, { castsShadows : Bool } )
    -> Light coordinates
    -> Lights coordinates
twoLights first second =
    eightLights
        first
        second
        disabledLight
        disabledLight
        disabledLight
        disabledLight
        disabledLight
        disabledLight


threeLights :
    ( Light coordinates, { castsShadows : Bool } )
    -> Light coordinates
    -> Light coordinates
    -> Lights coordinates
threeLights first second third =
    eightLights
        first
        second
        third
        disabledLight
        disabledLight
        disabledLight
        disabledLight
        disabledLight


fourLights :
    ( Light coordinates, { castsShadows : Bool } )
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Lights coordinates
fourLights first second third fourth =
    eightLights
        first
        second
        third
        fourth
        disabledLight
        disabledLight
        disabledLight
        disabledLight


fiveLights :
    ( Light coordinates, { castsShadows : Bool } )
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Lights coordinates
fiveLights first second third fourth fifth =
    eightLights
        first
        second
        third
        fourth
        fifth
        disabledLight
        disabledLight
        disabledLight


sixLights :
    ( Light coordinates, { castsShadows : Bool } )
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Lights coordinates
sixLights first second third fourth fifth sixth =
    eightLights
        first
        second
        third
        fourth
        fifth
        sixth
        disabledLight
        disabledLight


sevenLights :
    ( Light coordinates, { castsShadows : Bool } )
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Lights coordinates
sevenLights first second third fourth fifth sixth seventh =
    eightLights
        first
        second
        third
        fourth
        fifth
        sixth
        seventh
        disabledLight


eightLights :
    ( Light coordinates, { castsShadows : Bool } )
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Light coordinates
    -> Lights coordinates
eightLights ( firstLight, { castsShadows } ) secondLight thirdLight fourthLight fifthLight sixthLight seventhLight eigthLight =
    if castsShadows then
        TwoPasses
            { lights12 = lightPair firstLight secondLight
            , lights34 = lightPair thirdLight fourthLight
            , lights56 = lightPair fifthLight sixthLight
            , lights78 = lightPair seventhLight eigthLight
            }
            { lights12 = lightPair secondLight thirdLight
            , lights34 = lightPair fourthLight fifthLight
            , lights56 = lightPair sixthLight seventhLight
            , lights78 = lightPair eigthLight disabledLight
            }

    else
        SingleUnshadowedPass
            { lights12 = lightPair firstLight secondLight
            , lights34 = lightPair thirdLight fourthLight
            , lights56 = lightPair fifthLight sixthLight
            , lights78 = lightPair seventhLight eigthLight
            }



----- RENDERING -----


type alias RenderPass =
    LightMatrices -> List WebGL.Settings.Setting -> WebGL.Entity


type alias RenderPasses =
    { meshes : List RenderPass
    , shadows : List RenderPass
    }


createRenderPass : Mat4 -> Mat4 -> Mat4 -> Transformation -> DrawFunction -> RenderPass
createRenderPass sceneProperties viewMatrix ambientLighting transformation drawFunction =
    drawFunction
        sceneProperties
        transformation.scale
        (Transformation.modelMatrix transformation)
        transformation.isRightHanded
        viewMatrix
        ambientLighting


collectRenderPasses : Mat4 -> Mat4 -> Mat4 -> Transformation -> Node -> RenderPasses -> RenderPasses
collectRenderPasses sceneProperties viewMatrix ambientLighting currentTransformation node accumulated =
    case node of
        EmptyNode ->
            accumulated

        Transformed transformation childNode ->
            collectRenderPasses
                sceneProperties
                viewMatrix
                ambientLighting
                (Transformation.compose transformation currentTransformation)
                childNode
                accumulated

        MeshNode meshDrawFunction ->
            let
                updatedMeshes =
                    createRenderPass
                        sceneProperties
                        viewMatrix
                        ambientLighting
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
                        ambientLighting
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
                    ambientLighting
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
-- [ clipDistance  cameraX         whiteR     * ]
-- [ aspectRatio   cameraY         whiteG     * ]
-- [ kc            cameraZ         whiteB     * ]
-- [ kz            projectionType  pointSize  * ]


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


toEntities :
    List Option
    ->
        { ambientLighting : Maybe (AmbientLighting coordinates)
        , lights : Lights coordinates
        , scene : Drawable coordinates
        , camera : Camera3d Meters coordinates
        , exposure : Exposure
        , whiteBalance : Chromaticity
        , width : Quantity Float Pixels
        , height : Quantity Float Pixels
        }
    -> List WebGL.Entity
toEntities options { ambientLighting, lights, scene, camera, exposure, whiteBalance, width, height } =
    let
        givenDevicePixelRatio =
            getDevicePixelRatio options

        givenPointSize =
            getPointSize options

        aspectRatio =
            Quantity.ratio width height

        projectionParameters =
            Camera3d.projectionParameters { screenAspectRatio = aspectRatio }
                camera

        clipDistance =
            Math.Vector4.getX projectionParameters

        kc =
            Math.Vector4.getZ projectionParameters

        kz =
            Math.Vector4.getW projectionParameters

        eyePoint =
            Camera3d.viewpoint camera
                |> Viewpoint3d.eyePoint
                |> Point3d.unwrap

        projectionType =
            if kz == 0 then
                0

            else
                1

        (LinearRgb r g b) =
            Chromaticity.toLinearRgb whiteBalance

        maxLuminance =
            Luminance.inNits (Exposure.maxLuminance exposure)

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
                , m13 = maxLuminance * r
                , m23 = maxLuminance * g
                , m33 = maxLuminance * b
                , m43 = givenPointSize * givenDevicePixelRatio
                , m14 = 0
                , m24 = 0
                , m34 = 0
                , m44 = 0
                }

        viewMatrix =
            Camera3d.viewMatrix camera

        ambientLightingMatrix =
            case ambientLighting of
                Just (Types.AmbientLighting matrix) ->
                    matrix

                Nothing ->
                    ambientLightingDisabled

        (Types.Drawable rootNode) =
            scene

        renderPasses =
            collectRenderPasses
                sceneProperties
                viewMatrix
                ambientLightingMatrix
                Transformation.identity
                rootNode
                { meshes = []
                , shadows = []
                }
    in
    case lights of
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


render :
    List Option
    ->
        { ambientLighting : Maybe (AmbientLighting coordinates)
        , lights : Lights coordinates
        , scene : Drawable coordinates
        , camera : Camera3d Meters coordinates
        , exposure : Exposure
        , whiteBalance : Chromaticity
        , width : Quantity Float Pixels
        , height : Quantity Float Pixels
        }
    -> Html msg
render options arguments =
    let
        widthInPixels =
            inPixels arguments.width

        heightInPixels =
            inPixels arguments.height

        givenDevicePixelRatio =
            getDevicePixelRatio options

        givenClearColor =
            Color.toRgba (getClearColor options)

        webGLOptions =
            [ WebGL.depth 1
            , WebGL.stencil 0
            , WebGL.alpha True
            , WebGL.antialias
            , WebGL.clearColor
                givenClearColor.red
                givenClearColor.green
                givenClearColor.blue
                givenClearColor.alpha
            ]
    in
    WebGL.toHtmlWith webGLOptions
        [ Html.Attributes.width (round (givenDevicePixelRatio * widthInPixels))
        , Html.Attributes.height (round (givenDevicePixelRatio * heightInPixels))
        , Html.Attributes.style "width" (String.fromFloat widthInPixels ++ "px")
        , Html.Attributes.style "height" (String.fromFloat heightInPixels ++ "px")
        ]
        (toEntities options arguments)


unlit :
    List Option
    ->
        { scene : Drawable coordinates
        , camera : Camera3d Meters coordinates
        , width : Quantity Float Pixels
        , height : Quantity Float Pixels
        }
    -> Html msg
unlit options arguments =
    render options
        { scene = arguments.scene
        , camera = arguments.camera
        , width = arguments.width
        , height = arguments.height
        , ambientLighting = Nothing
        , lights = noLights
        , exposure = Exposure.srgb
        , whiteBalance = Chromaticity.daylight
        }



----- OPTIONS -----


type Option
    = DevicePixelRatio Float
    | Alpha Bool
    | ClearColor Color
    | PointSize Float


devicePixelRatio : Float -> Option
devicePixelRatio value =
    DevicePixelRatio value


pointSize : Quantity Float Pixels -> Option
pointSize (Quantity size) =
    PointSize size


clearColor : Color -> Option
clearColor color =
    ClearColor color


getDevicePixelRatio : List Option -> Float
getDevicePixelRatio options =
    let
        defaultValue =
            1.0

        update option oldValue =
            case option of
                DevicePixelRatio newValue ->
                    newValue

                _ ->
                    oldValue
    in
    List.foldl update defaultValue options


getPointSize : List Option -> Float
getPointSize options =
    let
        defaultValue =
            1.0

        update option oldValue =
            case option of
                PointSize newValue ->
                    newValue

                _ ->
                    oldValue
    in
    List.foldl update defaultValue options


getAlpha : List Option -> Bool
getAlpha options =
    let
        defaultValue =
            True

        update option oldValue =
            case option of
                Alpha newValue ->
                    newValue

                _ ->
                    oldValue
    in
    List.foldl update defaultValue options


getClearColor : List Option -> Color
getClearColor options =
    let
        defaultValue =
            Color.rgba 1.0 1.0 1.0 0.0

        update option oldValue =
            case option of
                ClearColor newValue ->
                    newValue

                _ ->
                    oldValue
    in
    List.foldl update defaultValue options
