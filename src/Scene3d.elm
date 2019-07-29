module Scene3d exposing
    ( RenderOption
    , alpha
    , antialias
    , clearColor
    , devicePixelRatio
    , gammaCorrection
    , render
    , renderWith
    , toEntities
    , toEntitiesWith
    )

import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import Pixels exposing (Pixels, inPixels)
import Rectangle2d
import Scene3d.Drawable exposing (Drawable)
import Scene3d.Light exposing (Light)
import Scene3d.Shader as Shader
import Scene3d.Transformation as Transformation exposing (Transformation)
import Scene3d.Types as Types
import Viewpoint3d
import WebGL
import WebGL.Settings
import WebGL.Settings.DepthTest
import WebGL.Texture exposing (Texture)


type alias AmbientProperties =
    { color : Vec3
    , lookupTexture : Texture
    }


type alias LightProperties =
    { lightType : Int
    , lightColor : Vec3
    , lightVector : Vec3
    , lightRadius : Float
    }


zeroVector : Vec3
zeroVector =
    Vector3.vec3 0 0 0


disabledLight : LightProperties
disabledLight =
    { lightType = -1
    , lightColor = zeroVector
    , lightVector = zeroVector
    , lightRadius = 0
    }


type alias PhysicallyBasedRenderer =
    List WebGL.Settings.Setting
    -> Vec3
    -> Float
    -> Mat4
    -> Mat4
    -> Float
    -> WebGL.Mesh Types.PhysicalAttributes
    -> WebGL.Entity


type PhysicallyBasedLighting
    = AmbientLighting AmbientProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | NoAmbientLighting LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | DummyLighting


type alias RenderProperties worldUnits worldCoordinates screenUnits screenCoordinates =
    { camera : Camera3d worldUnits worldCoordinates screenUnits screenCoordinates
    , eyePoint : Vec3
    , projectionMatrix : Mat4
    , physicallyBasedRenderer : PhysicallyBasedRenderer
    , gammaCorrection : Float
    }


physicallyBasedLighting : List (Light units coordinates) -> PhysicallyBasedLighting
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

                Types.PointLight pointLight ->
                    let
                        thisLight =
                            { lightType = 2
                            , lightColor = pointLight.color
                            , lightVector = pointLight.position
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
                    AmbientLighting ambientProperties disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight

                [ light1 ] ->
                    AmbientLighting ambientProperties light1 disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight

                [ light1, light2 ] ->
                    AmbientLighting ambientProperties light1 light2 disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight

                [ light1, light2, light3 ] ->
                    AmbientLighting ambientProperties light1 light2 light3 disabledLight disabledLight disabledLight disabledLight disabledLight

                [ light1, light2, light3, light4 ] ->
                    AmbientLighting ambientProperties light1 light2 light3 light4 disabledLight disabledLight disabledLight disabledLight

                [ light1, light2, light3, light4, light5 ] ->
                    AmbientLighting ambientProperties light1 light2 light3 light4 light5 disabledLight disabledLight disabledLight

                [ light1, light2, light3, light4, light5, light6 ] ->
                    AmbientLighting ambientProperties light1 light2 light3 light4 light5 light6 disabledLight disabledLight

                [ light1, light2, light3, light4, light5, light6, light7 ] ->
                    AmbientLighting ambientProperties light1 light2 light3 light4 light5 light6 light7 disabledLight

                [ light1, light2, light3, light4, light5, light6, light7, light8 ] ->
                    AmbientLighting ambientProperties light1 light2 light3 light4 light5 light6 light7 light8

                _ ->
                    DummyLighting

        Nothing ->
            case lightingState.lights of
                [] ->
                    NoAmbientLighting disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight

                [ light1 ] ->
                    NoAmbientLighting light1 disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight

                [ light1, light2 ] ->
                    NoAmbientLighting light1 light2 disabledLight disabledLight disabledLight disabledLight disabledLight disabledLight

                [ light1, light2, light3 ] ->
                    NoAmbientLighting light1 light2 light3 disabledLight disabledLight disabledLight disabledLight disabledLight

                [ light1, light2, light3, light4 ] ->
                    NoAmbientLighting light1 light2 light3 light4 disabledLight disabledLight disabledLight disabledLight

                [ light1, light2, light3, light4, light5 ] ->
                    NoAmbientLighting light1 light2 light3 light4 light5 disabledLight disabledLight disabledLight

                [ light1, light2, light3, light4, light5, light6 ] ->
                    NoAmbientLighting light1 light2 light3 light4 light5 light6 disabledLight disabledLight

                [ light1, light2, light3, light4, light5, light6, light7 ] ->
                    NoAmbientLighting light1 light2 light3 light4 light5 light6 light7 disabledLight

                [ light1, light2, light3, light4, light5, light6, light7, light8 ] ->
                    NoAmbientLighting light1 light2 light3 light4 light5 light6 light7 light8

                _ ->
                    DummyLighting


physicallyBasedRendererFor : List (Light units coordinates) -> PhysicallyBasedRenderer
physicallyBasedRendererFor lights =
    case physicallyBasedLighting lights of
        AmbientLighting ambientProperties light1 light2 light3 light4 light5 light6 light7 light8 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gamma mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gamma
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
                        , lightType5 = light5.lightType
                        , lightColor5 = light5.lightColor
                        , lightVector5 = light5.lightVector
                        , lightRadius5 = light5.lightRadius
                        , lightType6 = light6.lightType
                        , lightColor6 = light6.lightColor
                        , lightVector6 = light6.lightVector
                        , lightRadius6 = light6.lightRadius
                        , lightType7 = light7.lightType
                        , lightColor7 = light7.lightColor
                        , lightVector7 = light7.lightVector
                        , lightRadius7 = light7.lightRadius
                        , lightType8 = light8.lightType
                        , lightColor8 = light8.lightColor
                        , lightVector8 = light8.lightVector
                        , lightRadius8 = light8.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.ambientFragment mesh uniforms

        NoAmbientLighting light1 light2 light3 light4 light5 light6 light7 light8 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gamma mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gamma
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
                        , lightType5 = light5.lightType
                        , lightColor5 = light5.lightColor
                        , lightVector5 = light5.lightVector
                        , lightRadius5 = light5.lightRadius
                        , lightType6 = light6.lightType
                        , lightColor6 = light6.lightColor
                        , lightVector6 = light6.lightVector
                        , lightRadius6 = light6.lightRadius
                        , lightType7 = light7.lightType
                        , lightColor7 = light7.lightColor
                        , lightVector7 = light7.lightVector
                        , lightRadius7 = light7.lightRadius
                        , lightType8 = light8.lightType
                        , lightColor8 = light8.lightColor
                        , lightVector8 = light8.lightVector
                        , lightRadius8 = light8.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbientFragment mesh uniforms

        DummyLighting ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gamma mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , gammaCorrection = gamma
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.dummyFragment mesh uniforms


toEntity : RenderProperties worldUnits worldCoordinates screenUnits screenCoordinates -> Transformation -> Types.Mesh -> WebGL.Entity
toEntity renderProperties transformation mesh =
    let
        modelScale =
            transformation.scale

        placementFrame =
            Transformation.placementFrame transformation

        modelMatrix =
            Frame3d.toMat4 placementFrame

        modelViewMatrix =
            Camera3d.modelViewMatrix placementFrame renderProperties.camera

        projectionMatrix =
            renderProperties.projectionMatrix

        modelViewProjectionMatrix =
            Math.Matrix4.mul projectionMatrix modelViewMatrix

        cullSetting =
            if transformation.isRightHanded then
                WebGL.Settings.back

            else
                WebGL.Settings.front

        settings =
            [ WebGL.Settings.DepthTest.default
            , WebGL.Settings.cullFace cullSetting
            ]
    in
    case mesh of
        Types.SimpleMesh colorType boundingBox webGlMesh ->
            case colorType of
                Types.FlatColor ->
                    let
                        uniforms =
                            { modelScale = modelScale
                            , modelMatrix = modelMatrix
                            , modelViewProjectionMatrix = modelViewProjectionMatrix
                            }
                    in
                    WebGL.entityWith settings
                        Shader.simpleVertex
                        Shader.flatFragment
                        webGlMesh
                        uniforms

                Types.EmissiveColor ->
                    let
                        uniforms =
                            { modelScale = modelScale
                            , modelMatrix = modelMatrix
                            , modelViewProjectionMatrix = modelViewProjectionMatrix
                            , gammaCorrection = renderProperties.gammaCorrection
                            }
                    in
                    WebGL.entityWith settings
                        Shader.simpleVertex
                        Shader.emissiveFragment
                        webGlMesh
                        uniforms

        Types.PhysicalMesh boundingBox webGlMesh ->
            renderProperties.physicallyBasedRenderer
                settings
                renderProperties.eyePoint
                modelScale
                modelMatrix
                modelViewProjectionMatrix
                renderProperties.gammaCorrection
                webGlMesh


collectEntities : RenderProperties worldUnits worldCoordinates screenUnits screenCoordinates -> Transformation -> Types.Drawable_ -> List WebGL.Entity -> List WebGL.Entity
collectEntities renderProperties currentTransformation drawable_ accumulated =
    case drawable_ of
        Types.TransformedDrawable transformation childDrawable ->
            collectEntities renderProperties
                (Transformation.compose transformation currentTransformation)
                childDrawable
                accumulated

        Types.MeshDrawable mesh ->
            toEntity renderProperties currentTransformation mesh :: accumulated

        Types.DrawableGroup childDrawables ->
            List.foldl (collectEntities renderProperties currentTransformation)
                accumulated
                childDrawables

        Types.EmptyDrawable ->
            accumulated


toEntities : List (Light worldUnits worldCoordinates) -> Camera3d worldUnits worldCoordinates screenUnits screenCoordinates -> Drawable worldUnits worldCoordinates -> List WebGL.Entity
toEntities =
    toEntitiesWith []


toEntitiesWith : List RenderOption -> List (Light worldUnits worldCoordinates) -> Camera3d worldUnits worldCoordinates screenUnits screenCoordinates -> Drawable worldUnits worldCoordinates -> List WebGL.Entity
toEntitiesWith options lights camera (Types.Drawable rootDrawable_) =
    let
        renderProperties =
            { camera = camera
            , eyePoint =
                Point3d.toVec3 (Viewpoint3d.eyePoint (Camera3d.viewpoint camera))
            , projectionMatrix = Camera3d.projectionMatrix camera
            , physicallyBasedRenderer = physicallyBasedRendererFor lights
            , gammaCorrection = getGammaCorrection options
            }
    in
    collectEntities renderProperties Transformation.identity rootDrawable_ []


render : List (Light worldUnits worldCoordinates) -> Camera3d worldUnits worldCoordinates Pixels screenCoordinates -> Drawable worldUnits worldCoordinates -> Html msg
render =
    renderWith []


type RenderOption
    = DevicePixelRatio Float
    | GammaCorrection Float
    | Antialias Bool
    | Alpha Bool
    | ClearColor Color


devicePixelRatio : Float -> RenderOption
devicePixelRatio =
    DevicePixelRatio


gammaCorrection : Float -> RenderOption
gammaCorrection =
    GammaCorrection


antialias : Bool -> RenderOption
antialias =
    Antialias


alpha : Bool -> RenderOption
alpha =
    Alpha


clearColor : Color -> RenderOption
clearColor =
    ClearColor


getDevicePixelRatio : List RenderOption -> Float
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


getGammaCorrection : List RenderOption -> Float
getGammaCorrection options =
    let
        defaultValue =
            0.45

        update option oldValue =
            case option of
                GammaCorrection newValue ->
                    newValue

                _ ->
                    oldValue
    in
    List.foldl update defaultValue options


getAntialias : List RenderOption -> Bool
getAntialias options =
    let
        defaultValue =
            True

        update option oldValue =
            case option of
                Antialias newValue ->
                    newValue

                _ ->
                    oldValue
    in
    List.foldl update defaultValue options


getAlpha : List RenderOption -> Bool
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


getClearColor : List RenderOption -> Color
getClearColor options =
    let
        defaultValue =
            Color.rgba 0 0 0 0.0

        update option oldValue =
            case option of
                ClearColor newValue ->
                    newValue

                _ ->
                    oldValue
    in
    List.foldl update defaultValue options


renderWith : List RenderOption -> List (Light worldUnits worldCoordinates) -> Camera3d worldUnits worldCoordinates Pixels screenCoordinates -> Drawable worldUnits worldCoordinates -> Html msg
renderWith options lights camera rootNode =
    let
        ( width, height ) =
            Rectangle2d.dimensions (Camera3d.screen camera)

        widthInPixels =
            inPixels width

        heightInPixels =
            inPixels height

        givenDevicePixelRatio =
            getDevicePixelRatio options

        givenAntialias =
            getAntialias options

        givenAlpha =
            getAlpha options

        givenClearColor =
            Color.toRgba (getClearColor options)

        clearColorOption =
            WebGL.clearColor
                givenClearColor.red
                givenClearColor.green
                givenClearColor.blue
                givenClearColor.alpha

        commonOptions =
            [ WebGL.depth 1, WebGL.alpha givenAlpha, clearColorOption ]

        webGLOptions =
            if givenAntialias then
                WebGL.antialias :: commonOptions

            else
                commonOptions
    in
    WebGL.toHtmlWith webGLOptions
        [ Html.Attributes.width (round (givenDevicePixelRatio * widthInPixels))
        , Html.Attributes.height (round (givenDevicePixelRatio * heightInPixels))
        , Html.Attributes.style "width" (String.fromFloat widthInPixels ++ "px")
        , Html.Attributes.style "height" (String.fromFloat heightInPixels ++ "px")
        ]
        (toEntitiesWith options lights camera rootNode)
