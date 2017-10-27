module OpenSolid.Scene
    exposing
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

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 as Vector3 exposing (Vec3)
import OpenSolid.Camera as Camera exposing (Camera)
import OpenSolid.Interop.LinearAlgebra.Frame3d as Frame3d
import OpenSolid.Interop.LinearAlgebra.Point3d as Point3d
import OpenSolid.Scene.Drawable exposing (Drawable)
import OpenSolid.Scene.Light exposing (Light)
import OpenSolid.Scene.Placement as Placement exposing (Placement)
import OpenSolid.Scene.Shader as Shader
import OpenSolid.Scene.Types as Types
import OpenSolid.Viewpoint as Viewpoint
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
    = AmbientOnlyLighting AmbientProperties
    | AmbientLighting1 AmbientProperties LightProperties
    | AmbientLighting2 AmbientProperties LightProperties LightProperties
    | AmbientLighting3 AmbientProperties LightProperties LightProperties LightProperties
    | AmbientLighting4 AmbientProperties LightProperties LightProperties LightProperties LightProperties
    | AmbientLighting5 AmbientProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | AmbientLighting6 AmbientProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | AmbientLighting7 AmbientProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | AmbientLighting8 AmbientProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | NoAmbientLighting1 LightProperties
    | NoAmbientLighting2 LightProperties LightProperties
    | NoAmbientLighting3 LightProperties LightProperties LightProperties
    | NoAmbientLighting4 LightProperties LightProperties LightProperties LightProperties
    | NoAmbientLighting5 LightProperties LightProperties LightProperties LightProperties LightProperties
    | NoAmbientLighting6 LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | NoAmbientLighting7 LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | NoAmbientLighting8 LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties LightProperties
    | DummyLighting


type alias RenderProperties =
    { camera : Camera
    , eyePoint : Vec3
    , projectionMatrix : Mat4
    , physicallyBasedRenderer : PhysicallyBasedRenderer
    , gammaCorrection : Float
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
                    AmbientOnlyLighting ambientProperties

                [ light1 ] ->
                    AmbientLighting1 ambientProperties light1

                [ light1, light2 ] ->
                    AmbientLighting2 ambientProperties light1 light2

                [ light1, light2, light3 ] ->
                    AmbientLighting3 ambientProperties light1 light2 light3

                [ light1, light2, light3, light4 ] ->
                    AmbientLighting4 ambientProperties light1 light2 light3 light4

                [ light1, light2, light3, light4, light5 ] ->
                    AmbientLighting5 ambientProperties light1 light2 light3 light4 light5

                [ light1, light2, light3, light4, light5, light6 ] ->
                    AmbientLighting6 ambientProperties light1 light2 light3 light4 light5 light6

                [ light1, light2, light3, light4, light5, light6, light7 ] ->
                    AmbientLighting7 ambientProperties light1 light2 light3 light4 light5 light6 light7

                [ light1, light2, light3, light4, light5, light6, light7, light8 ] ->
                    AmbientLighting8 ambientProperties light1 light2 light3 light4 light5 light6 light7 light8

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

                [ light1, light2, light3, light4, light5 ] ->
                    NoAmbientLighting5 light1 light2 light3 light4 light5

                [ light1, light2, light3, light4, light5, light6 ] ->
                    NoAmbientLighting6 light1 light2 light3 light4 light5 light6

                [ light1, light2, light3, light4, light5, light6, light7 ] ->
                    NoAmbientLighting7 light1 light2 light3 light4 light5 light6 light7

                [ light1, light2, light3, light4, light5, light6, light7, light8 ] ->
                    NoAmbientLighting8 light1 light2 light3 light4 light5 light6 light7 light8

                _ ->
                    DummyLighting


physicallyBasedRendererFor : List Light -> PhysicallyBasedRenderer
physicallyBasedRendererFor lights =
    case physicallyBasedLighting lights of
        AmbientOnlyLighting ambientProperties ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
                        , ambientLightColor = ambientProperties.color
                        , ambientLookupTexture = ambientProperties.lookupTexture
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.ambientOnlyFragment mesh uniforms

        AmbientLighting1 ambientProperties light1 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
                        , ambientLightColor = ambientProperties.color
                        , ambientLookupTexture = ambientProperties.lookupTexture
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.ambient1Fragment mesh uniforms

        AmbientLighting2 ambientProperties light1 light2 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                WebGL.entityWith settings Shader.physicalVertex Shader.ambient2Fragment mesh uniforms

        AmbientLighting3 ambientProperties light1 light2 light3 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                WebGL.entityWith settings Shader.physicalVertex Shader.ambient3Fragment mesh uniforms

        AmbientLighting4 ambientProperties light1 light2 light3 light4 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                WebGL.entityWith settings Shader.physicalVertex Shader.ambient4Fragment mesh uniforms

        AmbientLighting5 ambientProperties light1 light2 light3 light4 light5 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.ambient5Fragment mesh uniforms

        AmbientLighting6 ambientProperties light1 light2 light3 light4 light5 light6 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.ambient6Fragment mesh uniforms

        AmbientLighting7 ambientProperties light1 light2 light3 light4 light5 light6 light7 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.ambient7Fragment mesh uniforms

        AmbientLighting8 ambientProperties light1 light2 light3 light4 light5 light6 light7 light8 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                WebGL.entityWith settings Shader.physicalVertex Shader.ambient8Fragment mesh uniforms

        NoAmbientLighting1 light1 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
                        , lightType1 = light1.lightType
                        , lightColor1 = light1.lightColor
                        , lightVector1 = light1.lightVector
                        , lightRadius1 = light1.lightRadius
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbient1Fragment mesh uniforms

        NoAmbientLighting2 light1 light2 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbient2Fragment mesh uniforms

        NoAmbientLighting3 light1 light2 light3 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbient3Fragment mesh uniforms

        NoAmbientLighting4 light1 light2 light3 light4 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbient4Fragment mesh uniforms

        NoAmbientLighting5 light1 light2 light3 light4 light5 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbient5Fragment mesh uniforms

        NoAmbientLighting6 light1 light2 light3 light4 light5 light6 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbient6Fragment mesh uniforms

        NoAmbientLighting7 light1 light2 light3 light4 light5 light6 light7 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbient7Fragment mesh uniforms

        NoAmbientLighting8 light1 light2 light3 light4 light5 light6 light7 light8 ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , eyePoint = eyePoint
                        , gammaCorrection = gammaCorrection
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
                WebGL.entityWith settings Shader.physicalVertex Shader.noAmbient8Fragment mesh uniforms

        DummyLighting ->
            \settings eyePoint modelScale modelMatrix modelViewProjectionMatrix gammaCorrection mesh ->
                let
                    uniforms =
                        { modelScale = modelScale
                        , modelMatrix = modelMatrix
                        , modelViewProjectionMatrix = modelViewProjectionMatrix
                        , gammaCorrection = gammaCorrection
                        }
                in
                WebGL.entityWith settings Shader.physicalVertex Shader.dummyFragment mesh uniforms


toEntity : RenderProperties -> Placement -> Types.Mesh -> WebGL.Entity
toEntity renderProperties placement mesh =
    let
        modelScale =
            Placement.scale placement

        placementFrame =
            Placement.frame placement

        modelMatrix =
            Frame3d.toMat4 placementFrame

        modelViewMatrix =
            Camera.modelViewMatrix renderProperties.camera placementFrame

        projectionMatrix =
            renderProperties.projectionMatrix

        modelViewProjectionMatrix =
            Math.Matrix4.mul projectionMatrix modelViewMatrix

        cullSetting =
            if Placement.isRightHanded placement then
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


collectEntities : RenderProperties -> Placement -> Drawable -> List WebGL.Entity -> List WebGL.Entity
collectEntities renderProperties currentPlacement node accumulated =
    case node of
        Types.TransformedDrawable placement childDrawable ->
            collectEntities renderProperties
                (Placement.compose placement currentPlacement)
                childDrawable
                accumulated

        Types.MeshDrawable mesh ->
            toEntity renderProperties currentPlacement mesh :: accumulated

        Types.DrawableGroup childDrawables ->
            List.foldl (collectEntities renderProperties currentPlacement)
                accumulated
                childDrawables

        Types.EmptyDrawable ->
            accumulated


toEntities : List Light -> Camera -> Drawable -> List WebGL.Entity
toEntities =
    toEntitiesWith []


toEntitiesWith : List RenderOption -> List Light -> Camera -> Drawable -> List WebGL.Entity
toEntitiesWith options lights camera rootDrawable =
    let
        renderProperties =
            { camera = camera
            , eyePoint =
                Point3d.toVec3 (Viewpoint.eyePoint (Camera.viewpoint camera))
            , projectionMatrix = Camera.projectionMatrix camera
            , physicallyBasedRenderer = physicallyBasedRendererFor lights
            , gammaCorrection = getGammaCorrection options
            }
    in
    collectEntities renderProperties Placement.identity rootDrawable []


render : List Light -> Camera -> Drawable -> Html msg
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


renderWith : List RenderOption -> List Light -> Camera -> Drawable -> Html msg
renderWith options lights camera rootNode =
    let
        width =
            Camera.screenWidth camera

        height =
            Camera.screenHeight camera

        devicePixelRatio =
            getDevicePixelRatio options

        antialias =
            getAntialias options

        alpha =
            getAlpha options

        clearColor =
            Color.toRgb (getClearColor options)

        clearColorOption =
            WebGL.clearColor
                (toFloat clearColor.red / 255)
                (toFloat clearColor.green / 255)
                (toFloat clearColor.blue / 255)
                clearColor.alpha

        commonOptions =
            [ WebGL.depth 1, WebGL.alpha alpha, clearColorOption ]

        webGLOptions =
            if antialias then
                WebGL.antialias :: commonOptions
            else
                commonOptions
    in
    WebGL.toHtmlWith webGLOptions
        [ Html.Attributes.width (round (devicePixelRatio * width))
        , Html.Attributes.height (round (devicePixelRatio * height))
        , Html.Attributes.style
            [ ( "width", toString width ++ "px" )
            , ( "height", toString height ++ "px" )
            ]
        ]
        (toEntitiesWith options lights camera rootNode)
