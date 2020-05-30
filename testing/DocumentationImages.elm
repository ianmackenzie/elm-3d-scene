module DocumentationImages exposing (main)

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d
import Browser
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Debug.Control as Control exposing (Control)
import Direction3d exposing (Direction3d)
import Frame3d
import Html exposing (Html)
import Http
import Illuminance exposing (Illuminance, Lux)
import Length exposing (Length, Meters)
import LuminousFlux exposing (Lumens, LuminousFlux)
import Maybe.Extra as Maybe
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Unitless)
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Scene3d.Mesh.Decode as Decode
import SketchPlane3d exposing (SketchPlane3d)
import Sphere3d exposing (Sphere3d)
import Task
import Temperature exposing (Temperature)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL.Texture


type WorldCoordinates
    = WorldCoordinates


type alias LightingParameters =
    { soft : Bool
    , point : Bool
    , directional : Bool
    }


type EntityType
    = Duckling Bool DucklingMaterial
    | Sphere SphereMaterial


type UntexturedMaterial
    = ConstantColor
    | Matte
    | Pbr { roughness : Float, metallic : Float }


type DucklingMaterial
    = UntexturedDuckling UntexturedMaterial
    | TexturedColorDuckling
    | TexturedMatteDuckling
    | TexturedPbrDuckling { roughness : Float, metallic : Float }


type SphereMaterial
    = UntexturedSphere UntexturedMaterial
    | TexturedColorSphere
    | TexturedMatteSphere
    | TexturedPbrSphere
        { baseColor : SphereColor
        , roughness : SphereParameter
        , metallic : SphereParameter
        }


type SphereColor
    = TexturedSphereColor
    | ConstantSphereColor


type SphereParameter
    = TexturedSphereParameter
    | ConstantSphereParameter Float


type ProjectionType
    = Perspective
    | Orthographic


type ToneMappingType
    = NoToneMapping
    | ReinhardToneMapping Float
    | ReinhardPerChannelToneMapping Float
    | HableFilmicToneMapping


type alias Parameters =
    { entity : EntityType
    , projection : ProjectionType
    , lighting : LightingParameters
    , exposureValue : Float
    , shadows : Bool
    , toneMapping : ToneMappingType
    }


type alias LoadingModel =
    { flatMesh : Maybe (Mesh.Textured WorldCoordinates)
    , smoothMesh : Maybe (Mesh.Textured WorldCoordinates)
    , shadow : Maybe (Mesh.Shadow WorldCoordinates)
    , ducklingTexture : Maybe (Material.Texture Color)
    , colorTexture : Maybe (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , metallicTexture : Maybe (Material.Texture Float)
    }


type alias LoadedModel =
    { flatMesh : Mesh.Textured WorldCoordinates
    , smoothMesh : Mesh.Textured WorldCoordinates
    , shadow : Mesh.Shadow WorldCoordinates
    , ducklingTexture : Material.Texture Color
    , colorTexture : Material.Texture Color
    , roughnessTexture : Material.Texture Float
    , metallicTexture : Material.Texture Float
    , control : Control Parameters
    }


type Model
    = Loading LoadingModel
    | Error String
    | Loaded LoadedModel


type Msg
    = SmoothMeshResponse (Result Http.Error (Mesh.Textured WorldCoordinates))
    | FlatMeshResponse (Result Http.Error (Mesh.Textured WorldCoordinates))
    | ShadowResponse (Result Http.Error (Mesh.Shadow WorldCoordinates))
    | DucklingTextureResponse (Result WebGL.Texture.Error (Material.Texture Color))
    | ColorTextureResponse (Result WebGL.Texture.Error (Material.Texture Color))
    | RoughnessTextureResponse (Result WebGL.Texture.Error (Material.Texture Float))
    | MetallicTextureResponse (Result WebGL.Texture.Error (Material.Texture Float))
    | UpdatedControl (Control Parameters)


init : () -> ( Model, Cmd Msg )
init () =
    ( Loading
        { ducklingTexture = Nothing
        , colorTexture = Nothing
        , roughnessTexture = Nothing
        , metallicTexture = Nothing
        , flatMesh = Nothing
        , smoothMesh = Nothing
        , shadow = Nothing
        }
    , Cmd.batch
        [ Material.loadWith Material.trilinearFiltering
            "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling.png"
            |> Task.attempt DucklingTextureResponse
        , Material.load
            "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_col.jpg"
            |> Task.attempt ColorTextureResponse
        , Material.load
            "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_rgh.jpg"
            |> Task.attempt RoughnessTextureResponse
        , Material.load
            "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_met.jpg"
            |> Task.attempt MetallicTextureResponse
        , Http.get
            { url = "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling.json"
            , expect = Http.expectJson SmoothMeshResponse Decode.texturedFaces
            }
        , Http.get
            { url = "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling.json"
            , expect = Http.expectJson FlatMeshResponse Decode.texturedFacets
            }
        , Http.get
            { url = "https://ianmackenzie.github.io/elm-3d-scene/examples/duckling-shadow.json"
            , expect = Http.expectJson ShadowResponse Decode.shadow
            }
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FlatMeshResponse (Ok flatMesh) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | flatMesh = Just flatMesh }, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        FlatMeshResponse (Err _) ->
            ( Error "Flat mesh request failed", Cmd.none )

        SmoothMeshResponse (Ok smoothMesh) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | smoothMesh = Just smoothMesh }, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        SmoothMeshResponse (Err _) ->
            ( Error "Smooth mesh request failed", Cmd.none )

        ShadowResponse (Ok shadow) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | shadow = Just shadow }, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        ShadowResponse (Err _) ->
            ( Error "Shadow request failed", Cmd.none )

        DucklingTextureResponse (Ok texture) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | ducklingTexture = Just texture }, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        DucklingTextureResponse (Err errorMessage) ->
            ( Error "Duckling texture request failed", Cmd.none )

        ColorTextureResponse (Ok texture) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | colorTexture = Just texture }, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        ColorTextureResponse (Err errorMessage) ->
            ( Error "Color texture request failed", Cmd.none )

        RoughnessTextureResponse (Ok texture) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | roughnessTexture = Just texture }, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        RoughnessTextureResponse (Err errorMessage) ->
            ( Error "Roughness texture request failed", Cmd.none )

        MetallicTextureResponse (Ok texture) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | metallicTexture = Just texture }, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded _ ->
                    ( model, Cmd.none )

        MetallicTextureResponse (Err errorMessage) ->
            ( Error "Metallic texture request failed", Cmd.none )

        UpdatedControl updatedControl ->
            case model of
                Loading _ ->
                    ( model, Cmd.none )

                Error _ ->
                    ( model, Cmd.none )

                Loaded loadedModel ->
                    ( Loaded { loadedModel | control = updatedControl }, Cmd.none )


checkIfLoaded : LoadingModel -> Model
checkIfLoaded loadingModel =
    Just
        (\flatMesh smoothMesh shadow ducklingTexture colorTexture roughnessTexture metallicTexture ->
            Loaded
                { flatMesh = flatMesh
                , smoothMesh = smoothMesh
                , shadow = shadow
                , ducklingTexture = ducklingTexture
                , colorTexture = colorTexture
                , roughnessTexture = roughnessTexture
                , metallicTexture = metallicTexture
                , control = initialControl
                }
        )
        |> Maybe.andMap loadingModel.flatMesh
        |> Maybe.andMap loadingModel.smoothMesh
        |> Maybe.andMap loadingModel.shadow
        |> Maybe.andMap loadingModel.ducklingTexture
        |> Maybe.andMap loadingModel.colorTexture
        |> Maybe.andMap loadingModel.roughnessTexture
        |> Maybe.andMap loadingModel.metallicTexture
        |> Maybe.withDefault (Loading loadingModel)


initialControl : Control Parameters
initialControl =
    Control.record Parameters
        |> Control.field "Entity" entityControl
        |> Control.field "Projection" projectionControl
        |> Control.field "Lighting" lightingControl
        |> Control.field "Exposure" (Control.float { min = 3, max = 15 } 8)
        |> Control.field "Shadows" (Control.bool True)
        |> Control.field "Tone mapping" toneMappingControl


projectionControl : Control ProjectionType
projectionControl =
    Control.choice
        [ ( "Perspective", Control.value Perspective )
        , ( "Orthographic", Control.value Orthographic )
        ]


entityControl : Control EntityType
entityControl =
    Control.choice
        [ ( "Duckling (smooth)", Control.map (Duckling True) ducklingMaterialControl )
        , ( "Duckling (flat)", Control.map (Duckling False) ducklingMaterialControl )
        , ( "Sphere", Control.map Sphere sphereMaterialControl )
        ]


untexturedMaterialControl : Control UntexturedMaterial
untexturedMaterialControl =
    Control.choice
        [ ( "Constant color", Control.value ConstantColor )
        , ( "Matte", Control.value Matte )
        , ( "PBR"
          , Control.record (\roughness metallic -> Pbr { roughness = roughness, metallic = metallic })
                |> Control.field "Roughness" (Control.float { min = 0, max = 1 } 0.5)
                |> Control.field "Metallic" (Control.float { min = 0, max = 1 } 0)
          )
        ]


ducklingMaterialControl : Control DucklingMaterial
ducklingMaterialControl =
    Control.choice
        [ ( "Untextured", Control.map UntexturedDuckling untexturedMaterialControl )
        , ( "Textured color", Control.value TexturedColorDuckling )
        , ( "Textured matte", Control.value TexturedMatteDuckling )
        , ( "Textured PBR"
          , Control.record (\roughness metallic -> TexturedPbrDuckling { roughness = roughness, metallic = metallic })
                |> Control.field "Roughness" (Control.float { min = 0, max = 1 } 0.5)
                |> Control.field "Metallic" (Control.float { min = 0, max = 1 } 0)
          )
        ]


sphereMaterialControl : Control SphereMaterial
sphereMaterialControl =
    Control.choice
        [ ( "Untextured", Control.map UntexturedSphere untexturedMaterialControl )
        , ( "Textured color", Control.value TexturedColorSphere )
        , ( "Textured matte", Control.value TexturedMatteSphere )
        , ( "Textured PBR"
          , Control.record
                (\baseColor roughness metallic ->
                    TexturedPbrSphere { baseColor = baseColor, roughness = roughness, metallic = metallic }
                )
                |> Control.field "Base color" sphereColorControl
                |> Control.field "Roughness" sphereParameterControl
                |> Control.field "Metallic" sphereParameterControl
          )
        ]


sphereColorControl : Control SphereColor
sphereColorControl =
    Control.choice
        [ ( "Constant", Control.value ConstantSphereColor )
        , ( "Textured", Control.value TexturedSphereColor )
        ]


sphereParameterControl : Control SphereParameter
sphereParameterControl =
    Control.choice
        [ ( "Constant", Control.map ConstantSphereParameter (Control.float { min = 0, max = 1 } 0.5) )
        , ( "Textured", Control.value TexturedSphereParameter )
        ]


toneMappingControl : Control ToneMappingType
toneMappingControl =
    let
        parameterControl =
            Control.float { min = 1, max = 10 } 2
    in
    Control.choice
        [ ( "No tone mapping", Control.value NoToneMapping )
        , ( "Reinhard", Control.map ReinhardToneMapping parameterControl )
        , ( "Reinhard per channel", Control.map ReinhardToneMapping parameterControl )
        , ( "Hable filmic", Control.value HableFilmicToneMapping )
        ]


lightingControl : Control LightingParameters
lightingControl =
    Control.record LightingParameters
        |> Control.field "Soft" (Control.bool True)
        |> Control.field "Point" (Control.bool True)
        |> Control.field "Directional" (Control.bool True)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            Html.text "Loading..."

        Error errorMessage ->
            Html.text errorMessage

        Loaded loadedModel ->
            Html.div []
                [ viewScene loadedModel
                , viewControl loadedModel.control
                ]


viewScene : LoadedModel -> Html Msg
viewScene model =
    let
        parameters =
            Control.currentValue model.control
    in
    Scene3d.custom
        { dimensions = ( Pixels.pixels 300, Pixels.pixels 300 )
        , background = Scene3d.transparentBackground
        , clipDepth = Length.meters 1
        , camera = camera parameters
        , lights = lights parameters
        , entities = entities model parameters
        , antialiasing = Scene3d.multisampling
        , exposure = Scene3d.exposureValue parameters.exposureValue
        , toneMapping = toneMapping parameters.toneMapping
        , whiteBalance = Light.incandescent
        }


camera : Parameters -> Camera3d Meters WorldCoordinates
camera parameters =
    let
        eyePoint =
            Point3d.meters 10 5 7

        verticalFieldOfView =
            Angle.degrees 30

        focalDistance =
            Point3d.distanceFrom eyePoint Point3d.origin

        viewportHeight =
            focalDistance
                |> Quantity.multiplyBy
                    (2 * Angle.tan (Quantity.half verticalFieldOfView))

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }
    in
    case parameters.projection of
        Perspective ->
            Camera3d.perspective
                { viewpoint = viewpoint
                , verticalFieldOfView = verticalFieldOfView
                }

        Orthographic ->
            Camera3d.orthographic
                { viewpoint = viewpoint
                , viewportHeight = viewportHeight
                }


lights : Parameters -> Scene3d.Lights WorldCoordinates
lights parameters =
    let
        pointLight =
            Light.point (Light.castsShadows parameters.shadows)
                { chromaticity = Light.fluorescent
                , position = Point3d.meters 0 -4 4
                , intensity = LuminousFlux.lumens 400000
                }

        directionalLight =
            Light.directional (Light.castsShadows parameters.shadows)
                { chromaticity = Light.colorTemperature (Temperature.kelvins 2200)
                , intensity = Illuminance.lux 1200
                , direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -30)
                }

        softLighting =
            Light.soft
                { upDirection = Direction3d.z
                , chromaticity = Light.incandescent
                , intensityAbove = Illuminance.lux 100
                , intensityBelow = Illuminance.lux 20
                }
    in
    case ( parameters.lighting.point, parameters.lighting.directional, parameters.lighting.soft ) of
        ( False, False, False ) ->
            Scene3d.noLights

        ( True, False, False ) ->
            Scene3d.oneLight pointLight

        ( False, True, False ) ->
            Scene3d.oneLight directionalLight

        ( True, True, False ) ->
            Scene3d.twoLights pointLight directionalLight

        ( False, False, True ) ->
            Scene3d.oneLight softLighting

        ( True, False, True ) ->
            Scene3d.twoLights pointLight softLighting

        ( False, True, True ) ->
            Scene3d.twoLights directionalLight softLighting

        ( True, True, True ) ->
            Scene3d.threeLights pointLight directionalLight softLighting


untexturedMaterial : UntexturedMaterial -> Color -> Material.Textured WorldCoordinates
untexturedMaterial untexturedMaterialType defaultColor =
    case untexturedMaterialType of
        ConstantColor ->
            Material.color defaultColor

        Matte ->
            Material.matte defaultColor

        Pbr { roughness, metallic } ->
            Material.pbr
                { baseColor = defaultColor
                , roughness = roughness
                , metallic = metallic
                }


entities : LoadedModel -> Parameters -> List (Scene3d.Entity WorldCoordinates)
entities model parameters =
    [ mainEntity model parameters ]


mainEntity : LoadedModel -> Parameters -> Scene3d.Entity WorldCoordinates
mainEntity model parameters =
    case parameters.entity of
        Duckling smooth materialType ->
            let
                material =
                    case materialType of
                        UntexturedDuckling untexturedMaterialType ->
                            untexturedMaterial untexturedMaterialType Color.blue

                        TexturedColorDuckling ->
                            Material.texturedColor model.ducklingTexture

                        TexturedMatteDuckling ->
                            Material.texturedMatte model.ducklingTexture

                        TexturedPbrDuckling { roughness, metallic } ->
                            Material.texturedPbr
                                { baseColor = model.ducklingTexture
                                , roughness = Material.constant roughness
                                , metallic = Material.constant metallic
                                }

                transform =
                    Scene3d.rotateAround Axis3d.x (Angle.degrees 90)
                        >> Scene3d.translateIn Direction3d.negativeZ (Length.meters 0.67)
                        >> Scene3d.scaleAbout Point3d.origin 3
            in
            transform <|
                if smooth then
                    if parameters.shadows then
                        Scene3d.meshWithShadow material model.smoothMesh model.shadow

                    else
                        Scene3d.mesh material model.smoothMesh

                else if parameters.shadows then
                    Scene3d.meshWithShadow material model.flatMesh model.shadow

                else
                    Scene3d.mesh material model.flatMesh

        Sphere materialType ->
            let
                sphere =
                    Sphere3d.withRadius (Length.meters 2.5) Point3d.origin

                defaultColor =
                    Color.rgb255 233 235 236

                material =
                    case materialType of
                        UntexturedSphere untexturedMaterialType ->
                            untexturedMaterial untexturedMaterialType Color.blue

                        TexturedColorSphere ->
                            Material.texturedColor model.colorTexture

                        TexturedMatteSphere ->
                            Material.texturedMatte model.colorTexture

                        TexturedPbrSphere { baseColor, roughness, metallic } ->
                            let
                                baseColorTexture =
                                    case baseColor of
                                        TexturedSphereColor ->
                                            model.colorTexture

                                        ConstantSphereColor ->
                                            Material.constant defaultColor

                                roughnessTexture =
                                    case roughness of
                                        TexturedSphereParameter ->
                                            model.roughnessTexture

                                        ConstantSphereParameter value ->
                                            Material.constant value

                                metallicTexture =
                                    case metallic of
                                        TexturedSphereParameter ->
                                            model.metallicTexture

                                        ConstantSphereParameter value ->
                                            Material.constant value
                            in
                            Material.texturedPbr
                                { baseColor = baseColorTexture
                                , roughness = roughnessTexture
                                , metallic = metallicTexture
                                }
            in
            if parameters.shadows then
                Scene3d.sphereWithShadow material sphere

            else
                Scene3d.sphere material sphere


toneMapping : ToneMappingType -> Scene3d.ToneMapping
toneMapping toneMappingType =
    case toneMappingType of
        NoToneMapping ->
            Scene3d.noToneMapping

        ReinhardToneMapping value ->
            Scene3d.reinhardToneMapping value

        ReinhardPerChannelToneMapping value ->
            Scene3d.reinhardPerChannelToneMapping value

        HableFilmicToneMapping ->
            Scene3d.hableFilmicToneMapping


viewControl : Control Parameters -> Html Msg
viewControl currentControl =
    Control.view UpdatedControl currentControl
