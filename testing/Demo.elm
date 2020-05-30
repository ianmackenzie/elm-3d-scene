module Demo exposing (main)

import Angle exposing (Angle)
import Axis3d
import Block3d
import Browser
import Browser.Dom
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Cone3d
import Cylinder3d
import Direction3d exposing (Direction3d)
import Frame3d exposing (Frame3d)
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode
import Length exposing (Length, Meters, meters, millimeters)
import Luminance
import LuminousFlux
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Round
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d
import Task
import Temperature
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( initialModel
                , Task.perform
                    (\{ viewport } ->
                        Resize ( Pixels.pixels (viewport.width - panelWidth), Pixels.pixels viewport.height )
                    )
                    Browser.Dom.getViewport
                )
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions =
            \_ ->
                Browser.Events.onResize
                    (\width height ->
                        Resize ( Pixels.pixels (toFloat width - panelWidth), Pixels.pixels (toFloat height) )
                    )
        , view = view
        }



-- MODEL


type alias Model =
    { dimensions : ( Quantity Float Pixels, Quantity Float Pixels )

    -- Camera:
    , distance : Length
    , azimuth : Angle
    , elevation : Angle
    , orbiting : Bool

    -- Options:
    , multisampling : Bool
    , supersampling : Float
    , dynamicRange : Float
    , exposure : Float
    , backgroundColor : String

    -- Environmental lighting:
    , environmentalLighting : Bool
    , environmentalLuminanceAbove : Float
    , environmentalTemperatureAbove : Float
    , environmentalLuminanceBelow : Float
    , environmentalTemperatureBelow : Float

    -- Direct lights:
    , lights : List DirectLight

    -- Objects:
    , objects : List Object
    }


type Msg
    = Resize ( Quantity Float Pixels, Quantity Float Pixels )
    | Zoom Float
    | MouseDown
    | MouseMove Float Float
    | MouseUp
      -- Options:
    | OnMultisamplingToggled Bool
    | OnSupersamplingChanged Float
    | OnDynamicRangeChanged Float
    | OnBackgroundColorChanged String
    | OnExposureChanged Float
      -- Environmental lighting:
    | OnEnvironmentalLightingToggled Bool
    | OnEnvironmentalLuminanceAboveChanged Float
    | OnEnvironmentalTemperatureAboveChanged Float
    | OnEnvironmentalLuminanceBelowChanged Float
    | OnEnvironmentalTemperatureBelowChanged Float
      --  Direct lights:
    | OnDirectLightTemperatureChanged Int Float
    | OnDirectLightCastsShadowsToggled Int Bool
    | OnDirectLightIntensityChanged Int Float
      -- Objects:
    | OnObjectMaterialChanged Int Material
    | OnObjectColorChanged Int String
    | OnObjectRoughnessChanged Int Float


initialModel : Model
initialModel =
    { dimensions = ( Quantity.zero, Quantity.zero )

    -- Camera:
    , distance = Length.meters 4.7
    , azimuth = Angle.degrees 0
    , elevation = Angle.degrees 20
    , orbiting = False

    -- Options:
    , multisampling = True
    , supersampling = 1
    , dynamicRange = 1
    , exposure = 13
    , backgroundColor = "#FFFFFF"

    -- Environmental lighting:
    , environmentalLighting = True
    , environmentalLuminanceAbove = 5000
    , environmentalTemperatureAbove = 5600
    , environmentalLuminanceBelow = 0
    , environmentalTemperatureBelow = 5600

    -- Direct lights:
    , lights =
        [ { kind = Sun (Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60))
          , castsShadows = True
          , intensity = 10000
          , temperature = 5600
          }
        , { kind = Bulb (Point3d.centimeters 0 -30 140)
          , castsShadows = False
          , intensity = 1000
          , temperature = 5600
          }
        , { kind = Bulb (Point3d.centimeters 0 0 120)
          , castsShadows = False
          , intensity = 500
          , temperature = 5600
          }
        , { kind = Bulb (Point3d.centimeters 0 30 130)
          , castsShadows = False
          , intensity = 0
          , temperature = 5600
          }
        ]

    -- Objects:
    , objects =
        [ { material = Nonmetal
          , roughness = 0.4
          , color = "#FF5555"
          , kind = Cone
          , position = Frame3d.atPoint (Point3d.centimeters -15 -3 71.6)
          }
        , { material = Matte
          , roughness = 0.6
          , color = "#5555FF"
          , kind = Cube
          , position =
                Frame3d.atOrigin
                    |> Frame3d.rotateAround Axis3d.z (Angle.degrees 30)
                    |> Frame3d.moveTo (Point3d.centimeters 10 10 71.6)
          }
        , { material = Metal
          , roughness = 0.4
          , color = "#FFFF55"
          , kind = Sphere
          , position = Frame3d.atPoint (Point3d.centimeters 10 -12 71.6)
          }
        ]
    }


type alias DirectLight =
    { kind : LightKind
    , castsShadows : Bool
    , intensity : Float
    , temperature : Float
    }


type LightKind
    = Bulb (Point3d Meters SceneCoordinates)
    | Sun (Direction3d SceneCoordinates)


type alias Object =
    { material : Material
    , roughness : Float
    , color : String
    , kind : ObjectKind
    , position : Frame3d Meters SceneCoordinates { defines : EntityCoordinates }
    }


type ObjectKind
    = Sphere
    | Cone
    | Cube


type Material
    = Metal
    | Nonmetal
    | Matte



-- VIEW


unlitScene : Model -> Html Msg
unlitScene model =
    Scene3d.unlit
        { dimensions = ( Pixels.pixels 360, Pixels.pixels 300 )
        , camera = camera model.distance model.azimuth model.elevation
        , clipDepth = Length.meters 0.1
        , background = Scene3d.transparentBackground
        , entities =
            [ table |> Scene3d.placeIn Frame3d.atOrigin

            --, floor |> Scene3d.placeIn Frame3d.atOrigin
            , chair
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees -10)
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -0.4 0))
            , chair
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 60)
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -0.6 0))
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 180)
            ]
                ++ List.map object model.objects
        }


sunnyScene : Model -> Html Msg
sunnyScene model =
    Scene3d.sunny
        { dimensions = ( Pixels.pixels 360, Pixels.pixels 300 )
        , camera = camera model.distance model.azimuth model.elevation
        , clipDepth = Length.meters 0.1
        , background = Scene3d.transparentBackground
        , entities =
            [ table |> Scene3d.placeIn Frame3d.atOrigin

            --, floor |> Scene3d.placeIn Frame3d.atOrigin
            , chair
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees -10)
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -0.4 0))
            , chair
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 60)
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -0.6 0))
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 180)
            ]
                ++ List.map object model.objects
        , shadows = True
        , upDirection = Direction3d.positiveZ
        , sunlightDirection = Direction3d.xyZ (Angle.degrees 135) (Angle.degrees -60)
        }


cloudyScene : Model -> Html Msg
cloudyScene model =
    Scene3d.cloudy
        { dimensions = ( Pixels.pixels 360, Pixels.pixels 300 )
        , camera = camera model.distance model.azimuth model.elevation
        , clipDepth = Length.meters 0.1
        , background = Scene3d.transparentBackground
        , entities =
            [ table |> Scene3d.placeIn Frame3d.atOrigin

            --, floor |> Scene3d.placeIn Frame3d.atOrigin
            , chair
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees -10)
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -0.4 0))
            , chair
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 60)
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -0.6 0))
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 180)
            ]
                ++ List.map object model.objects
        , upDirection = Direction3d.negativeZ
        }


customScene : Model -> Html Msg
customScene model =
    Scene3d.custom
        { dimensions = model.dimensions
        , antialiasing =
            if model.multisampling then
                Scene3d.multisampling

            else
                Scene3d.supersampling model.supersampling
        , camera = camera model.distance model.azimuth model.elevation
        , clipDepth = Length.meters 0.1
        , lights = lights model.lights
        , exposure = Scene3d.exposureValue model.exposure
        , toneMapping = Scene3d.reinhardToneMapping model.dynamicRange
        , whiteBalance = Light.daylight
        , background =
            Scene3d.backgroundColor
                (Color.fromHex model.backgroundColor
                    |> Result.withDefault (Color.fromRGB ( 0, 0, 0 ))
                )
        , entities =
            [ table |> Scene3d.placeIn Frame3d.atOrigin
            , floor |> Scene3d.placeIn Frame3d.atOrigin
            , chair
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees -10)
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -0.4 0))
            , chair
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 60)
                |> Scene3d.placeIn (Frame3d.atPoint (Point3d.meters 0 -0.6 0))
                |> Scene3d.rotateAround Axis3d.z (Angle.degrees 180)
            ]
                ++ List.map object model.objects
                ++ List.filterMap bulb model.lights
        }


view : Model -> Html Msg
view model =
    Html.div []
        [ panel model
        , Html.div
            (Html.Attributes.style "position" "fixed"
                :: Html.Attributes.style "left" "0"
                :: Html.Attributes.style "overflow" "hidden"
                :: Html.Attributes.style "top" "0"
                :: events model.orbiting
            )
            [ sunnyScene model
            , cloudyScene model
            , unlitScene model
            ]
        ]


events : Bool -> List (Attribute Msg)
events orbiting =
    Html.Events.preventDefaultOn "mousewheel"
        (Json.Decode.map
            (\deltaY -> ( Zoom deltaY, True ))
            (Json.Decode.field "deltaY" Json.Decode.float)
        )
        :: (if orbiting then
                [ Html.Events.on "mousemove"
                    (Json.Decode.map2 MouseMove
                        (Json.Decode.field "movementX" Json.Decode.float)
                        (Json.Decode.field "movementY" Json.Decode.float)
                    )
                , Html.Events.on "mouseup" (Json.Decode.succeed MouseUp)
                ]

            else
                [ Html.Events.onMouseDown MouseDown ]
           )


type SceneCoordinates
    = SceneCoordinates


camera : Length -> Angle -> Angle -> Camera3d Meters SceneCoordinates
camera distance azimuth elevation =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.orbit
                { focalPoint = Point3d.meters 0 0.1 0.71
                , groundPlane = SketchPlane3d.xy
                , azimuth = azimuth
                , elevation = elevation
                , distance = distance
                }
        , verticalFieldOfView = Angle.degrees 24
        }


lights : List DirectLight -> Scene3d.Lights SceneCoordinates
lights directLights =
    let
        lightsWithoutShadows =
            List.filterMap (directLight False Light.neverCastsShadows) directLights

        lightsWithShadows =
            List.filterMap (directLight True (Light.castsShadows True)) directLights
    in
    case ( List.head lightsWithShadows, lightsWithoutShadows ) of
        ( Nothing, [] ) ->
            Scene3d.noLights

        ( Just light1, [] ) ->
            Scene3d.oneLight light1

        ( Nothing, light1 :: [] ) ->
            Scene3d.oneLight light1

        ( Just light1, light2 :: [] ) ->
            Scene3d.twoLights light1 light2

        ( Nothing, light1 :: light2 :: [] ) ->
            Scene3d.twoLights light1 light2

        ( Just light1, light2 :: light3 :: [] ) ->
            Scene3d.threeLights light1 light2 light3

        ( Nothing, light1 :: light2 :: light3 :: [] ) ->
            Scene3d.threeLights light1 light2 light3

        ( Just light1, light2 :: light3 :: light4 :: _ ) ->
            Scene3d.fourLights light1 light2 light3 light4

        ( Nothing, light1 :: light2 :: light3 :: light4 :: _ ) ->
            Scene3d.fourLights light1 light2 light3 light4


directLight : Bool -> Light.CastsShadows castsShadows -> DirectLight -> Maybe (Light SceneCoordinates castsShadows)
directLight shouldHaveShadows shadows light =
    if (shouldHaveShadows == light.castsShadows) && light.intensity > 0 then
        Just
            (case light.kind of
                Bulb position ->
                    Light.point shadows
                        { chromaticity = Light.colorTemperature (Temperature.kelvins light.temperature)
                        , intensity = LuminousFlux.lumens light.intensity
                        , position = position
                        }

                Sun direction ->
                    Light.directional shadows
                        { chromaticity = Light.colorTemperature (Temperature.kelvins light.temperature)
                        , intensity = Illuminance.lux light.intensity
                        , direction = direction
                        }
            )

    else
        Nothing


panelWidth : Float
panelWidth =
    300


panel : Model -> Html Msg
panel model =
    Html.div
        [ Html.Attributes.style "float" "right"
        , Html.Attributes.style "width" (String.fromFloat panelWidth ++ "px")
        , Html.Attributes.style "box-sizing" "border-box"
        , Html.Attributes.style "padding" "0 20px"
        ]
        [ Html.h1 [] [ Html.text "Scene3d" ]
        , collapsible True
            "Rendering Options"
            [ checkBox OnMultisamplingToggled "Multisampling" model.multisampling
            , slider OnSupersamplingChanged { from = 1, to = 2, fraction = 2, units = "" } "Supersampling" model.supersampling
            , slider OnDynamicRangeChanged { from = 1, to = 10, fraction = 2, units = "" } "Dynamic range" model.dynamicRange
            , slider OnExposureChanged { from = 5, to = 16, fraction = 2, units = "EV" } "Exposure" model.exposure
            , colorPicker OnBackgroundColorChanged "Background color" model.backgroundColor
            ]
        , collapsible False
            "Environmental Lighting"
            (if model.environmentalLighting then
                [ checkBox OnEnvironmentalLightingToggled "Enabled" model.environmentalLighting
                , slider OnEnvironmentalLuminanceAboveChanged lumensOptions "Luminance above" model.environmentalLuminanceAbove
                , slider OnEnvironmentalTemperatureAboveChanged kelvinsOptions "Temperature above" model.environmentalTemperatureAbove
                , slider OnEnvironmentalLuminanceBelowChanged lumensOptions "Luminance below" model.environmentalLuminanceBelow
                , slider OnEnvironmentalTemperatureBelowChanged kelvinsOptions "Temperature below" model.environmentalTemperatureBelow
                ]

             else
                [ checkBox OnEnvironmentalLightingToggled "Enabled" model.environmentalLighting ]
            )
        , collapsible False "Direct Lighting" (List.indexedMap directLightPanel model.lights)
        , collapsible False "Objects" (List.indexedMap objectPanel model.objects)
        ]


directLightPanel : Int -> DirectLight -> Html Msg
directLightPanel index light =
    fieldset
        (case light.kind of
            Sun _ ->
                "Directional"

            Bulb _ ->
                "Bulb " ++ String.fromInt index
        )
        ((case light.kind of
            Sun _ ->
                [ slider (OnDirectLightIntensityChanged index) { from = 0, to = 100000, fraction = 0, units = "" } "Intensity (lux)" light.intensity
                ]

            Bulb _ ->
                [ slider (OnDirectLightIntensityChanged index) lumensOptions "Intensity (lumens)" light.intensity
                ]
         )
            ++ (if light.intensity > 0 then
                    [ checkBox (OnDirectLightCastsShadowsToggled index) "Casts shadows" light.castsShadows
                    , slider (OnDirectLightTemperatureChanged index) kelvinsOptions "Temperature" light.temperature
                    ]

                else
                    []
               )
        )


materials : (Material -> Msg) -> Material -> Html Msg
materials msg current =
    let
        stringToMaterial st =
            case st of
                "Metal" ->
                    Metal

                "Nonmetal" ->
                    Nonmetal

                _ ->
                    Matte

        materialToString m =
            case m of
                Metal ->
                    "Metal"

                Nonmetal ->
                    "Nonmetal"

                Matte ->
                    "Matte"

        option m =
            Html.option
                [ Html.Attributes.value (materialToString m)
                , Html.Attributes.selected (m == current)
                ]
                [ Html.text (materialToString m) ]
    in
    label
        [ Html.text "Material:"
        , Html.select
            [ Html.Events.onInput (stringToMaterial >> msg)
            , Html.Attributes.style "margin" "0 0 0 0.5em"
            ]
            (List.map option [ Metal, Nonmetal, Matte ])
        ]


objectPanel : Int -> Object -> Html Msg
objectPanel index obj =
    fieldset
        (case obj.kind of
            Sphere ->
                "Sphere"

            Cube ->
                "Cube"

            Cone ->
                "Cone"
        )
        [ materials (OnObjectMaterialChanged index) obj.material
        , colorPicker (OnObjectColorChanged index) "Color" obj.color
        , if obj.material /= Matte then
            slider (OnObjectRoughnessChanged index)
                { from = 0, to = 1, fraction = 2, units = "" }
                "Roughness"
                obj.roughness

          else
            Html.text ""
        ]


colorPicker : (String -> Msg) -> String -> String -> Html Msg
colorPicker msg txt value =
    label
        [ Html.text (txt ++ ":")
        , Html.input
            [ Html.Attributes.type_ "color"
            , Html.Events.onInput msg
            , Html.Attributes.value value
            , Html.Attributes.style "margin" "0 0 0 0.5em"
            ]
            []
        ]


type alias SliderOptions =
    { from : Float
    , to : Float
    , fraction : Int
    , units : String
    }


kelvinsOptions : SliderOptions
kelvinsOptions =
    { from = 1700
    , to = 7000
    , fraction = 0
    , units = "K"
    }


lumensOptions : SliderOptions
lumensOptions =
    { from = 100
    , to = 10000
    , fraction = 0
    , units = ""
    }


slider : (Float -> Msg) -> SliderOptions -> String -> Float -> Html Msg
slider msg { from, to, fraction, units } txt value =
    label
        [ Html.text (txt ++ ": " ++ Round.round fraction value ++ " " ++ units)
        , Html.input
            [ Html.Attributes.type_ "range"
            , Html.Events.onInput (String.toFloat >> Maybe.withDefault 0 >> msg)
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.min (String.fromFloat from)
            , Html.Attributes.max (String.fromFloat to)
            , Html.Attributes.step "0.001"
            , Html.Attributes.value (String.fromFloat value)
            ]
            []
        ]


checkBox : (Bool -> Msg) -> String -> Bool -> Html Msg
checkBox msg txt on =
    label
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked on
            , Html.Events.onCheck msg
            , Html.Attributes.style "margin" "-2px 0.6em 0 0"
            , Html.Attributes.style "vertical-align" "middle"
            , Html.Attributes.style "display" "inline-block"
            ]
            []
        , Html.text txt
        ]


label : List (Html Msg) -> Html Msg
label =
    Html.label
        [ Html.Attributes.style "font-family" "sans-serif"
        , Html.Attributes.style "display" "block"
        , Html.Attributes.style "margin" "1em 0"
        ]


fieldset : String -> List (Html Msg) -> Html Msg
fieldset legend rest =
    Html.fieldset
        [ Html.Attributes.style "margin" "1em 0"
        , Html.Attributes.style "padding" "0 1em"
        ]
        (Html.legend
            [ Html.Attributes.style "font-family" "sans-serif"
            , Html.Attributes.style "display" "block"
            ]
            [ Html.text legend ]
            :: rest
        )


collapsible : Bool -> String -> List (Html Msg) -> Html Msg
collapsible open legend rest =
    Html.details
        (if open then
            [ Html.Attributes.style "margin" "0"
            , Html.Attributes.attribute "open" "true"
            ]

         else
            [ Html.Attributes.style "margin" "0" ]
        )
        (Html.summary
            [ Html.Attributes.style "font-family" "sans-serif"
            , Html.Attributes.style "cursor" "pointer"
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "margin" "1em 0"
            , Html.Attributes.style "padding" "0.5em"
            , Html.Attributes.style "background" "lightgray"
            ]
            [ Html.text legend ]
            :: rest
        )



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resize dimensions ->
            { model | dimensions = dimensions }

        -- Camera:
        Zoom deltaY ->
            { model
                | distance =
                    model.distance
                        |> Quantity.minus (Length.meters (deltaY * 0.01))
                        |> Quantity.clamp (Length.meters 1) (Length.meters 6)
            }

        MouseDown ->
            { model | orbiting = True }

        MouseMove dx dy ->
            if model.orbiting then
                { model
                    | azimuth =
                        model.azimuth
                            |> Quantity.minus (Angle.degrees dx)
                    , elevation =
                        model.elevation
                            |> Quantity.plus (Angle.degrees dy)
                            |> Quantity.clamp (Angle.degrees 6) (Angle.degrees 90)
                }

            else
                model

        MouseUp ->
            { model | orbiting = False }

        -- Options:
        OnMultisamplingToggled multisampling ->
            { model | multisampling = multisampling }

        OnSupersamplingChanged supersampling ->
            { model | supersampling = supersampling }

        OnDynamicRangeChanged dynamicRange ->
            { model | dynamicRange = dynamicRange }

        OnExposureChanged exposure ->
            { model | exposure = exposure }

        OnBackgroundColorChanged backgroundColor ->
            { model | backgroundColor = backgroundColor }

        -- Environmental lighting:
        OnEnvironmentalLightingToggled environmentalLighting ->
            { model | environmentalLighting = environmentalLighting }

        OnEnvironmentalLuminanceAboveChanged environmentalLuminanceAbove ->
            { model | environmentalLuminanceAbove = environmentalLuminanceAbove }

        OnEnvironmentalTemperatureAboveChanged environmentalTemperatureAbove ->
            { model | environmentalTemperatureAbove = environmentalTemperatureAbove }

        OnEnvironmentalLuminanceBelowChanged environmentalLuminanceBelow ->
            { model | environmentalLuminanceBelow = environmentalLuminanceBelow }

        OnEnvironmentalTemperatureBelowChanged environmentalTemperatureBelow ->
            { model | environmentalTemperatureBelow = environmentalTemperatureBelow }

        -- Direct lights:
        OnDirectLightCastsShadowsToggled index castsShadows ->
            { model
                | lights =
                    List.indexedMap
                        (\idx light ->
                            if idx == index then
                                { light | castsShadows = castsShadows }

                            else if castsShadows then
                                { light | castsShadows = False }

                            else
                                light
                        )
                        model.lights
            }

        OnDirectLightTemperatureChanged index temperature ->
            { model
                | lights =
                    List.indexedMap
                        (\idx light ->
                            if idx == index then
                                { light | temperature = temperature }

                            else
                                light
                        )
                        model.lights
            }

        OnDirectLightIntensityChanged index intensity ->
            { model
                | lights =
                    List.indexedMap
                        (\idx light ->
                            if idx == index then
                                { light | intensity = intensity }

                            else
                                light
                        )
                        model.lights
            }

        -- Objects:
        OnObjectMaterialChanged index material ->
            { model
                | objects =
                    List.indexedMap
                        (\idx obj ->
                            if idx == index then
                                { obj | material = material }

                            else
                                obj
                        )
                        model.objects
            }

        OnObjectColorChanged index color ->
            { model
                | objects =
                    List.indexedMap
                        (\idx obj ->
                            if idx == index then
                                { obj | color = color }

                            else
                                obj
                        )
                        model.objects
            }

        OnObjectRoughnessChanged index roughness ->
            { model
                | objects =
                    List.indexedMap
                        (\idx obj ->
                            if idx == index then
                                { obj | roughness = roughness }

                            else
                                obj
                        )
                        model.objects
            }



-- ENTITIES


type EntityCoordinates
    = EntityCoordinates


floor : Scene3d.Entity EntityCoordinates
floor =
    Scene3d.quad
        --(Material.color (Color.fromRGB ( 50, 50, 50 )))
        (Material.nonmetal
            { baseColor = Color.fromRGB ( 50, 50, 50 )
            , roughness = 0.9
            }
        )
        (Point3d.meters -1.5 -3 0)
        (Point3d.meters 1.5 -3 0)
        (Point3d.meters 1.5 3 0)
        (Point3d.meters -1.5 3 0)


table : Scene3d.Entity EntityCoordinates
table =
    let
        halfLength =
            87.6 / 2

        halfWidth =
            87.6 / 2

        height =
            71.6

        thickness =
            5

        leg =
            5
    in
    [ Block3d.from
        (Point3d.centimeters (halfWidth - leg) (halfLength - leg) 0)
        (Point3d.centimeters halfWidth halfLength (height - thickness))
    , Block3d.from
        (Point3d.centimeters (-halfWidth + leg) (-halfLength + leg) 0)
        (Point3d.centimeters -halfWidth -halfLength (height - thickness))
    , Block3d.from
        (Point3d.centimeters (-halfWidth + leg) (halfLength - leg) 0)
        (Point3d.centimeters -halfWidth halfLength (height - thickness))
    , Block3d.from
        (Point3d.centimeters (halfWidth - leg) (-halfLength + leg) 0)
        (Point3d.centimeters halfWidth -halfLength (height - thickness))
    , Block3d.from
        (Point3d.centimeters -halfWidth -halfLength (height - thickness))
        (Point3d.centimeters halfWidth halfLength height)
    ]
        |> List.map
            (Scene3d.blockWithShadow
                --(Material.color (Color.fromRGB ( 250, 180, 60 )))
                (Material.nonmetal
                    { baseColor = Color.fromRGB ( 250, 180, 60 )
                    , roughness = 0.8
                    }
                )
            )
        |> Scene3d.group


chair : Scene3d.Entity EntityCoordinates
chair =
    let
        halfDepth =
            49.8 / 2

        halfWidth =
            41 / 2

        seatHeight =
            45.1

        height =
            94.9

        thickness =
            5

        leg =
            5

        backHeight =
            10

        backDistanceFromTop =
            5
    in
    [ Block3d.from
        (Point3d.centimeters (halfWidth - leg) (halfDepth - leg) 0)
        (Point3d.centimeters halfWidth halfDepth (seatHeight - thickness))
    , Block3d.from
        (Point3d.centimeters (-halfWidth + leg) (-halfDepth + leg) 0)
        (Point3d.centimeters -halfWidth -halfDepth height)
    , Block3d.from
        (Point3d.centimeters (-halfWidth + leg) (halfDepth - leg) 0)
        (Point3d.centimeters -halfWidth halfDepth (seatHeight - thickness))
    , Block3d.from
        (Point3d.centimeters (halfWidth - leg) (-halfDepth + leg) 0)
        (Point3d.centimeters halfWidth -halfDepth height)
    , Block3d.from
        (Point3d.centimeters -halfWidth -halfDepth (seatHeight - thickness))
        (Point3d.centimeters halfWidth halfDepth seatHeight)
    , Block3d.from
        (Point3d.centimeters (halfWidth - leg) -halfDepth (height - backDistanceFromTop))
        (Point3d.centimeters (-halfWidth + leg) (-halfDepth + leg) (height - backDistanceFromTop - backHeight))
    ]
        |> List.map
            (Scene3d.blockWithShadow
                --(Material.color (Color.fromRGB ( 250, 180, 60 ) |> Color.blacken 25))
                (Material.nonmetal
                    { baseColor = Color.fromRGB ( 250, 180, 60 ) |> Color.blacken 25
                    , roughness = 0.8
                    }
                )
            )
        |> Scene3d.group


object : Object -> Scene3d.Entity SceneCoordinates
object { kind, position, material, color, roughness } =
    let
        objectColor =
            Color.fromHex color
                |> Result.withDefault (Color.fromRGB ( 255, 255, 255 ))

        objectMaterial =
            --Material.color objectColor
            case material of
                Matte ->
                    Material.matte objectColor

                Metal ->
                    Material.metal
                        { baseColor = objectColor
                        , roughness = roughness
                        }

                Nonmetal ->
                    Material.nonmetal
                        { baseColor = objectColor
                        , roughness = roughness
                        }
    in
    case kind of
        Sphere ->
            Scene3d.placeIn position (sphere objectMaterial)

        Cube ->
            Scene3d.placeIn position (cube objectMaterial)

        Cone ->
            Scene3d.placeIn position (cone objectMaterial)


cube : Material.Uniform EntityCoordinates -> Scene3d.Entity EntityCoordinates
cube material =
    Block3d.from (Point3d.centimeters -7.5 -7.5 15) (Point3d.centimeters 7.5 7.5 0)
        |> Scene3d.blockWithShadow material


sphere : Material.Textured EntityCoordinates -> Scene3d.Entity EntityCoordinates
sphere material =
    Sphere3d.atPoint (Point3d.centimeters 0 0 7) (Length.centimeters 7)
        |> Scene3d.sphereWithShadow material


cone : Material.Uniform EntityCoordinates -> Scene3d.Entity EntityCoordinates
cone material =
    Cone3d.startingAt
        Point3d.origin
        Direction3d.z
        { radius = Length.centimeters 8, length = Length.centimeters 25 }
        |> Scene3d.coneWithShadow material


bulb : DirectLight -> Maybe (Scene3d.Entity SceneCoordinates)
bulb light =
    case light.kind of
        Bulb position ->
            Just
                (Scene3d.group
                    [ Sphere3d.atOrigin (Length.centimeters 2)
                        |> Scene3d.sphere
                            (if light.intensity > 600 then
                                Material.emissive Light.daylight
                                    (Luminance.nits light.intensity)

                             else
                                Material.matte (Color.fromRGB ( 255, 255, 255 ))
                            )
                    , Cylinder3d.startingAt (Point3d.centimeters 0 0 2)
                        Direction3d.z
                        { radius = Length.millimeters 2, length = Length.meters 7 }
                        |> Scene3d.cylinder (Material.matte (Color.fromRGB ( 255, 255, 255 )))
                    ]
                    |> Scene3d.placeIn (Frame3d.atPoint position)
                )

        _ ->
            Nothing
