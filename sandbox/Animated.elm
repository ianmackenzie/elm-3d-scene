module Animated exposing (Model, Msg(..), Styles(..), box, cylinder, init, main, sphere, translateBy, update, view)

import Angle
import Axis3d exposing (Axis3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color
import Direction3d exposing (Direction3d)
import Duration exposing (Duration, milliseconds)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Length exposing (Meters, meters)
import Materials
import Pixels exposing (Pixels, pixels)
import Point3d exposing (Point3d)
import PointLight exposing (PointLight(..))
import Quantity exposing (zero)
import Rectangle2d
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable exposing (Drawable)
import Scene3d.Exposure as Exposure
import Scene3d.Light as Light exposing (Light)
import Scene3d.Mesh as Mesh exposing (HasNormals, NoTangents, NoUV)
import Scene3d.Shape as Shape
import SketchPlane3d
import Task
import Time
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture


type World
    = World


translateBy : ( Float, Float ) -> Drawable World -> Drawable World
translateBy ( x, y ) =
    Drawable.translateBy (Vector3d.meters x y 0)


sphere : Material -> ( Float, Float ) -> Drawable World
sphere material offset =
    Drawable.physical material (Shape.sphere (meters 1))
        |> translateBy offset


box : Material HasNormals NoUV NoTangents -> ( Float, Float ) -> Drawable World
box material offset =
    Drawable.physical material
        (Shape.box (meters 1.5) (meters 1.5) (meters 1.5))
        |> translateBy offset


cylinder : Material -> ( Float, Float ) -> Drawable World
cylinder material offset =
    Drawable.physical material
        (Shapes.cylinder { radius = meters 1, height = meters 1.5 })
        |> Drawable.translateBy (Vector3d.meters 0 0 -0.75)
        |> translateBy offset


type Styles
    = DefaultStyle
    | PanelStyle
    | OuterStyle
    | HeadingStyle


view : Model -> Html Msg
view =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters 10 10 10
                        , focalPoint = Point3d.origin
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                , clipDepth = meters 0.1
                }

        screenWidth =
            pixels 1024

        screenHeight =
            pixels 768

        shapes =
            Drawable.group
                [ sphere Materials.gold ( 3, -3 )
                , cylinder Materials.whitePlastic ( 3, 0 )
                , sphere Materials.copper ( 3, 3 )
                , box Materials.chromium ( 0, -3 )
                , sphere Materials.aluminum ( 0, 0 )
                , cylinder Materials.gold ( 0, 3 )
                , sphere Materials.copper ( -3, -3 )
                , box Materials.blackPlastic ( -3, 0 )
                , sphere Materials.whitePlastic ( -3, 3 )
                ]

        -- blue
        pointLight1Start =
            Point3d.meters 1.5 1.5 3

        pointLight1RotationAxis =
            Axis3d.z

        -- red
        pointLight2Start =
            Point3d.meters 1.5 -1.5 0

        pointLight2RotationAxis =
            Axis3d.x |> Axis3d.rotateAround Axis3d.z (Angle.degrees 45)

        overheadLight1 =
            Light.point Chromaticity.daylight
                lumens
                (Point3d.meters 8 8 5)
                PointLight.at
                (Point3d.meters 8 8 5)
                { color = overheadLightColor
                , radius = pointLightRadius
                }

        overheadLight2 =
            PointLight.at (Point3d.meters 8 -8 5)
                { color = overheadLightColor
                , radius = pointLightRadius
                }

        overheadLight3 =
            PointLight.at (Point3d.meters -8 8 5)
                { color = overheadLightColor
                , radius = pointLightRadius
                }

        overheadLight4 =
            PointLight.at (Point3d.meters -8 -8 5)
                { color = overheadLightColor
                , radius = pointLightRadius
                }

        directionalLight1Color =
            ( 0, 0.1, 0.02 )

        directionalLight2Color =
            ( 0.3, 0.3, 0.3 )

        ambientLightColor =
            ( 0.01, 0.01, 0.01 )
    in
    \model ->
        case model.loadedTexture of
            Nothing ->
                Html.text "Loading texture..."

            Just (Err _) ->
                Html.text "Error loading texture"

            Just (Ok lookupTexture) ->
                let
                    directionalSpeed1 =
                        Angle.degrees 111 |> Quantity.per (Duration.seconds 1)

                    lightDirection1 =
                        Direction3d.fromAzimuthInAndElevationFrom
                            SketchPlane3d.xy
                            (Angle.degrees 180
                                |> Quantity.plus
                                    (model.elapsed
                                        |> Quantity.at directionalSpeed1
                                    )
                            )
                            (Angle.degrees -15)

                    directionalSpeed2 =
                        Angle.degrees 47 |> Quantity.per (Duration.seconds 1)

                    lightDirection2 =
                        Direction3d.fromAzimuthInAndElevationFrom
                            SketchPlane3d.xy
                            (Angle.degrees 270
                                |> Quantity.plus
                                    (model.elapsed
                                        |> Quantity.at directionalSpeed2
                                    )
                            )
                            (Angle.degrees -45)

                    pointSpeed1 =
                        Angle.degrees 67 |> Quantity.per (Duration.seconds 1)

                    pointLight1 =
                        pointLight1Start
                            |> PointLight.rotateAround pointLight1RotationAxis
                                (model.elapsed |> Quantity.at pointSpeed1)

                    pointSpeed2 =
                        Angle.degrees 71 |> Quantity.per (Duration.seconds 1)

                    pointLight2 =
                        pointLight2Start
                            |> PointLight.rotateAround pointLight2RotationAxis
                                (model.elapsed |> Quantity.at pointSpeed2)

                    addIf flag item list =
                        if flag model then
                            item :: list

                        else
                            list

                    lights =
                        []
                            |> addIf .directional1Enabled
                                (Light.directional lightDirection1
                                    directionalLight1Color
                                )
                            |> addIf .directional2Enabled
                                (Light.directional lightDirection2
                                    directionalLight2Color
                                )
                            |> addIf .point1Enabled
                                (PointLight.light pointLight1)
                            |> addIf .point2Enabled
                                (PointLight.light pointLight2)
                            |> addIf .overheadEnabled
                                (PointLight.light overheadLight1)
                            |> addIf .overheadEnabled
                                (PointLight.light overheadLight2)
                            |> addIf .overheadEnabled
                                (PointLight.light overheadLight3)
                            |> addIf .overheadEnabled
                                (PointLight.light overheadLight4)

                    scene =
                        Drawable.group
                            ([ shapes ]
                                |> addIf .point1Enabled
                                    (PointLight.drawable pointLight1)
                                |> addIf .point2Enabled
                                    (PointLight.drawable pointLight2)
                            )

                    renderOptions =
                        [ Scene3d.devicePixelRatio 2
                        , Scene3d.gammaCorrection 0.45
                        ]

                    sceneElement =
                        Element.html <|
                            Scene3d.render
                                { options = renderOptions
                                , lights = lights
                                , camera = camera
                                , screenWidth = screenWidth
                                , screenHeight = screenHeight
                                , scene = scene
                                , exposure = Exposure.sunny16
                                , whiteBalance = Chromaticity.daylight
                                }

                    checkbox value message label =
                        Input.checkbox []
                            { onChange = message
                            , icon = Input.defaultCheckbox
                            , checked = value
                            , label = Input.labelRight [] (Element.text label)
                            }

                    directional1Checkbox =
                        checkbox model.directional1Enabled
                            SetDirectional1Enabled
                            "Green directional"

                    directional2Checkbox =
                        checkbox model.directional2Enabled
                            SetDirectional2Enabled
                            "White directional"

                    point1Checkbox =
                        checkbox model.point1Enabled
                            SetPoint1Enabled
                            "Blue point"

                    point2Checkbox =
                        checkbox model.point2Enabled
                            SetPoint2Enabled
                            "Red point"

                    overheadCheckbox =
                        checkbox model.overheadEnabled
                            SetOverheadEnabled
                            "White overhead"

                    checkboxes =
                        Element.column
                            [ Element.spacing 5
                            , Element.padding 8
                            , Border.widthEach
                                { bottom = 0
                                , top = 0
                                , left = 0
                                , right = 1
                                }
                            , Border.solid
                            , Border.color (Element.rgb 0.5 0.5 0.5)
                            , Background.color (Element.rgb 0.9 0.9 0.9)
                            , Element.height Element.fill
                            , Font.size 16
                            ]
                            [ Element.el
                                [ Font.size 20
                                , Font.bold
                                , Element.paddingEach
                                    { top = 0
                                    , left = 0
                                    , right = 0
                                    , bottom = 8
                                    }
                                ]
                                (Element.text "Lights")
                            , directional1Checkbox
                            , directional2Checkbox
                            , point1Checkbox
                            , point2Checkbox
                            , overheadCheckbox
                            ]

                    layout =
                        Element.row
                            [ Element.height Element.fill ]
                            [ checkboxes, sceneElement ]
                in
                Element.layout [] layout


type alias Model =
    { elapsed : Duration
    , ambientEnabled : Bool
    , directional1Enabled : Bool
    , directional2Enabled : Bool
    , point1Enabled : Bool
    , point2Enabled : Bool
    , overheadEnabled : Bool
    }


type Msg
    = SetDirectional1Enabled Bool
    | SetDirectional2Enabled Bool
    | SetPoint1Enabled Bool
    | SetPoint2Enabled Bool
    | SetOverheadEnabled Bool
    | Tick Duration


init : () -> ( Model, Cmd Msg )
init () =
    ( { loadedTexture = Nothing
      , elapsed = zero
      , ambientEnabled = True
      , directional1Enabled = True
      , directional2Enabled = True
      , point1Enabled = True
      , point2Enabled = True
      , overheadEnabled = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Tick duration ->
            ( { model | elapsed = model.elapsed |> Quantity.plus duration }, Cmd.none )

        SetDirectional1Enabled value ->
            ( { model | directional1Enabled = value }, Cmd.none )

        SetDirectional2Enabled value ->
            ( { model | directional2Enabled = value }, Cmd.none )

        SetPoint1Enabled value ->
            ( { model | point1Enabled = value }, Cmd.none )

        SetPoint2Enabled value ->
            ( { model | point2Enabled = value }, Cmd.none )

        SetOverheadEnabled value ->
            ( { model | overheadEnabled = value }, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = always (Browser.Events.onAnimationFrameDelta (milliseconds >> Tick))
        , update = update
        , view = \model -> { title = "Animated", body = [ view model ] }
        }
