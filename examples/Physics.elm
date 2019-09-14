module Physics exposing (main)

import Angle
import Array exposing (Array)
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Camera3d
import Color
import Common.Materials as Materials
import Direction3d
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Illuminance
import Length exposing (Length, Meters, inMeters, meters)
import Luminance
import Physics.Body as Body exposing (Body)
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d
import Random
import Scene3d
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Drawable as Drawable exposing (Drawable, Material)
import Scene3d.Exposure as Exposure
import Scene3d.Light as Light
import Scene3d.Mesh as Mesh exposing (Mesh, NoTangents, NoUV, ShadowsDisabled, ShadowsEnabled, Triangles, WithNormals)
import Scene3d.Shape as Shape
import Task
import Viewpoint3d


type alias Model =
    { world : World (Drawable BodyCoordinates)
    , screenWidth : Float
    , screenHeight : Float
    }


type Msg
    = Tick Float
    | Resize Float Float


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initialWorld
      , screenWidth = 0
      , screenHeight = 0
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( { model | world = World.simulate (1000 / 60) model.world }
            , Cmd.none
            )

        Resize width height ->
            ( { model | screenWidth = width, screenHeight = height }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        ]


view : Model -> Html Msg
view { world, screenWidth, screenHeight } =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters 0 20 20
                        , focalPoint = Point3d.meters 0 0 0
                        , upDirection = Direction3d.positiveZ
                        }
                , clipDepth = meters 0.1
                , verticalFieldOfView = Angle.degrees 24
                }

        drawables =
            List.map getTransformedDrawable (World.getBodies world)

        sunlight =
            Light.directional Chromaticity.daylight
                (Illuminance.lux 10000)
                (Direction3d.xyZ (Angle.degrees 45) (Angle.degrees -60))

        ambientLighting =
            Light.overcast
                { zenithDirection = Direction3d.z
                , chromaticity = Chromaticity.daylight
                , zenithLuminance = Luminance.nits 5000
                }
    in
    Scene3d.render []
        { width = pixels screenWidth
        , height = pixels screenHeight
        , camera = camera
        , lights = Scene3d.oneLight sunlight { castsShadows = True }
        , ambientLighting = Just ambientLighting
        , exposure = Exposure.fromMaxLuminance (Luminance.nits 10000)
        , whiteBalance = Chromaticity.daylight
        }
        drawables


initialWorld : World (Drawable BodyCoordinates)
initialWorld =
    let
        moonGravity =
            { x = 0, y = 0, z = -1.62 }
    in
    World.empty
        |> World.setGravity moonGravity
        |> World.add floor
        |> addBoxes


materials : Array Material
materials =
    Array.fromList
        [ Materials.aluminum
        , Materials.whitePlastic
        , Materials.copper
        , { baseColor = Color.lightBlue, roughness = 0.25, metallic = False }
        , Materials.gold
        , Materials.whitePlastic
        , { baseColor = Color.blue, roughness = 0.25, metallic = False }
        ]


type alias Offsets =
    { x : Float
    , y : Float
    , z : Float
    }


offsetGenerator : Random.Generator Offsets
offsetGenerator =
    let
        magnitude =
            0.01
    in
    Random.map3 Offsets
        (Random.float -magnitude magnitude)
        (Random.float -magnitude magnitude)
        (Random.float -magnitude magnitude)


randomOffsets : Int -> Offsets
randomOffsets index =
    Random.step offsetGenerator (Random.initialSeed index)
        |> Tuple.first


addBoxes : World (Drawable BodyCoordinates) -> World (Drawable BodyCoordinates)
addBoxes world =
    let
        xySize =
            4

        zSize =
            5

        xyDimensions =
            List.map toFloat (List.range 0 (xySize - 1))

        zDimensions =
            List.map toFloat (List.range 0 (zSize - 1))

        distance =
            1
    in
    List.foldl
        (\x world1 ->
            List.foldl
                (\y world2 ->
                    List.foldl
                        (\z ->
                            let
                                index =
                                    round (z * xySize * xySize + y * xySize + x)

                                material =
                                    Array.get (index |> modBy (Array.length materials)) materials
                                        |> Maybe.withDefault Materials.aluminum

                                body =
                                    if (index |> modBy 3) == 0 then
                                        box material

                                    else
                                        sphere material

                                offsets =
                                    randomOffsets index
                            in
                            body
                                |> Body.moveBy
                                    { x = (x - (xySize - 1) / 2) * distance + offsets.x
                                    , y = (y - (xySize - 1) / 2) * distance + offsets.y
                                    , z = (z + (2 * zSize + 1) / 2) * distance + offsets.z
                                    }
                                |> World.add
                        )
                        world2
                        zDimensions
                )
                world1
                xyDimensions
        )
        world
        xyDimensions


floorRadius : Length
floorRadius =
    Length.meters 30


floorMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsDisabled)
floorMesh =
    Shape.sphere { radius = floorRadius, subdivisions = 144 }


floor : Body (Drawable BodyCoordinates)
floor =
    Drawable.physical Materials.aluminum floorMesh
        |> Body.sphere (Length.inMeters floorRadius)
        |> Body.setMass 0
        |> Body.moveBy { x = 0, y = 0, z = -(Length.inMeters floorRadius) }


boxSize : Length
boxSize =
    Length.meters 0.9


boxMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsEnabled)
boxMesh =
    Shape.block boxSize boxSize boxSize
        |> Mesh.enableShadows


box : Material -> Body (Drawable BodyCoordinates)
box material =
    let
        bodySize =
            Length.inMeters boxSize
    in
    Drawable.physical material boxMesh
        |> Drawable.withShadow boxMesh
        |> Body.box { x = bodySize, y = bodySize, z = bodySize }
        |> Body.setMass 5


sphereRadius : Length
sphereRadius =
    Length.meters 0.45


sphereMesh : Mesh BodyCoordinates (Triangles WithNormals NoUV NoTangents ShadowsEnabled)
sphereMesh =
    Shape.sphere { radius = sphereRadius, subdivisions = 36 }
        |> Mesh.enableShadows


sphere : Material -> Body (Drawable BodyCoordinates)
sphere material =
    Drawable.physical material sphereMesh
        |> Drawable.withShadow sphereMesh
        |> Body.sphere (inMeters sphereRadius)
        |> Body.setMass 2.5


type BodyCoordinates
    = BodyCoordinates


type WorldCoordinates
    = WorldCoordinates


toFrame :
    { m11 : Float
    , m21 : Float
    , m31 : Float
    , m41 : Float
    , m12 : Float
    , m22 : Float
    , m32 : Float
    , m42 : Float
    , m13 : Float
    , m23 : Float
    , m33 : Float
    , m43 : Float
    , m14 : Float
    , m24 : Float
    , m34 : Float
    , m44 : Float
    }
    -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates }
toFrame t =
    Frame3d.unsafe
        { originPoint = Point3d.unsafe { x = t.m14, y = t.m24, z = t.m34 }
        , xDirection = Direction3d.unsafe { x = t.m11, y = t.m21, z = t.m31 }
        , yDirection = Direction3d.unsafe { x = t.m12, y = t.m22, z = t.m32 }
        , zDirection = Direction3d.unsafe { x = t.m13, y = t.m23, z = t.m33 }
        }


getTransformedDrawable : Body (Drawable BodyCoordinates) -> Drawable WorldCoordinates
getTransformedDrawable body =
    Body.getData body
        |> Drawable.placeIn
            (toFrame (Body.getTransformation body))
