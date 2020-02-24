module TestApp exposing (main)

import Array
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Color exposing (Color)
import Cylinder3d exposing (Cylinder3d)
import Html exposing (Html)
import Http
import Length exposing (Meters)
import Palette.Tango as Tango
import Pixels
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity, Unitless)
import Quantity.Interval as Interval exposing (Interval)
import Result.Extra
import Scene3d
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Sphere3d exposing (Sphere3d)
import Task
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL.Texture


type Mesh
    = Points
    | LineSegments
    | Polyline
    | Triangles
    | Facets
    | Plain
    | Uniform
    | Unlit
    | Textured
    | Quad
    | Block
    | Sphere
    | Cylinder


type Material
    = Color
    | Emissive
    | Matte
    | Pbr
    | TexturedColor
    | TexturedEmissive
    | TexturedMatte
    | TexturedPbr


type Transformation
    = NoTransformation
    | Translation
    | Rotation
    | Scale
    | Mirror


type LightSources
    = NoLightSources
    | PointLight
    | DirectionalLight
    | TwoLightSources


type EnvironmentalLighting
    = NoEnvironmentalLighting
    | SoftLighting


type Antialiasing
    = NoAntialiasing
    | Multisampling
    | Supersampling


type alias TestCase =
    { mesh : Mesh
    , material : Material
    , shadow : Bool
    , transformation : Transformation
    , lightSources : LightSources
    , environmentalLighting : EnvironmentalLighting
    , dynamicRange : Float
    , antialiasing : Antialiasing
    }


parseMesh : String -> Result String Mesh
parseMesh string =
    case string of
        "Points" ->
            Ok Points

        "LineSegments" ->
            Ok LineSegments

        "Polyline" ->
            Ok Polyline

        "Triangles" ->
            Ok Triangles

        "Facets" ->
            Ok Facets

        "Plain" ->
            Ok Plain

        "Uniform" ->
            Ok Uniform

        "Unlit" ->
            Ok Unlit

        "Textured" ->
            Ok Textured

        "Quad" ->
            Ok Quad

        "Block" ->
            Ok Block

        "Sphere" ->
            Ok Sphere

        "Cylinder" ->
            Ok Cylinder

        _ ->
            Err ("Unrecognized mesh type '" ++ string ++ "'")


parseMaterial : String -> Result String Material
parseMaterial string =
    case string of
        "Color" ->
            Ok Color

        "Emissive" ->
            Ok Emissive

        "Matte" ->
            Ok Matte

        "Pbr" ->
            Ok Pbr

        "TexturedColor" ->
            Ok TexturedColor

        "TexturedEmissive" ->
            Ok TexturedEmissive

        "TexturedMatte" ->
            Ok TexturedMatte

        "TexturedPbr" ->
            Ok TexturedPbr

        _ ->
            Err ("Unrecognized material type '" ++ string ++ "'")


parseShadow : String -> Result String Bool
parseShadow string =
    case string of
        "NoShadow" ->
            Ok False

        "Shadow" ->
            Ok True

        _ ->
            Err ("Unrecognized shadow setting '" ++ string ++ "'")


parseTransformation : String -> Result String Transformation
parseTransformation string =
    case string of
        "NoTransformation" ->
            Ok NoTransformation

        "Translation" ->
            Ok Translation

        "Rotation" ->
            Ok Rotation

        "Scale" ->
            Ok Scale

        "Mirror" ->
            Ok Mirror

        _ ->
            Err ("Unrecognized transformation type '" ++ string ++ "'")


parseLightSources : String -> Result String LightSources
parseLightSources string =
    case string of
        "NoLightSources" ->
            Ok NoLightSources

        "PointLight" ->
            Ok PointLight

        "DirectionalLight" ->
            Ok DirectionalLight

        "TwoLightSources" ->
            Ok TwoLightSources

        _ ->
            Err ("Unrecognized light sources type '" ++ string ++ "'")


parseEnvironmentalLighting : String -> Result String EnvironmentalLighting
parseEnvironmentalLighting string =
    case string of
        "NoEnvironmentalLighting" ->
            Ok NoEnvironmentalLighting

        "SoftLighting" ->
            Ok SoftLighting

        _ ->
            Err ("Unrecognized environmental lighting type '" ++ string ++ "'")


parseDynamicRange : String -> Result String Float
parseDynamicRange string =
    case String.toFloat string of
        Just value ->
            Ok value

        Nothing ->
            Err ("Expected floating-point value for dynamic range, got '" ++ string ++ "'")


parseAntialiasing : String -> Result String Antialiasing
parseAntialiasing string =
    case string of
        "NoAntialiasing" ->
            Ok NoAntialiasing

        "Multisampling" ->
            Ok Multisampling

        "Supersampling" ->
            Ok Supersampling

        _ ->
            Err ("Unrecognized environmental lighting type '" ++ string ++ "'")


parseTestCase : String -> Result String TestCase
parseTestCase line =
    let
        items =
            String.split "\t" line
    in
    case items of
        [ mesh, material, shadow, transformation, lightSources, environmentalLighting, dynamicRange, antialiasing ] ->
            Ok TestCase
                |> Result.Extra.andMap (parseMesh mesh)
                |> Result.Extra.andMap (parseMaterial material)
                |> Result.Extra.andMap (parseShadow shadow)
                |> Result.Extra.andMap (parseTransformation transformation)
                |> Result.Extra.andMap (parseLightSources lightSources)
                |> Result.Extra.andMap (parseEnvironmentalLighting environmentalLighting)
                |> Result.Extra.andMap (parseDynamicRange dynamicRange)
                |> Result.Extra.andMap (parseAntialiasing antialiasing)

        _ ->
            Err ("Expected 8 items in line, got '" ++ line ++ "' with " ++ String.fromInt (List.length items))


parseTestCases : String -> Result String (List TestCase)
parseTestCases fileContents =
    String.lines fileContents
        |> List.drop 1
        |> List.filter (not << String.isEmpty)
        |> List.map parseTestCase
        |> Result.Extra.combine


type WorldCoordinates
    = WorldCoordinates


type alias Scene =
    { testCase : TestCase
    , entity : Scene3d.Entity WorldCoordinates
    }



-- type Mesh
--     = Points
--     | LineSegments
--     | Polyline
--     | Triangles
--     | Facets
--     | Plain
--     | Uniform
--     | Unlit
--     | Textured
--     | Quad
--     | Block
--     | Sphere
--     | Cylinder
-- type Material
--     = Color
--     | Emissive
--     | Matte
--     | Pbr
--     | TexturedColor
--     | TexturedEmissive
--     | TexturedMatte
--     | TexturedPbr
-- constructEntity : TestCase -> Result String (Scene3d.Entity WorldCoordinates)
-- constructEntity testCase =
--     case testCase.material of
--         Color ->


polyline : Polyline3d Meters WorldCoordinates
polyline =
    Polyline3d.fromVertices
        [ Point3d.meters -1 -1 1
        , Point3d.meters 0 -1 1
        , Point3d.origin
        , Point3d.meters 0 1 1
        , Point3d.meters 1 1 1
        ]


pointsMesh : Mesh.Plain WorldCoordinates
pointsMesh =
    Mesh.points { radius = Pixels.pixels 10 } (Polyline3d.vertices polyline)


lineSegmentsMesh : Mesh.Plain WorldCoordinates
lineSegmentsMesh =
    Mesh.lineSegments (Polyline3d.segments polyline)


polylineMesh : Mesh.Plain WorldCoordinates
polylineMesh =
    Mesh.polyline polyline


pyramidTriangles : List (Triangle3d Meters WorldCoordinates)
pyramidTriangles =
    let
        p0 =
            Point3d.meters 0 0 2

        p1 =
            Point3d.meters 1 0 1

        p2 =
            Point3d.meters 0 1 1

        p3 =
            Point3d.meters -1 0 1

        p4 =
            Point3d.meters 0 -1 1
    in
    [ Triangle3d.from p1 p2 p0
    , Triangle3d.from p2 p3 p0
    , Triangle3d.from p3 p4 p0
    , Triangle3d.from p4 p1 p0
    ]


trianglesMesh : Mesh.Plain WorldCoordinates
trianglesMesh =
    Mesh.triangles pyramidTriangles


facetsMesh : Mesh.Uniform WorldCoordinates
facetsMesh =
    Mesh.facets pyramidTriangles


type alias Vertex =
    { position : Point3d Meters WorldCoordinates
    , normal : Vector3d Unitless WorldCoordinates
    , uv : ( Float, Float )
    }


baseMesh : TriangularMesh Vertex
baseMesh =
    let
        v0 =
            { position = Point3d.meters 0 0 2
            , normal = Vector3d.unitless 0 0 1
            , uv = ( 0.5, 0.5 )
            }

        v1 =
            { position = Point3d.meters 1 0 1
            , normal = Vector3d.unitless 1 0 0
            , uv = ( 0, 0 )
            }

        v2 =
            { position = Point3d.meters 0 1 1
            , normal = Vector3d.unitless 0 1 0
            , uv = ( 1, 0 )
            }

        v3 =
            { position = Point3d.meters -1 0 1
            , normal = Vector3d.unitless -1 0 0
            , uv = ( 1, 1 )
            }

        v4 =
            { position = Point3d.meters 0 -1 1
            , normal = Vector3d.unitless 0 -1 0
            , uv = ( 0, 1 )
            }
    in
    TriangularMesh.indexed
        (Array.fromList [ v0, v1, v2, v3, v4 ])
        [ ( 1, 2, 0 )
        , ( 2, 3, 0 )
        , ( 3, 4, 0 )
        , ( 4, 1, 0 )
        ]


plainMesh : Mesh.Plain WorldCoordinates
plainMesh =
    Mesh.plain
        (baseMesh
            |> TriangularMesh.mapVertices
                (\{ position } -> position)
        )


uniformMesh : Mesh.Uniform WorldCoordinates
uniformMesh =
    Mesh.uniform
        (baseMesh
            |> TriangularMesh.mapVertices
                (\{ position, normal } ->
                    { position = position, normal = normal }
                )
        )


unlitMesh : Mesh.Unlit WorldCoordinates
unlitMesh =
    Mesh.unlit
        (baseMesh
            |> TriangularMesh.mapVertices
                (\{ position, uv } ->
                    { position = position, uv = uv }
                )
        )


texturedMesh : Mesh.Textured WorldCoordinates
texturedMesh =
    Mesh.textured baseMesh


quadEntity : Scene3d.CastsShadows a -> Material.Textured WorldCoordinates -> Scene3d.Entity WorldCoordinates
quadEntity castsShadows material =
    let
        p1 =
            Point3d.meters 1 -1 1

        p2 =
            Point3d.meters 1 1 1

        p3 =
            Point3d.meters -1 1 1

        p4 =
            Point3d.meters -1 -1 1
    in
    Scene3d.quad castsShadows material p1 p2 p3 p4


blockEntity : Scene3d.CastsShadows a -> Material.Uniform WorldCoordinates -> Scene3d.Entity WorldCoordinates
blockEntity castsShadows material =
    Scene3d.block castsShadows material <|
        Block3d.from
            (Point3d.meters -1 -1 1)
            (Point3d.meters 1 1 2)


sphereEntity : Scene3d.CastsShadows a -> Material.Textured WorldCoordinates -> Scene3d.Entity WorldCoordinates
sphereEntity castsShadows material =
    Scene3d.sphere castsShadows material <|
        Sphere3d.withRadius (Length.meters 1) (Point3d.meters 0 0 2)


cylinderEntity : Scene3d.CastsShadows a -> Material.Uniform WorldCoordinates -> Scene3d.Entity WorldCoordinates
cylinderEntity castsShadows material =
    Scene3d.cylinder castsShadows material <|
        Cylinder3d.along Axis3d.z
            { start = Length.meters 1
            , end = Length.meters 2
            , radius = Length.meters 1
            }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias LoadingModel =
    { testCases : Maybe (List TestCase)
    , colorTexture : Maybe (Material.Texture Color)
    , roughnessTexture : Maybe (Material.Texture Float)
    , metallicTexture : Maybe (Material.Texture Float)
    }


type alias LoadedModel =
    { testCases : List TestCase
    , colorTexture : Material.Texture Color
    , roughnessTexture : Material.Texture Float
    , metallicTexture : Material.Texture Float
    }


type Model
    = Loading LoadingModel
    | Loaded LoadedModel
    | Error String


init : flags -> ( Model, Cmd Msg )
init flags =
    ( Loading
        { testCases = Nothing
        , colorTexture = Nothing
        , roughnessTexture = Nothing
        , metallicTexture = Nothing
        }
    , Cmd.batch
        [ Http.get
            { url = "test_cases.txt"
            , expect = Http.expectString TestCasesResponse
            }
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_col.jpg"
            |> Task.attempt ColorTextureResponse
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_rgh.jpg"
            |> Task.attempt RoughnessTextureResponse
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_met.jpg"
            |> Task.attempt MetallicTextureResponse
        ]
    )


type alias Assets =
    { testCases : String
    , colorTexture : Material.Texture Color
    }


type Msg
    = TestCasesResponse (Result Http.Error String)
    | ColorTextureResponse (Result WebGL.Texture.Error (Material.Texture Color))
    | RoughnessTextureResponse (Result WebGL.Texture.Error (Material.Texture Float))
    | MetallicTextureResponse (Result WebGL.Texture.Error (Material.Texture Float))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TestCasesResponse (Ok fileContents) ->
            case model of
                Loading loadingModel ->
                    case parseTestCases fileContents of
                        Ok testCases ->
                            ( checkIfLoaded { loadingModel | testCases = Just testCases }
                            , Cmd.none
                            )

                        Err message ->
                            ( Error message, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        TestCasesResponse (Err _) ->
            ( Error "Error loading test cases", Cmd.none )

        ColorTextureResponse (Ok colorTexture) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | colorTexture = Just colorTexture }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ColorTextureResponse (Err _) ->
            ( Error "Error loading color texture", Cmd.none )

        RoughnessTextureResponse (Ok roughnessTexture) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | roughnessTexture = Just roughnessTexture }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        RoughnessTextureResponse (Err _) ->
            ( Error "Error loading roughness texture", Cmd.none )

        MetallicTextureResponse (Ok metallicTexture) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | metallicTexture = Just metallicTexture }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        MetallicTextureResponse (Err _) ->
            ( Error "Error loading metallic texture", Cmd.none )


checkIfLoaded : LoadingModel -> Model
checkIfLoaded loadingModel =
    Maybe.map4 LoadedModel
        loadingModel.testCases
        loadingModel.colorTexture
        loadingModel.roughnessTexture
        loadingModel.metallicTexture
        |> Maybe.map Loaded
        |> Maybe.withDefault (Loading loadingModel)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            Html.text "Loading..."

        Loaded { testCases } ->
            Html.text ("Loaded " ++ String.fromInt (List.length testCases) ++ " test cases")

        Error message ->
            Html.text message
