module TestApp exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d exposing (Cylinder3d)
import Dict
import Direction3d exposing (Direction3d)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Http
import Illuminance
import Json.Decode as Decode
import Length exposing (Meters)
import LineSegment3d exposing (LineSegment3d)
import Luminance
import LuminousFlux
import Palette.Tango as Tango
import Pixels
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity, Unitless)
import Quantity.Interval as Interval exposing (Interval)
import Result.Extra
import Scene3d exposing (Entity)
import Scene3d.Chromaticity as Chromaticity
import Scene3d.Exposure as Exposure
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Set
import Sphere3d exposing (Sphere3d)
import Task
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)
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
        [ meshString, materialString, shadowString, transformationString, lightSourcesString, environmentalLightingString, dynamicRangeString, antialiasingString ] ->
            Ok TestCase
                |> Result.Extra.andMap (parseMesh meshString)
                |> Result.Extra.andMap (parseMaterial materialString)
                |> Result.Extra.andMap (parseShadow shadowString)
                |> Result.Extra.andMap (parseTransformation transformationString)
                |> Result.Extra.andMap (parseLightSources lightSourcesString)
                |> Result.Extra.andMap (parseEnvironmentalLighting environmentalLightingString)
                |> Result.Extra.andMap (parseDynamicRange dynamicRangeString)
                |> Result.Extra.andMap (parseAntialiasing antialiasingString)

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
    , entity : Entity WorldCoordinates
    }


addShadowIf : Bool -> Mesh.Shadow WorldCoordinates -> Entity WorldCoordinates -> Entity WorldCoordinates
addShadowIf enabled shadow givenEntity =
    if enabled then
        givenEntity |> Scene3d.withShadow shadow

    else
        givenEntity


polyline : Polyline3d Meters WorldCoordinates
polyline =
    Polyline3d.fromVertices
        [ Point3d.meters -1 -1 1
        , Point3d.meters 0 -1 1
        , Point3d.meters 0 0 1
        , Point3d.meters 0 1 1
        , Point3d.meters 1 1 1
        ]


pointsMesh : Mesh.Plain WorldCoordinates
pointsMesh =
    Mesh.points { radius = Pixels.pixels 10 } (Polyline3d.vertices polyline)


pointsShadow : Mesh.Shadow WorldCoordinates
pointsShadow =
    Mesh.shadow pointsMesh


pointsEntity : Bool -> Material.Plain WorldCoordinates -> Entity WorldCoordinates
pointsEntity castsShadows material =
    Scene3d.mesh material pointsMesh |> addShadowIf castsShadows pointsShadow


lineSegmentsMesh : Mesh.Plain WorldCoordinates
lineSegmentsMesh =
    Mesh.lineSegments (Polyline3d.segments polyline)


lineSegmentsShadow : Mesh.Shadow WorldCoordinates
lineSegmentsShadow =
    Mesh.shadow lineSegmentsMesh


lineSegmentsEntity : Bool -> Material.Plain WorldCoordinates -> Entity WorldCoordinates
lineSegmentsEntity castsShadows material =
    Scene3d.mesh material lineSegmentsMesh |> addShadowIf castsShadows lineSegmentsShadow


polylineMesh : Mesh.Plain WorldCoordinates
polylineMesh =
    Mesh.polyline polyline


polylineShadow : Mesh.Shadow WorldCoordinates
polylineShadow =
    Mesh.shadow polylineMesh


polylineEntity : Bool -> Material.Plain WorldCoordinates -> Entity WorldCoordinates
polylineEntity castsShadows material =
    Scene3d.mesh material polylineMesh |> addShadowIf castsShadows polylineShadow


type alias Vertex =
    { position : Point3d Meters WorldCoordinates
    , normal : Vector3d Unitless WorldCoordinates
    , uv : ( Float, Float )
    }


suzanneTransform : Entity WorldCoordinates -> Entity WorldCoordinates
suzanneTransform =
    Scene3d.rotateAround Axis3d.z (Angle.degrees 90)
        >> Scene3d.rotateAround
            (Axis3d.through (Point3d.meters 0 0 1) Direction3d.y)
            (Angle.degrees 90)
        >> Scene3d.translateBy (Vector3d.meters 1 0 1)


plainEntity :
    Bool
    -> Material.Plain WorldCoordinates
    -> Mesh.Plain WorldCoordinates
    -> Mesh.Shadow WorldCoordinates
    -> Entity WorldCoordinates
plainEntity castsShadows material plainMesh plainShadow =
    Scene3d.mesh material plainMesh
        |> addShadowIf castsShadows plainShadow
        |> suzanneTransform


uniformEntity :
    Bool
    -> Material.Uniform WorldCoordinates
    -> Mesh.Uniform WorldCoordinates
    -> Mesh.Shadow WorldCoordinates
    -> Entity WorldCoordinates
uniformEntity castsShadows material uniformMesh uniformShadow =
    Scene3d.mesh material uniformMesh
        |> addShadowIf castsShadows uniformShadow
        |> suzanneTransform


unlitEntity : Bool -> Material.Unlit WorldCoordinates -> Mesh.Unlit WorldCoordinates -> Mesh.Shadow WorldCoordinates -> Entity WorldCoordinates
unlitEntity castsShadows material unlitMesh unlitShadow =
    Scene3d.mesh material unlitMesh
        |> addShadowIf castsShadows unlitShadow
        |> suzanneTransform


texturedEntity : Bool -> Material.Textured WorldCoordinates -> Mesh.Textured WorldCoordinates -> Mesh.Shadow WorldCoordinates -> Entity WorldCoordinates
texturedEntity castsShadows material texturedMesh texturedShadow =
    Scene3d.mesh material texturedMesh
        |> addShadowIf castsShadows texturedShadow
        |> suzanneTransform


quadEntity : Bool -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
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
    if castsShadows then
        Scene3d.quad Scene3d.castsShadows material p1 p2 p3 p4

    else
        Scene3d.quad Scene3d.doesNotCastShadows material p1 p2 p3 p4


blockEntity : Bool -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
blockEntity castsShadows material =
    let
        block =
            Block3d.from
                (Point3d.meters -1 -1 1)
                (Point3d.meters 1 1 2)
    in
    if castsShadows then
        Scene3d.block Scene3d.castsShadows material block

    else
        Scene3d.block Scene3d.doesNotCastShadows material block


sphereEntity : Bool -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
sphereEntity castsShadows material =
    let
        sphere =
            Sphere3d.withRadius (Length.meters 1) (Point3d.meters 0 0 2)
    in
    if castsShadows then
        Scene3d.sphere Scene3d.castsShadows material sphere

    else
        Scene3d.sphere Scene3d.doesNotCastShadows material sphere


cylinderEntity : Bool -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
cylinderEntity castsShadows material =
    let
        cylinder =
            Cylinder3d.along Axis3d.z
                { start = Length.meters 1
                , end = Length.meters 2
                , radius = Length.meters 1
                }
    in
    if castsShadows then
        Scene3d.cylinder Scene3d.castsShadows material cylinder

    else
        Scene3d.cylinder Scene3d.doesNotCastShadows material cylinder


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
    , suzanneMesh : Maybe (TriangularMesh Vertex)
    }


type alias LoadedModel =
    { testCases : Array TestCase
    , testCaseIndex : Int
    , colorTexture : Material.Texture Color
    , roughnessTexture : Material.Texture Float
    , metallicTexture : Material.Texture Float
    , trianglesMesh : Mesh.Plain WorldCoordinates
    , trianglesShadow : Mesh.Shadow WorldCoordinates
    , facetsMesh : Mesh.Uniform WorldCoordinates
    , facetsShadow : Mesh.Shadow WorldCoordinates
    , plainMesh : Mesh.Plain WorldCoordinates
    , plainShadow : Mesh.Shadow WorldCoordinates
    , uniformMesh : Mesh.Uniform WorldCoordinates
    , uniformShadow : Mesh.Shadow WorldCoordinates
    , unlitMesh : Mesh.Unlit WorldCoordinates
    , unlitShadow : Mesh.Shadow WorldCoordinates
    , texturedMesh : Mesh.Textured WorldCoordinates
    , texturedShadow : Mesh.Shadow WorldCoordinates
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
        , suzanneMesh = Nothing
        }
    , Cmd.batch
        [ Http.get
            { url = "test_cases_2.txt"
            , expect = Http.expectString TestCasesResponse
            }
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_col.jpg"
            |> Task.attempt ColorTextureResponse
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_rgh.jpg"
            |> Task.attempt RoughnessTextureResponse
        , Material.load "https://ianmackenzie.github.io/elm-3d-scene/examples/metal/Metal03_met.jpg"
            |> Task.attempt MetallicTextureResponse
        , Http.get
            { url = "https://ianmackenzie.github.io/elm-3d-scene/examples/suzanne.obj"
            , expect = Http.expectString SuzanneMeshResponse
            }
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
    | SuzanneMeshResponse (Result Http.Error String)
    | Next
    | Previous
    | First
    | Last


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

        SuzanneMeshResponse (Ok fileContents) ->
            case model of
                Loading loadingModel ->
                    ( checkIfLoaded { loadingModel | suzanneMesh = Just (parseObj fileContents) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SuzanneMeshResponse (Err _) ->
            ( Error "Error loading Suzanne mesh", Cmd.none )

        Previous ->
            case model of
                Loaded loadedModel ->
                    ( Loaded
                        { loadedModel
                            | testCaseIndex = max 0 (loadedModel.testCaseIndex - 1)
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Next ->
            case model of
                Loaded loadedModel ->
                    ( Loaded
                        { loadedModel
                            | testCaseIndex = min (loadedModel.testCaseIndex + 1) (Array.length loadedModel.testCases - 1)
                        }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        First ->
            case model of
                Loaded loadedModel ->
                    ( Loaded { loadedModel | testCaseIndex = 0 }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        Last ->
            case model of
                Loaded loadedModel ->
                    ( Loaded { loadedModel | testCaseIndex = Array.length loadedModel.testCases - 1 }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


parseObj : String -> TriangularMesh Vertex
parseObj fileContents =
    let
        lines =
            String.lines fileContents

        positionLines =
            lines |> List.filter (String.startsWith "v ")

        uvLines =
            lines |> List.filter (String.startsWith "vt ")

        normalLines =
            lines |> List.filter (String.startsWith "vn ")

        faceLines =
            lines |> List.filter (String.startsWith "f ")

        positions =
            Array.fromList (List.filterMap parsePosition positionLines)

        uvs =
            Array.fromList (List.filterMap parseUv uvLines)

        normals =
            Array.fromList (List.filterMap parseNormal normalLines)

        faceVertexIndices =
            List.filterMap parseFace faceLines

        uniqueVertices =
            faceVertexIndices
                |> List.concatMap (\( v1, v2, v3 ) -> [ v1, v2, v3 ])
                |> Set.fromList
                |> Set.toList
                |> List.filterMap
                    (\(( positionIndex, uvIndex, normalIndex ) as vertexIndices) ->
                        Maybe.map3
                            (\position uv normal ->
                                ( vertexIndices
                                , { position = position, uv = uv, normal = normal }
                                )
                            )
                            (Array.get positionIndex positions)
                            (Array.get uvIndex uvs)
                            (Array.get normalIndex normals)
                    )

        vertexArray =
            uniqueVertices |> List.map Tuple.second |> Array.fromList

        vertexIndexDict =
            uniqueVertices
                |> List.indexedMap (\index ( vertexIndices, _ ) -> ( vertexIndices, index ))
                |> Dict.fromList

        faceIndices =
            faceVertexIndices
                |> List.filterMap
                    (\( v1, v2, v3 ) ->
                        Maybe.map3
                            (\i j k -> ( i, j, k ))
                            (Dict.get v1 vertexIndexDict)
                            (Dict.get v2 vertexIndexDict)
                            (Dict.get v3 vertexIndexDict)
                    )
    in
    TriangularMesh.indexed vertexArray faceIndices


parsePosition : String -> Maybe (Point3d Meters WorldCoordinates)
parsePosition line =
    let
        coordinates =
            line
                |> String.dropLeft 2
                |> String.words
                |> List.filterMap String.toFloat
    in
    case coordinates of
        [ x, y, z ] ->
            Just (Point3d.meters x y z)

        _ ->
            Nothing


parseNormal : String -> Maybe (Vector3d Unitless WorldCoordinates)
parseNormal line =
    let
        coordinates =
            line
                |> String.dropLeft 3
                |> String.words
                |> List.filterMap String.toFloat
    in
    case coordinates of
        [ x, y, z ] ->
            Just (Vector3d.unitless x y z)

        _ ->
            Nothing


parseUv : String -> Maybe ( Float, Float )
parseUv line =
    let
        coordinates =
            line
                |> String.dropLeft 3
                |> String.words
                |> List.filterMap String.toFloat
    in
    case coordinates of
        [ u, v ] ->
            Just ( u, v )

        _ ->
            Nothing


parseFace : String -> Maybe ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) )
parseFace line =
    let
        vertices =
            line
                |> String.dropLeft 2
                |> String.words
                |> List.filterMap parseFaceVertex
    in
    case vertices of
        [ v1, v2, v3 ] ->
            Just ( v1, v2, v3 )

        _ ->
            Nothing


parseFaceVertex : String -> Maybe ( Int, Int, Int )
parseFaceVertex string =
    case String.split "/" string |> List.filterMap String.toInt of
        [ positionIndex, uvIndex, normalIndex ] ->
            Just ( positionIndex - 1, uvIndex - 1, normalIndex - 1 )

        _ ->
            Nothing


testCaseArray : List TestCase -> Array TestCase
testCaseArray testCases =
    let
        filteredTestCases =
            testCases
                |> List.filter
                    (\testCase ->
                        List.all (List.any ((==) True))
                            []
                    )

        ( firstGroup, secondGroup ) =
            List.partition (.antialiasing >> (==) Multisampling)
                filteredTestCases
    in
    Array.fromList (firstGroup ++ secondGroup)


checkIfLoaded : LoadingModel -> Model
checkIfLoaded loadingModel =
    Maybe.map5
        (\testCases colorTexture roughnessTexture metallicTexture suzanneMesh ->
            let
                plainTriangularMesh =
                    TriangularMesh.mapVertices .position suzanneMesh

                triangles =
                    List.map Triangle3d.fromVertices
                        (TriangularMesh.faceVertices plainTriangularMesh)

                trianglesMesh =
                    Mesh.triangles triangles

                facetsMesh =
                    Mesh.facets triangles

                plainMesh =
                    Mesh.plain plainTriangularMesh

                uniformMesh =
                    Mesh.uniform
                        (TriangularMesh.mapVertices
                            (\{ position, normal } -> { position = position, normal = normal })
                            suzanneMesh
                        )

                unlitMesh =
                    Mesh.unlit
                        (TriangularMesh.mapVertices
                            (\{ position, uv } -> { position = position, uv = uv })
                            suzanneMesh
                        )

                texturedMesh =
                    Mesh.textured suzanneMesh
            in
            { testCases = testCaseArray testCases
            , testCaseIndex = 0
            , colorTexture = colorTexture
            , roughnessTexture = roughnessTexture
            , metallicTexture = metallicTexture
            , trianglesMesh = trianglesMesh
            , trianglesShadow = Mesh.shadow trianglesMesh
            , facetsMesh = facetsMesh
            , facetsShadow = Mesh.shadow facetsMesh
            , plainMesh = plainMesh
            , plainShadow = Mesh.shadow plainMesh
            , uniformMesh = uniformMesh
            , uniformShadow = Mesh.shadow uniformMesh
            , unlitMesh = unlitMesh
            , unlitShadow = Mesh.shadow unlitMesh
            , texturedMesh = texturedMesh
            , texturedShadow = Mesh.shadow texturedMesh
            }
        )
        loadingModel.testCases
        loadingModel.colorTexture
        loadingModel.roughnessTexture
        loadingModel.metallicTexture
        loadingModel.suzanneMesh
        |> Maybe.map Loaded
        |> Maybe.withDefault (Loading loadingModel)


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onKeyDown
        (Decode.field "key" Decode.string
            |> Decode.andThen
                (\key ->
                    case key of
                        "ArrowLeft" ->
                            Decode.succeed Previous

                        "ArrowRight" ->
                            Decode.succeed Next

                        "ArrowUp" ->
                            Decode.succeed Previous

                        "ArrowDown" ->
                            Decode.succeed Next

                        "Home" ->
                            Decode.succeed First

                        "End" ->
                            Decode.succeed Last

                        _ ->
                            Decode.fail "Unrecognized key"
                )
        )


view : Model -> Html Msg
view model =
    case model of
        Loading _ ->
            Html.text "Loading..."

        Loaded loadedModel ->
            case Array.get loadedModel.testCaseIndex loadedModel.testCases of
                Just currentTestCase ->
                    let
                        buttonAttributes =
                            [ Element.paddingXY 10 0
                            , Element.Border.rounded 5
                            , Element.Border.solid
                            , Element.Border.width 1
                            ]
                    in
                    Element.layout [] <|
                        Element.column []
                            [ viewTestCase loadedModel currentTestCase
                            ]

                Nothing ->
                    Html.text ""

        Error message ->
            Html.text message


entity : LoadedModel -> TestCase -> Maybe (Entity WorldCoordinates)
entity model testCase =
    case testCase.material of
        Color ->
            let
                material =
                    Material.color Tango.skyBlue2
            in
            case testCase.mesh of
                Points ->
                    Just (pointsEntity testCase.shadow material)

                LineSegments ->
                    Just (lineSegmentsEntity testCase.shadow material)

                Polyline ->
                    Just (polylineEntity testCase.shadow material)

                Triangles ->
                    Just (plainEntity testCase.shadow material model.trianglesMesh model.trianglesShadow)

                Facets ->
                    Just (uniformEntity testCase.shadow material model.facetsMesh model.facetsShadow)

                Plain ->
                    Just (plainEntity testCase.shadow material model.plainMesh model.plainShadow)

                Uniform ->
                    Just (uniformEntity testCase.shadow material model.uniformMesh model.uniformShadow)

                Unlit ->
                    Just (unlitEntity testCase.shadow material model.unlitMesh model.unlitShadow)

                Textured ->
                    Just (texturedEntity testCase.shadow material model.texturedMesh model.texturedShadow)

                Quad ->
                    Just (quadEntity testCase.shadow material)

                Block ->
                    Just (blockEntity testCase.shadow material)

                Sphere ->
                    Just (sphereEntity testCase.shadow material)

                Cylinder ->
                    Just (cylinderEntity testCase.shadow material)

        Emissive ->
            let
                material =
                    Material.emissive Tango.orange2 (Luminance.nits 250)
            in
            case testCase.mesh of
                Points ->
                    Just (pointsEntity testCase.shadow material)

                LineSegments ->
                    Just (lineSegmentsEntity testCase.shadow material)

                Polyline ->
                    Just (polylineEntity testCase.shadow material)

                Triangles ->
                    Just (plainEntity testCase.shadow material model.trianglesMesh model.trianglesShadow)

                Facets ->
                    Just (uniformEntity testCase.shadow material model.facetsMesh model.facetsShadow)

                Plain ->
                    Just (plainEntity testCase.shadow material model.plainMesh model.plainShadow)

                Uniform ->
                    Just (uniformEntity testCase.shadow material model.uniformMesh model.uniformShadow)

                Unlit ->
                    Just (unlitEntity testCase.shadow material model.unlitMesh model.unlitShadow)

                Textured ->
                    Just (texturedEntity testCase.shadow material model.texturedMesh model.texturedShadow)

                Quad ->
                    Just (quadEntity testCase.shadow material)

                Block ->
                    Just (blockEntity testCase.shadow material)

                Sphere ->
                    Just (sphereEntity testCase.shadow material)

                Cylinder ->
                    Just (cylinderEntity testCase.shadow material)

        Matte ->
            let
                material =
                    Material.matte Tango.skyBlue2
            in
            case testCase.mesh of
                Points ->
                    Nothing

                LineSegments ->
                    Nothing

                Polyline ->
                    Nothing

                Triangles ->
                    Nothing

                Facets ->
                    Just (uniformEntity testCase.shadow material model.facetsMesh model.facetsShadow)

                Plain ->
                    Nothing

                Uniform ->
                    Just (uniformEntity testCase.shadow material model.uniformMesh model.uniformShadow)

                Unlit ->
                    Nothing

                Textured ->
                    Just (texturedEntity testCase.shadow material model.texturedMesh model.texturedShadow)

                Quad ->
                    Just (quadEntity testCase.shadow material)

                Block ->
                    Just (blockEntity testCase.shadow material)

                Sphere ->
                    Just (sphereEntity testCase.shadow material)

                Cylinder ->
                    Just (cylinderEntity testCase.shadow material)

        Pbr ->
            let
                material =
                    Material.nonmetal
                        { baseColor = Tango.skyBlue2
                        , roughness = 0.25
                        }
            in
            case testCase.mesh of
                Points ->
                    Nothing

                LineSegments ->
                    Nothing

                Polyline ->
                    Nothing

                Triangles ->
                    Nothing

                Facets ->
                    Just (uniformEntity testCase.shadow material model.facetsMesh model.facetsShadow)

                Plain ->
                    Nothing

                Uniform ->
                    Just (uniformEntity testCase.shadow material model.uniformMesh model.uniformShadow)

                Unlit ->
                    Nothing

                Textured ->
                    Just (texturedEntity testCase.shadow material model.texturedMesh model.texturedShadow)

                Quad ->
                    Just (quadEntity testCase.shadow material)

                Block ->
                    Just (blockEntity testCase.shadow material)

                Sphere ->
                    Just (sphereEntity testCase.shadow material)

                Cylinder ->
                    Just (cylinderEntity testCase.shadow material)

        TexturedColor ->
            let
                material =
                    Material.texturedColor model.colorTexture
            in
            case testCase.mesh of
                Points ->
                    Nothing

                LineSegments ->
                    Nothing

                Polyline ->
                    Nothing

                Triangles ->
                    Nothing

                Facets ->
                    Nothing

                Plain ->
                    Nothing

                Uniform ->
                    Nothing

                Unlit ->
                    Just (unlitEntity testCase.shadow material model.unlitMesh model.unlitShadow)

                Textured ->
                    Just (texturedEntity testCase.shadow material model.texturedMesh model.texturedShadow)

                Quad ->
                    Just (quadEntity testCase.shadow material)

                Block ->
                    Nothing

                Sphere ->
                    Just (sphereEntity testCase.shadow material)

                Cylinder ->
                    Nothing

        TexturedEmissive ->
            let
                material =
                    Material.texturedEmissive model.colorTexture (Luminance.nits 10)
            in
            case testCase.mesh of
                Points ->
                    Nothing

                LineSegments ->
                    Nothing

                Polyline ->
                    Nothing

                Triangles ->
                    Nothing

                Facets ->
                    Nothing

                Plain ->
                    Nothing

                Uniform ->
                    Nothing

                Unlit ->
                    Just (unlitEntity testCase.shadow material model.unlitMesh model.unlitShadow)

                Textured ->
                    Just (texturedEntity testCase.shadow material model.texturedMesh model.texturedShadow)

                Quad ->
                    Just (quadEntity testCase.shadow material)

                Block ->
                    Nothing

                Sphere ->
                    Just (sphereEntity testCase.shadow material)

                Cylinder ->
                    Nothing

        TexturedMatte ->
            let
                material =
                    Material.texturedMatte model.colorTexture
            in
            case testCase.mesh of
                Points ->
                    Nothing

                LineSegments ->
                    Nothing

                Polyline ->
                    Nothing

                Triangles ->
                    Nothing

                Facets ->
                    Nothing

                Plain ->
                    Nothing

                Uniform ->
                    Nothing

                Unlit ->
                    Nothing

                Textured ->
                    Just (texturedEntity testCase.shadow material model.texturedMesh model.texturedShadow)

                Quad ->
                    Just (quadEntity testCase.shadow material)

                Block ->
                    Nothing

                Sphere ->
                    Just (sphereEntity testCase.shadow material)

                Cylinder ->
                    Nothing

        TexturedPbr ->
            let
                material =
                    Material.texturedPbr
                        { baseColor = model.colorTexture
                        , roughness = model.roughnessTexture
                        , metallic = model.metallicTexture
                        }
            in
            case testCase.mesh of
                Points ->
                    Nothing

                LineSegments ->
                    Nothing

                Polyline ->
                    Nothing

                Triangles ->
                    Nothing

                Facets ->
                    Nothing

                Plain ->
                    Nothing

                Uniform ->
                    Nothing

                Unlit ->
                    Nothing

                Textured ->
                    Just (texturedEntity testCase.shadow material model.texturedMesh model.texturedShadow)

                Quad ->
                    Just (quadEntity testCase.shadow material)

                Block ->
                    Nothing

                Sphere ->
                    Just (sphereEntity testCase.shadow material)

                Cylinder ->
                    Nothing


transformation : TestCase -> Entity WorldCoordinates -> Entity WorldCoordinates
transformation testCase =
    case testCase.transformation of
        NoTransformation ->
            identity

        Translation ->
            Scene3d.translateIn Direction3d.x (Length.meters 1)

        Rotation ->
            Scene3d.rotateAround Axis3d.z (Angle.degrees 45)

        Scale ->
            Scene3d.scaleAbout (Point3d.meters 0 0 1) 1.5

        Mirror ->
            Scene3d.mirrorAcross (Plane3d.yz |> Plane3d.translateIn Direction3d.x (Length.meters -0.5))


lights : TestCase -> Scene3d.Lights WorldCoordinates
lights testCase =
    let
        pointLightProperties =
            { chromaticity = Chromaticity.fluorescent
            , position = Point3d.meters 0 -4 4
            , intensity = LuminousFlux.lumens 20000
            }

        directionalLightProperties =
            { chromaticity = Chromaticity.kelvins 2200
            , intensity = Illuminance.lux 60
            , direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -30)
            }
    in
    case testCase.lightSources of
        NoLightSources ->
            Scene3d.noLights

        PointLight ->
            if testCase.shadow then
                Scene3d.oneLight <|
                    Scene3d.pointLight Scene3d.castsShadows pointLightProperties

            else
                Scene3d.oneLight <|
                    Scene3d.pointLight Scene3d.doesNotCastShadows pointLightProperties

        DirectionalLight ->
            if testCase.shadow then
                Scene3d.oneLight <|
                    Scene3d.directionalLight Scene3d.castsShadows directionalLightProperties

            else
                Scene3d.oneLight <|
                    Scene3d.directionalLight Scene3d.doesNotCastShadows directionalLightProperties

        TwoLightSources ->
            let
                pointLight =
                    Scene3d.pointLight Scene3d.doesNotCastShadows pointLightProperties
            in
            if testCase.shadow then
                Scene3d.twoLights
                    (Scene3d.directionalLight Scene3d.castsShadows directionalLightProperties)
                    pointLight

            else
                Scene3d.twoLights
                    (Scene3d.directionalLight Scene3d.doesNotCastShadows directionalLightProperties)
                    pointLight


environmentalLighting : TestCase -> Scene3d.EnvironmentalLighting WorldCoordinates
environmentalLighting testCase =
    case testCase.environmentalLighting of
        NoEnvironmentalLighting ->
            Scene3d.noEnvironmentalLighting

        SoftLighting ->
            Scene3d.softLighting
                { upDirection = Direction3d.z
                , above = ( Luminance.nits 5, Chromaticity.tungsten )
                , below = ( Luminance.nits 1, Chromaticity.tungsten )
                }


options : TestCase -> List Scene3d.Option
options testCase =
    Scene3d.dynamicRange testCase.dynamicRange
        :: (case testCase.antialiasing of
                NoAntialiasing ->
                    [ Scene3d.multisampling False ]

                Multisampling ->
                    [ Scene3d.multisampling True ]

                Supersampling ->
                    [ Scene3d.multisampling False, Scene3d.supersampling 2 ]
           )


viewTestCaseProperties : Int -> TestCase -> Element Msg
viewTestCaseProperties testCaseIndex testCase =
    Element.table [ Element.Font.size 14, Element.spacingXY 10 0 ]
        { data =
            [ ( "Test case:", String.fromInt (testCaseIndex + 1) )
            , ( "Mesh:", Debug.toString testCase.mesh )
            , ( "Material:", Debug.toString testCase.material )
            , ( "Shadow:", Debug.toString testCase.shadow )
            , ( "Transformation:", Debug.toString testCase.transformation )
            , ( "Light sources:", Debug.toString testCase.lightSources )
            , ( "Environmental lighting:", Debug.toString testCase.environmentalLighting )
            , ( "Dynamic range:", Debug.toString testCase.dynamicRange )
            , ( "Antialiasing:", Debug.toString testCase.antialiasing )
            ]
        , columns =
            [ { header = Element.none, width = Element.shrink, view = \( property, _ ) -> Element.text property }
            , { header = Element.none, width = Element.shrink, view = \( _, value ) -> Element.text value }
            ]
        }


axes : Entity WorldCoordinates
axes =
    let
        xAxisMesh =
            Mesh.lineSegments
                [ LineSegment3d.from
                    (Point3d.meters 0 0 0.01)
                    (Point3d.meters 3 0 0.01)
                ]

        yAxisMesh =
            Mesh.lineSegments
                [ LineSegment3d.from
                    (Point3d.meters 0 0 0.01)
                    (Point3d.meters 0 3 0.01)
                ]

        zAxisMesh =
            Mesh.lineSegments
                [ LineSegment3d.from
                    (Point3d.meters 0 0 0.01)
                    (Point3d.meters 0 0 3.01)
                ]
    in
    Scene3d.group
        [ Scene3d.mesh (Material.color Tango.scarletRed1) xAxisMesh
        , Scene3d.mesh (Material.color Tango.chameleon1) yAxisMesh
        , Scene3d.mesh (Material.color Tango.skyBlue1) zAxisMesh
        ]


viewTestCase : LoadedModel -> TestCase -> Element Msg
viewTestCase model testCase =
    case entity model testCase of
        Just validEntity ->
            Element.row [ Element.spacing 10 ]
                [ Element.el [] <|
                    Element.html <|
                        Scene3d.toHtml (options testCase)
                            { lights = lights testCase
                            , environmentalLighting = environmentalLighting testCase
                            , background = Scene3d.backgroundColor Tango.skyBlue1
                            , camera =
                                Camera3d.perspective
                                    { viewpoint =
                                        Viewpoint3d.lookAt
                                            { focalPoint = Point3d.meters 0 0 0.5
                                            , eyePoint = Point3d.meters 10 5 7
                                            , upDirection = Direction3d.z
                                            }
                                    , clipDepth = Length.meters 1
                                    , verticalFieldOfView = Angle.degrees 30
                                    }
                            , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                            , exposure = Exposure.fromEv100 4
                            , whiteBalance = Chromaticity.tungsten
                            }
                            [ Scene3d.quad Scene3d.doesNotCastShadows
                                (Material.matte Tango.aluminum3)
                                (Point3d.meters 4 -4 0)
                                (Point3d.meters 4 4 0)
                                (Point3d.meters -4 4 0)
                                (Point3d.meters -4 -4 0)
                            , axes
                            , validEntity |> transformation testCase
                            ]
                , viewTestCaseProperties model.testCaseIndex testCase
                ]

        Nothing ->
            Element.text "ERROR: Invalid test case"
