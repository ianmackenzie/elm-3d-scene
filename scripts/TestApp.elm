module TestApp exposing (main)

import Angle exposing (Angle)
import Array exposing (Array)
import Array.Extra as Array
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
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Set
import Sphere3d exposing (Sphere3d)
import Task
import Temperature
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


type Shadow
    = Shadow
    | NoShadow


type Transformation
    = NoTransformation
    | Translation
    | Rotation
    | Scale
    | Mirror


type PointLight
    = PointLight
    | NoPointLight


type DirectionalLight
    = DirectionalLight
    | NoDirectionalLight


type SoftLighting
    = SoftLighting
    | NoSoftLighting


type Antialiasing
    = NoAntialiasing
    | Multisampling
    | Supersampling


type Projection
    = Perspective
    | Orthographic


type alias TestCase =
    { mesh : Mesh
    , material : Material
    , shadow : Shadow
    , transformation : Transformation
    , pointLight : PointLight
    , directionalLight : DirectionalLight
    , softLighting : SoftLighting
    , dynamicRange : Float
    , antialiasing : Antialiasing
    , projection : Projection
    }


toggleMesh : Mesh -> Mesh
toggleMesh mesh =
    case mesh of
        Points ->
            LineSegments

        LineSegments ->
            Polyline

        Polyline ->
            Triangles

        Triangles ->
            Facets

        Facets ->
            Plain

        Plain ->
            Uniform

        Uniform ->
            Unlit

        Unlit ->
            Textured

        Textured ->
            Quad

        Quad ->
            Block

        Block ->
            Sphere

        Sphere ->
            Cylinder

        Cylinder ->
            Points


toggleShadow : Shadow -> Shadow
toggleShadow shadow =
    case shadow of
        Shadow ->
            NoShadow

        NoShadow ->
            Shadow


toggleTransformation : Transformation -> Transformation
toggleTransformation currentTransformation =
    case currentTransformation of
        NoTransformation ->
            Translation

        Translation ->
            Rotation

        Rotation ->
            Scale

        Scale ->
            Mirror

        Mirror ->
            NoTransformation


togglePointLight : PointLight -> PointLight
togglePointLight currentPointLight =
    case currentPointLight of
        PointLight ->
            NoPointLight

        NoPointLight ->
            PointLight


toggleDirectionalLight : DirectionalLight -> DirectionalLight
toggleDirectionalLight currentDirectionalLight =
    case currentDirectionalLight of
        DirectionalLight ->
            NoDirectionalLight

        NoDirectionalLight ->
            DirectionalLight


toggleSoftLighting : SoftLighting -> SoftLighting
toggleSoftLighting currentSoftLighting =
    case currentSoftLighting of
        SoftLighting ->
            NoSoftLighting

        NoSoftLighting ->
            SoftLighting


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


parseShadow : String -> Result String Shadow
parseShadow string =
    case string of
        "NoShadow" ->
            Ok NoShadow

        "Shadow" ->
            Ok Shadow

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


parsePointLight : String -> Result String PointLight
parsePointLight string =
    case string of
        "PointLight" ->
            Ok PointLight

        "NoPointLight" ->
            Ok NoPointLight

        _ ->
            Err ("Unrecognized point light type '" ++ string ++ "'")


parseDirectionalLight : String -> Result String DirectionalLight
parseDirectionalLight string =
    case string of
        "DirectionalLight" ->
            Ok DirectionalLight

        "NoDirectionalLight" ->
            Ok NoDirectionalLight

        _ ->
            Err ("Unrecognized directional light type '" ++ string ++ "'")


parseSoftLighting : String -> Result String SoftLighting
parseSoftLighting string =
    case string of
        "SoftLighting" ->
            Ok SoftLighting

        "NoSoftLighting" ->
            Ok NoSoftLighting

        _ ->
            Err ("Unrecognized soft lighting type '" ++ string ++ "'")


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
            Err ("Unrecognized antialiasing type '" ++ string ++ "'")


parseProjection : String -> Result String Projection
parseProjection string =
    case string of
        "Perspective" ->
            Ok Perspective

        "Orthographic" ->
            Ok Orthographic

        _ ->
            Err ("Unrecognized projection type '" ++ string ++ "'")


parseTestCase : String -> Result String TestCase
parseTestCase line =
    let
        items =
            String.split "\t" line
    in
    case items of
        [ meshString, materialString, shadowString, transformationString, pointLightString, directionalLightString, softLightingString, dynamicRangeString, antialiasingString, projectionString ] ->
            Ok TestCase
                |> Result.Extra.andMap (parseMesh meshString)
                |> Result.Extra.andMap (parseMaterial materialString)
                |> Result.Extra.andMap (parseShadow shadowString)
                |> Result.Extra.andMap (parseTransformation transformationString)
                |> Result.Extra.andMap (parsePointLight pointLightString)
                |> Result.Extra.andMap (parseDirectionalLight directionalLightString)
                |> Result.Extra.andMap (parseSoftLighting softLightingString)
                |> Result.Extra.andMap (parseDynamicRange dynamicRangeString)
                |> Result.Extra.andMap (parseAntialiasing antialiasingString)
                |> Result.Extra.andMap (parseProjection projectionString)

        _ ->
            Err ("Expected 10 items in line, got '" ++ line ++ "' with " ++ String.fromInt (List.length items))


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


addShadow : Shadow -> Mesh.Shadow WorldCoordinates -> Entity WorldCoordinates -> Entity WorldCoordinates
addShadow shadowSetting shadowMesh givenEntity =
    case shadowSetting of
        Shadow ->
            givenEntity |> Scene3d.withShadow shadowMesh

        NoShadow ->
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


pointsEntity : Shadow -> Material.Plain WorldCoordinates -> Entity WorldCoordinates
pointsEntity shadow material =
    Scene3d.mesh material pointsMesh |> addShadow shadow pointsShadow


lineSegmentsMesh : Mesh.Plain WorldCoordinates
lineSegmentsMesh =
    Mesh.lineSegments (Polyline3d.segments polyline)


lineSegmentsShadow : Mesh.Shadow WorldCoordinates
lineSegmentsShadow =
    Mesh.shadow lineSegmentsMesh


lineSegmentsEntity : Shadow -> Material.Plain WorldCoordinates -> Entity WorldCoordinates
lineSegmentsEntity shadow material =
    Scene3d.mesh material lineSegmentsMesh |> addShadow shadow lineSegmentsShadow


polylineMesh : Mesh.Plain WorldCoordinates
polylineMesh =
    Mesh.polyline polyline


polylineShadow : Mesh.Shadow WorldCoordinates
polylineShadow =
    Mesh.shadow polylineMesh


polylineEntity : Shadow -> Material.Plain WorldCoordinates -> Entity WorldCoordinates
polylineEntity shadow material =
    Scene3d.mesh material polylineMesh |> addShadow shadow polylineShadow


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
    Shadow
    -> Material.Plain WorldCoordinates
    -> Mesh.Plain WorldCoordinates
    -> Mesh.Shadow WorldCoordinates
    -> Entity WorldCoordinates
plainEntity shadow material plainMesh plainShadow =
    Scene3d.mesh material plainMesh
        |> addShadow shadow plainShadow
        |> suzanneTransform


uniformEntity :
    Shadow
    -> Material.Uniform WorldCoordinates
    -> Mesh.Uniform WorldCoordinates
    -> Mesh.Shadow WorldCoordinates
    -> Entity WorldCoordinates
uniformEntity shadow material uniformMesh uniformShadow =
    Scene3d.mesh material uniformMesh
        |> addShadow shadow uniformShadow
        |> suzanneTransform


unlitEntity : Shadow -> Material.Unlit WorldCoordinates -> Mesh.Unlit WorldCoordinates -> Mesh.Shadow WorldCoordinates -> Entity WorldCoordinates
unlitEntity shadow material unlitMesh unlitShadow =
    Scene3d.mesh material unlitMesh
        |> addShadow shadow unlitShadow
        |> suzanneTransform


texturedEntity : Shadow -> Material.Textured WorldCoordinates -> Mesh.Textured WorldCoordinates -> Mesh.Shadow WorldCoordinates -> Entity WorldCoordinates
texturedEntity shadow material texturedMesh texturedShadow =
    Scene3d.mesh material texturedMesh
        |> addShadow shadow texturedShadow
        |> suzanneTransform


quadEntity : Shadow -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
quadEntity shadow material =
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
    case shadow of
        Shadow ->
            Scene3d.quad Scene3d.castsShadows material p1 p2 p3 p4

        NoShadow ->
            Scene3d.quad Scene3d.doesNotCastShadows material p1 p2 p3 p4


blockEntity : Shadow -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
blockEntity shadow material =
    let
        block =
            Block3d.from
                (Point3d.meters -1 -1 1)
                (Point3d.meters 1 1 2)
    in
    case shadow of
        Shadow ->
            Scene3d.block Scene3d.castsShadows material block

        NoShadow ->
            Scene3d.block Scene3d.doesNotCastShadows material block


sphereEntity : Shadow -> Material.Textured WorldCoordinates -> Entity WorldCoordinates
sphereEntity shadow material =
    let
        sphere =
            Sphere3d.withRadius (Length.meters 1) (Point3d.meters 0 0 2)
    in
    case shadow of
        Shadow ->
            Scene3d.sphere Scene3d.castsShadows material sphere

        NoShadow ->
            Scene3d.sphere Scene3d.doesNotCastShadows material sphere


cylinderEntity : Shadow -> Material.Uniform WorldCoordinates -> Entity WorldCoordinates
cylinderEntity shadow material =
    let
        cylinder =
            Cylinder3d.along Axis3d.z
                { start = Length.meters 1
                , end = Length.meters 2
                , radius = Length.meters 1
                }
    in
    case shadow of
        Shadow ->
            Scene3d.cylinder Scene3d.castsShadows material cylinder

        NoShadow ->
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
    | ToggleShadow
    | ToggleTransformation
    | TogglePointLight
    | ToggleDirectionalLight
    | ToggleSoftLighting


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateCurrentTestCase function =
            ( case model of
                Loaded loadedModel ->
                    Loaded
                        { loadedModel
                            | testCases =
                                loadedModel.testCases
                                    |> Array.update loadedModel.testCaseIndex function
                        }

                _ ->
                    model
            , Cmd.none
            )
    in
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

        ToggleShadow ->
            updateCurrentTestCase
                (\testCase ->
                    { testCase
                        | shadow = toggleShadow testCase.shadow
                    }
                )

        ToggleTransformation ->
            updateCurrentTestCase
                (\testCase ->
                    { testCase
                        | transformation = toggleTransformation testCase.transformation
                    }
                )

        TogglePointLight ->
            updateCurrentTestCase
                (\testCase ->
                    { testCase
                        | pointLight = togglePointLight testCase.pointLight
                    }
                )

        ToggleDirectionalLight ->
            updateCurrentTestCase
                (\testCase ->
                    { testCase
                        | directionalLight = toggleDirectionalLight testCase.directionalLight
                    }
                )

        ToggleSoftLighting ->
            updateCurrentTestCase
                (\testCase ->
                    { testCase
                        | softLighting = toggleSoftLighting testCase.softLighting
                    }
                )


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
            { chromaticity = Scene3d.fluorescentLighting
            , position = Point3d.meters 0 -4 4
            , intensity = LuminousFlux.lumens 20000
            }

        directionalLightProperties =
            { chromaticity = Scene3d.colorTemperature (Temperature.kelvins 2200)
            , intensity = Illuminance.lux 60
            , direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -30)
            }

        softLighting =
            Scene3d.softLighting
                { upDirection = Direction3d.z
                , chromaticity = Scene3d.incandescentLighting
                , intensityAbove = Illuminance.lux 15
                , intensityBelow = Illuminance.lux 0
                }
    in
    case ( testCase.pointLight, testCase.directionalLight, testCase.softLighting ) of
        ( NoPointLight, NoDirectionalLight, NoSoftLighting ) ->
            Scene3d.noLights

        ( PointLight, NoDirectionalLight, NoSoftLighting ) ->
            case testCase.shadow of
                Shadow ->
                    Scene3d.oneLight <|
                        Scene3d.pointLight Scene3d.castsShadows pointLightProperties

                NoShadow ->
                    Scene3d.oneLight <|
                        Scene3d.pointLight Scene3d.doesNotCastShadows pointLightProperties

        ( NoPointLight, DirectionalLight, NoSoftLighting ) ->
            case testCase.shadow of
                Shadow ->
                    Scene3d.oneLight <|
                        Scene3d.directionalLight Scene3d.castsShadows directionalLightProperties

                NoShadow ->
                    Scene3d.oneLight <|
                        Scene3d.directionalLight Scene3d.doesNotCastShadows directionalLightProperties

        ( PointLight, DirectionalLight, NoSoftLighting ) ->
            let
                pointLight =
                    Scene3d.pointLight Scene3d.doesNotCastShadows pointLightProperties
            in
            case testCase.shadow of
                Shadow ->
                    Scene3d.twoLights
                        (Scene3d.directionalLight Scene3d.castsShadows directionalLightProperties)
                        pointLight

                NoShadow ->
                    Scene3d.twoLights
                        (Scene3d.directionalLight Scene3d.doesNotCastShadows directionalLightProperties)
                        pointLight

        ( NoPointLight, NoDirectionalLight, SoftLighting ) ->
            Scene3d.oneLight softLighting

        ( PointLight, NoDirectionalLight, SoftLighting ) ->
            case testCase.shadow of
                Shadow ->
                    Scene3d.twoLights
                        (Scene3d.pointLight Scene3d.castsShadows pointLightProperties)
                        softLighting

                NoShadow ->
                    Scene3d.twoLights
                        (Scene3d.pointLight Scene3d.doesNotCastShadows pointLightProperties)
                        softLighting

        ( NoPointLight, DirectionalLight, SoftLighting ) ->
            case testCase.shadow of
                Shadow ->
                    Scene3d.twoLights
                        (Scene3d.directionalLight Scene3d.castsShadows directionalLightProperties)
                        softLighting

                NoShadow ->
                    Scene3d.twoLights
                        (Scene3d.directionalLight Scene3d.doesNotCastShadows directionalLightProperties)
                        softLighting

        ( PointLight, DirectionalLight, SoftLighting ) ->
            let
                pointLight =
                    Scene3d.pointLight Scene3d.doesNotCastShadows pointLightProperties
            in
            case testCase.shadow of
                Shadow ->
                    Scene3d.threeLights
                        (Scene3d.directionalLight Scene3d.castsShadows directionalLightProperties)
                        pointLight
                        softLighting

                NoShadow ->
                    Scene3d.threeLights
                        (Scene3d.directionalLight Scene3d.doesNotCastShadows directionalLightProperties)
                        pointLight
                        softLighting


camera : TestCase -> Camera3d Meters WorldCoordinates
camera testCase =
    let
        focalPoint =
            Point3d.meters 0 0 0.5

        eyePoint =
            Point3d.meters 10 5 7

        verticalFieldOfView =
            Angle.degrees 30

        focalDistance =
            Point3d.distanceFrom eyePoint focalPoint

        viewportHeight =
            focalDistance
                |> Quantity.multiplyBy
                    (2 * Angle.tan (Quantity.half verticalFieldOfView))

        viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = focalPoint
                , eyePoint = eyePoint
                , upDirection = Direction3d.z
                }
    in
    case testCase.projection of
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


antialiasing : TestCase -> Scene3d.Antialiasing
antialiasing testCase =
    case testCase.antialiasing of
        NoAntialiasing ->
            Scene3d.noAntialiasing

        Multisampling ->
            Scene3d.multisampling

        Supersampling ->
            Scene3d.supersampling 2


toneMapping : TestCase -> Scene3d.ToneMapping
toneMapping testCase =
    Scene3d.reinhardToneMapping testCase.dynamicRange


viewTestCaseProperties : Int -> TestCase -> Element Msg
viewTestCaseProperties testCaseIndex testCase =
    Element.table [ Element.Font.size 14, Element.spacingXY 10 3 ]
        { data =
            [ ( "Test case:", String.fromInt (testCaseIndex + 1), Nothing )
            , ( "Mesh:", Debug.toString testCase.mesh, Nothing )
            , ( "Material:", Debug.toString testCase.material, Nothing )
            , ( "Shadow:", Debug.toString testCase.shadow, Just ToggleShadow )
            , ( "Transformation:", Debug.toString testCase.transformation, Just ToggleTransformation )
            , ( "Point light:", Debug.toString testCase.pointLight, Just TogglePointLight )
            , ( "Directional light:", Debug.toString testCase.directionalLight, Just ToggleDirectionalLight )
            , ( "Soft lighting:", Debug.toString testCase.softLighting, Just ToggleSoftLighting )
            , ( "Dynamic range:", Debug.toString testCase.dynamicRange, Nothing )
            , ( "Antialiasing:", Debug.toString testCase.antialiasing, Nothing )
            , ( "Projection:", Debug.toString testCase.projection, Nothing )
            ]
        , columns =
            [ { header = Element.none, width = Element.shrink, view = \( property, _, _ ) -> Element.text property }
            , { header = Element.none
              , width = Element.shrink
              , view =
                    \( _, value, maybeMessage ) ->
                        case maybeMessage of
                            Nothing ->
                                Element.text value

                            Just message ->
                                Input.button
                                    [ Element.Border.rounded 3
                                    , Element.Border.solid
                                    , Element.Border.width 1
                                    , Element.Border.color (Element.rgb 0.25 0.25 0.25)
                                    ]
                                    { onPress = Just message
                                    , label = Element.text value
                                    }
              }
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


floor : Entity WorldCoordinates
floor =
    Scene3d.quad Scene3d.doesNotCastShadows
        (Material.matte Tango.aluminum3)
        (Point3d.meters 4 -4 0)
        (Point3d.meters 4 4 0)
        (Point3d.meters -4 4 0)
        (Point3d.meters -4 -4 0)


viewTestCase : LoadedModel -> TestCase -> Element Msg
viewTestCase model testCase =
    case entity model testCase of
        Just validEntity ->
            Element.row [ Element.spacing 10 ]
                [ Element.el [] <|
                    Element.html <|
                        Scene3d.toHtml
                            { lights = lights testCase
                            , background = Scene3d.backgroundColor Tango.skyBlue1
                            , camera = camera testCase
                            , clipDepth = Length.meters 1
                            , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
                            , antialiasing = antialiasing testCase
                            , exposure = Scene3d.exposureValue 4
                            , toneMapping = toneMapping testCase
                            , whiteBalance = Scene3d.incandescentLighting
                            }
                            [ floor
                            , axes
                            , validEntity |> transformation testCase
                            ]
                , viewTestCaseProperties model.testCaseIndex testCase
                ]

        Nothing ->
            Element.text "ERROR: Invalid test case"
