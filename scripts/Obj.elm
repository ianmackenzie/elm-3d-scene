module Obj exposing (parse)

import Array exposing (Array)
import Dict exposing (Dict)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..), Unitless)
import Set exposing (Set)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)


type alias Vertex units coordinates =
    { position : Point3d units coordinates
    , normal : Vector3d Unitless coordinates
    , uv : ( Float, Float )
    }


parse : String -> TriangularMesh (Vertex units coordinates)
parse fileContents =
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
            List.concatMap parseFace faceLines

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


parsePosition : String -> Maybe (Point3d units coordinates)
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
            Just (Point3d.xyz (Quantity x) (Quantity y) (Quantity z))

        _ ->
            Nothing


parseNormal : String -> Maybe (Vector3d Unitless coordinates)
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
        u :: v :: rest ->
            Just ( u, v )

        _ ->
            Nothing


parseFace : String -> List ( ( Int, Int, Int ), ( Int, Int, Int ), ( Int, Int, Int ) )
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
            [ ( v1, v2, v3 ) ]

        [ v1, v2, v3, v4 ] ->
            [ ( v1, v2, v3 ), ( v1, v3, v4 ) ]

        _ ->
            []


parseFaceVertex : String -> Maybe ( Int, Int, Int )
parseFaceVertex string =
    case String.split "/" string |> List.filterMap String.toInt of
        [ positionIndex, uvIndex, normalIndex ] ->
            Just ( positionIndex - 1, uvIndex - 1, normalIndex - 1 )

        _ ->
            Nothing
