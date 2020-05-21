module Scene3d.Mesh.Decode exposing
    ( points, lineSegments, polyline, triangles, facets
    , indexedTriangles, indexedFacets, indexedFaces
    , texturedTriangles, texturedFacets, texturedFaces
    , shadow
    )

{-| These JSON decoders allow you to decode meshes that have been saved as JSON
using [`Encode.mesh`](Scene3d-Mesh-Encode#mesh) or [`Encode.shadow`](Scene3d-Mesh-Encode#shadow)
(or from JSON that you generate yourself; read on!). All the decoders correspond
to particular functions in the [`Mesh`](Scene3d-Mesh) module. For example, if
you had

    facetsMesh =
        Scene3d.Mesh.facets listOfTriangles

which you then encoded as

    meshJson =
        Scene3d.Mesh.Encode.mesh facetsMesh

you could then decode it with something like

    decodeResult =
        Json.Decode.decodeValue
            Scene3d.Mesh.Decode.facets
            meshJson

where `decodeResult` should be `Ok decodedMesh`, with `decodedMesh` being
equal to the original `facetsMesh` (within numerical roundoff).

Each function documents what JSON format it expects, so if you have some
existing 3D models then it should be possible to write a small script (in
whatever language you prefer) that converts those models into JSON that
`elm-3d-scene` can decode. For example, you might write a command-line tool
using [Assimp](https://www.assimp.org/) that reads in data from one of [Assimp's
supported file formats](https://github.com/assimp/assimp) and writes out JSON,
or write a [Blender script](https://docs.blender.org/manual/en/latest/advanced/scripting/introduction.html)
that exports the currently selected Blender object as JSON.

Hopefully at some point there will be good pure-Elm ways to load lots of
different 3D model formats directly, but in the meantime this JSON-based
approach should give you a path to using existing 3D assets without _too_ much
difficulty.

@docs points, lineSegments, polyline, triangles, facets


## Indexed meshes

The following functions all decode indexed meshes of the form

    { "vertices" :
        [ <vertex>
        , <vertex>
        , <vertex>
        ],
    , "faces":
        [ [ i, j, k ]
        , [ l, m, n ]
        , ...
        ]
    }

where the type of entries in the `"vertices"` list depends on the type of mesh
being decoded, but the entries in the `"faces"` list are always lists of three
zero-based indices into the list of vertices (the format used by
[`TriangularMesh.indexed`](https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest/TriangularMesh#indexed)).

@docs indexedTriangles, indexedFacets, indexedFaces

@docs texturedTriangles, texturedFacets, texturedFaces


## Shadows

@docs shadow

-}

import Array exposing (Array)
import BoundingBox3d exposing (BoundingBox3d)
import Json.Decode as Decode exposing (Decoder, Value)
import Length exposing (Length, Meters)
import LineSegment3d exposing (LineSegment3d)
import Math.Vector3
import Pixels exposing (Pixels)
import Point3d exposing (Point3d)
import Polyline3d exposing (Polyline3d)
import Quantity exposing (Quantity, Unitless)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Scene3d.Types as Types
import Triangle3d exposing (Triangle3d)
import TriangularMesh exposing (TriangularMesh)
import Vector3d exposing (Vector3d)
import WebGL


decodeEmpty : a -> Decoder a
decodeEmpty value =
    Decode.field "empty" (Decode.null value)


decodePoint : Decoder (Point3d Meters coordinates)
decodePoint =
    Decode.map3 Point3d.meters
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "z" Decode.float)


decodeLineSegment : Decoder (LineSegment3d Meters coordinates)
decodeLineSegment =
    Decode.map2 LineSegment3d.from
        (Decode.index 0 decodePoint)
        (Decode.index 1 decodePoint)


decodeTriangle : Decoder (Triangle3d Meters coordinates)
decodeTriangle =
    Decode.map3 Triangle3d.from
        (Decode.index 0 decodePoint)
        (Decode.index 1 decodePoint)
        (Decode.index 2 decodePoint)


decodePolyline : Decoder (Polyline3d Meters coordinates)
decodePolyline =
    Decode.map Polyline3d.fromVertices (Decode.list decodePoint)


decodeMeshFace : Decoder ( Int, Int, Int )
decodeMeshFace =
    Decode.map3 (\i j k -> ( i, j, k ))
        (Decode.index 0 Decode.int)
        (Decode.index 1 Decode.int)
        (Decode.index 2 Decode.int)


decodeTriangularMesh : Decoder vertex -> Decoder (TriangularMesh vertex)
decodeTriangularMesh decodeVertex =
    Decode.map2 TriangularMesh.indexed
        (Decode.field "vertices" (Decode.array decodeVertex))
        (Decode.field "faces" (Decode.list decodeMeshFace))


decodeUniformVertex : Decoder { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates }
decodeUniformVertex =
    Decode.map6
        (\x y z nx ny nz ->
            { position = Point3d.meters x y z
            , normal = Vector3d.unitless nx ny nz
            }
        )
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "z" Decode.float)
        (Decode.field "nx" Decode.float)
        (Decode.field "ny" Decode.float)
        (Decode.field "nz" Decode.float)


decodeUnlitVertex : Decoder { position : Point3d Meters coordinates, uv : ( Float, Float ) }
decodeUnlitVertex =
    Decode.map5
        (\x y z u v ->
            { position = Point3d.meters x y z
            , uv = ( u, v )
            }
        )
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "z" Decode.float)
        (Decode.field "u" Decode.float)
        (Decode.field "v" Decode.float)


decodeTexuredVertex : Decoder { position : Point3d Meters coordinates, normal : Vector3d Unitless coordinates, uv : ( Float, Float ) }
decodeTexuredVertex =
    Decode.map8
        (\x y z nx ny nz u v ->
            { position = Point3d.meters x y z
            , normal = Vector3d.unitless nx ny nz
            , uv = ( u, v )
            }
        )
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "z" Decode.float)
        (Decode.field "nx" Decode.float)
        (Decode.field "ny" Decode.float)
        (Decode.field "nz" Decode.float)
        (Decode.field "u" Decode.float)
        (Decode.field "v" Decode.float)


{-| Decode a mesh of points, passing in the point radius to use. This expects
JSON of the form

    [ { "x" : x1, "y" : y1, "z" : z1 }
    , { "x" : x2, "y" : y2, "z" : z2 }
    , ...
    ]

Corresponds to [`Mesh.points`](Scene3d-Mesh#points).

-}
points : { radius : Quantity Float Pixels } -> Decoder (Mesh.Plain coordinates)
points radius =
    Decode.map (Mesh.points radius) (Decode.list decodePoint)


{-| Decode a mesh of line segments. This expects JSON of the form

    [ [ { "x" : x1, "y" : y1, "z" : z1 }
      , { "x" : x2, "y" : y2, "z" : z2 }
      ]
    , [ { "x" : x3, "y" : y3, "z" : z3 }
      , { "x" : x4, "y" : y4, "z" : z4 }
      ]
    , ...
    ]

Corresponds to [`Mesh.lineSegments`](Scene3d-Mesh#lineSegments).

-}
lineSegments : Decoder (Mesh.Plain coordinates)
lineSegments =
    Decode.map Mesh.lineSegments (Decode.list decodeLineSegment)


{-| Decode a polyline mesh. This expects JSON of the form

    [ { "x" : x1, "y" : y1, "z" : z1 }
    , { "x" : x2, "y" : y2, "z" : z2 }
    , ...
    ]

Corresponds to [`Mesh.polyline`](Scene3d-Mesh#polyline).

-}
polyline : Decoder (Mesh.Plain coordinates)
polyline =
    Decode.map Mesh.polyline (Decode.map Polyline3d.fromVertices (Decode.list decodePoint))


{-| Decode a mesh of triangles. This expects JSON of the form

    [ [ { "x" : x1, "y" : y1, "z" : z1 }
      , { "x" : x2, "y" : y2, "z" : z2 }
      , { "x" : x3, "y" : y3, "z" : z3 }
      ]
    , [ { "x" : x4, "y" : y4, "z" : z4 }
      , { "x" : x5, "y" : y5, "z" : z5 }
      , { "x" : x6, "y" : y6, "z" : z6 }
      ]
    , ...
    ]

Corresponds to [`Mesh.triangles`](Scene3d-Mesh#triangles).

-}
triangles : Decoder (Mesh.Plain coordinates)
triangles =
    Decode.map Mesh.triangles (Decode.list decodeTriangle)


{-| Decode a mesh of facets. This expects JSON of the form

    [ [ { "x" : x1, "y" : y1, "z" : z1 }
      , { "x" : x2, "y" : y2, "z" : z2 }
      , { "x" : x3, "y" : y3, "z" : z3 }
      ]
    , [ { "x" : x4, "y" : y4, "z" : z4 }
      , { "x" : x5, "y" : y5, "z" : z5 }
      , { "x" : x6, "y" : y6, "z" : z6 }
      ]
    , ...
    ]

Corresponds to [`Mesh.facets`](Scene3d-Mesh#facets).

-}
facets : Decoder (Mesh.Uniform coordinates)
facets =
    Decode.map Mesh.facets (Decode.list decodeTriangle)


{-| Decode an indexed mesh of triangles where each vertex is of the form

    { "x" : x, "y" : y, "z" : z }

Corresponds to [`Mesh.indexedTriangles`](Scene3d-Mesh#indexedTriangles).

-}
indexedTriangles : Decoder (Mesh.Plain coordinates)
indexedTriangles =
    Decode.map Mesh.indexedTriangles (decodeTriangularMesh decodePoint)


{-| Decode an indexed mesh of facets where each vertex is of the form

    { "x" : x, "y" : y, "z" : z }

Corresponds to [`Mesh.indexedFacets`](Scene3d-Mesh#indexedFacets).

-}
indexedFacets : Decoder (Mesh.Uniform coordinates)
indexedFacets =
    Decode.map Mesh.indexedFacets (decodeTriangularMesh decodePoint)


{-| Decode an indexed mesh of faces where each vertex is of the form

    { "x" : x
    , "y" : y
    , "z" : z
    , "nx" : nx
    , "ny" : ny
    , "nz" : nz
    }

Corresponds to [`Mesh.indexedFaces`](Scene3d-Mesh#indexedFaces).

-}
indexedFaces : Decoder (Mesh.Uniform coordinates)
indexedFaces =
    Decode.map Mesh.indexedFaces (decodeTriangularMesh decodeUniformVertex)


{-| Decode an indexed mesh of textured triangles where each vertex is of the
form

    { "x" : x
    , "y" : y
    , "z" : z
    , "u" : u
    , "v" : v
    }

Corresponds to [`Mesh.texturedTriangles`](Scene3d-Mesh#texturedTriangles).

-}
texturedTriangles : Decoder (Mesh.Unlit coordinates)
texturedTriangles =
    Decode.map Mesh.texturedTriangles (decodeTriangularMesh decodeUnlitVertex)


{-| Decode an indexed mesh of textured facets where each vertex is of the
form

    { "x" : x
    , "y" : y
    , "z" : z
    , "u" : u
    , "v" : v
    }

Corresponds to [`Mesh.texturedFacets`](Scene3d-Mesh#texturedFacets).

-}
texturedFacets : Decoder (Mesh.Textured coordinates)
texturedFacets =
    Decode.map Mesh.texturedFacets (decodeTriangularMesh decodeUnlitVertex)


{-| Decode an indexed mesh of textured faces where each vertex is of the
form

    { "x" : x
    , "y" : y
    , "z" : z
    , "nx" : nx
    , "ny" : ny
    , "nz" : nz
    , "u" : u
    , "v" : v
    }

Corresponds to [`Mesh.texturedFaces`](Scene3d-Mesh#texturedFaces).

-}
texturedFaces : Decoder (Mesh.Textured coordinates)
texturedFaces =
    Decode.map Mesh.texturedFaces (decodeTriangularMesh decodeTexuredVertex)


decodeShadowVertex : Decoder Types.VertexWithNormal
decodeShadowVertex =
    Decode.map6
        (\x y z nx ny nz ->
            { position = Math.Vector3.fromRecord { x = x, y = y, z = z }
            , normal = Math.Vector3.fromRecord { x = nx, y = ny, z = nz }
            }
        )
        (Decode.field "x" Decode.float)
        (Decode.field "y" Decode.float)
        (Decode.field "z" Decode.float)
        (Decode.field "nx" Decode.float)
        (Decode.field "ny" Decode.float)
        (Decode.field "nz" Decode.float)


shadowVertexPosition : Types.VertexWithNormal -> Point3d Meters coordinates
shadowVertexPosition { position } =
    Point3d.fromMeters (Math.Vector3.toRecord position)


{-| Decode a [`Shadow`](Scene3d-Mesh#Shadow) value. This expects a mesh in the
same form as [`indexedFaces`](#indexedFaces), but the mesh itself has to be
constructed in a very special way that generates a large number of auxiliary
zero-area faces. In general you should only use this to decode a value produced
by [`Encode.shadow`](Scene3d-Mesh-Encode#shadow).

If you are interested in generating properly-encoded shadow values yourself,
please reach out to me (**@ianmackenzie**) on the [Elm Slack](http://elmlang.herokuapp.com/).

-}
shadow : Decoder (Mesh.Shadow coordinates)
shadow =
    Decode.map
        (\triangularMesh ->
            let
                vertices =
                    Array.toList (TriangularMesh.vertices triangularMesh)

                faceIndices =
                    TriangularMesh.faceIndices triangularMesh
            in
            case BoundingBox3d.hullOfN shadowVertexPosition vertices of
                Just meshBounds ->
                    Types.Shadow meshBounds triangularMesh <|
                        WebGL.indexedTriangles vertices faceIndices

                Nothing ->
                    Types.EmptyShadow
        )
        (decodeTriangularMesh decodeShadowVertex)
