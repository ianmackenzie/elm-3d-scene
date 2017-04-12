module OpenSolid.SceneGraph
    exposing
        ( Mesh
        , Lines
        , Points
        , Node
        , triangles
        , mesh
        , lines
        , polyline
        , points
        , scaleAbout
        , rotateAround
        , translateBy
        , mirrorAcross
        , placeIn
        , Projection
        , perspectiveProjection
        , orthographicProjection
        , Lighting
        , PointLight
        , ambientLighting
        , directionalLighting
        , singlePointLighting
        , twoPointLighting
        , threePointLighting
        , fourPointLighting
        )

import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Triangle3d as Triangle3d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Polyline3d as Polyline3d
import OpenSolid.WebGL.Direction3d as Direction3d
import OpenSolid.WebGL.Point3d as Point3d
import OpenSolid.WebGL.Color as Color
import Math.Vector3 as Vec3 exposing (Vec3)
import Math.Vector4 as Vec4 exposing (Vec4)
import Math.Matrix4 as Mat4 exposing (Mat4)
import WebGL
import Color exposing (Color)


type alias VertexPositionAndNormal =
    { vertexPosition : Vec3
    , vertexNormal : Vec3
    }


type alias VertexPosition =
    { vertexPosition : Vec3 }


type alias Uniforms a =
    { a
        | modelMatrix : Mat4
        , modelViewMatrix : Mat4
        , modelViewProjectionMatrix : Mat4
    }


type alias MeshUniforms a =
    Uniforms { a | normalMatrix : Mat4 }


type alias PrimitiveUniforms =
    Uniforms { color : Vec4 }


type alias PositionAndNormal =
    { position : Vec3
    , normal : Vec3
    }


type alias MeshVertexShader a =
    WebGL.Shader VertexPositionAndNormal (MeshUniforms a) PositionAndNormal


type alias MeshFragmentShader a =
    WebGL.Shader {} (Uniforms a) PositionAndNormal


type alias PrimitiveVertexShader =
    WebGL.Shader VertexPosition PrimitiveUniforms {}


type alias PrimitiveFragmentShader =
    WebGL.Shader {} PrimitiveUniforms {}


type Mesh
    = Mesh (WebGL.Mesh VertexPositionAndNormal)


type Lines
    = Lines (WebGL.Mesh VertexPosition)


type Points
    = Points (WebGL.Mesh VertexPosition)


type MeshShaders
    = MeshShaders


type alias PrimitiveShaders =
    ( PrimitiveVertexShader, PrimitiveFragmentShader )


type Entity
    = MeshEntity Mesh MeshShaders
    | LinesEntity Lines PrimitiveShaders
    | PointsEntity Points PrimitiveShaders


type Transformation
    = Transformation Frame3d Float


type Node
    = Node (List ( Entity, Transformation ))


type Projection
    = Projection Mat4


type Lighting
    = AmbientLighting Color
    | DirectionalLighting Direction3d Color
    | SinglePointLighting PointLight
    | TwoPointLighting PointLight PointLight
    | ThreePointLighting PointLight PointLight PointLight
    | FourPointLighting PointLight PointLight PointLight PointLight


type alias PointLight =
    ( Point3d, Color )


zeroVec3 : Vec3
zeroVec3 =
    Vec3.vec3 0 0 0


vertexPositionsAndNormals : Triangle3d -> ( VertexPositionAndNormal, VertexPositionAndNormal, VertexPositionAndNormal )
vertexPositionsAndNormals triangle =
    let
        normalVector =
            case Triangle3d.normalDirection triangle of
                Just direction ->
                    Direction3d.toVec3 direction

                Nothing ->
                    zeroVec3

        ( p1, p2, p3 ) =
            Triangle3d.vertices triangle
    in
        ( { vertexPosition = Point3d.toVec3 p1, vertexNormal = normalVector }
        , { vertexPosition = Point3d.toVec3 p2, vertexNormal = normalVector }
        , { vertexPosition = Point3d.toVec3 p3, vertexNormal = normalVector }
        )


triangles : List Triangle3d -> Mesh
triangles triangles_ =
    Mesh (WebGL.triangles (List.map vertexPositionsAndNormals triangles_))


vertexPositionAndNormal : ( Point3d, Direction3d ) -> VertexPositionAndNormal
vertexPositionAndNormal ( point, normalDirection ) =
    { vertexPosition = Point3d.toVec3 point
    , vertexNormal = Direction3d.toVec3 normalDirection
    }


mesh : List ( Point3d, Direction3d ) -> List ( Int, Int, Int ) -> Mesh
mesh vertices faces =
    let
        vertexPositionsAndNormals =
            List.map vertexPositionAndNormal vertices
    in
        Mesh (WebGL.indexedTriangles vertexPositionsAndNormals faces)


vertexPositions : LineSegment3d -> ( VertexPosition, VertexPosition )
vertexPositions lineSegment =
    let
        ( p1, p2 ) =
            LineSegment3d.endpoints lineSegment
    in
        ( { vertexPosition = Point3d.toVec3 p1 }
        , { vertexPosition = Point3d.toVec3 p2 }
        )


lines : List LineSegment3d -> Lines
lines lineSegments =
    Lines (WebGL.lines (List.map vertexPositions lineSegments))


vertexPosition : Point3d -> VertexPosition
vertexPosition point =
    { vertexPosition = Point3d.toVec3 point }


polyline : Polyline3d -> Lines
polyline polyline_ =
    let
        vertices =
            Polyline3d.vertices polyline_
    in
        Lines (WebGL.lineStrip (List.map vertexPosition vertices))


points : List Point3d -> Points
points points_ =
    Points (WebGL.points (List.map vertexPosition points_))


group : List Node -> Node
group nodes =
    Debug.crash "TODO"


scaleAbout : Point3d -> Float -> Node -> Node
scaleAbout point scale node =
    Debug.crash "TODO"


rotateAround : Axis3d -> Float -> Node -> Node
rotateAround axis angle node =
    Debug.crash "TODO"


translateBy : Vector3d -> Node -> Node
translateBy displacement node =
    Debug.crash "TODO"


mirrorAcross : Plane3d -> Node -> Node
mirrorAcross plane node =
    Debug.crash "TODO"


placeIn : Frame3d -> Node -> Node
placeIn frame node =
    Debug.crash "TODO"


perspectiveProjection : { verticalFov : Float, aspectRatio : Float, zNear : Float, zFar : Float } -> Projection
perspectiveProjection { verticalFov, aspectRatio, zNear, zFar } =
    Debug.crash "TODO"


orthographicProjection : { height : Float, aspectRatio : Float, zNear : Float, zFar : Float } -> Projection
orthographicProjection { height, aspectRatio, zNear, zFar } =
    Debug.crash "TODO"


ambientLighting : Color -> Lighting
ambientLighting =
    AmbientLighting


directionalLighting : Direction3d -> Color -> Lighting
directionalLighting =
    DirectionalLighting


singlePointLighting : PointLight -> Lighting
singlePointLighting =
    SinglePointLighting


twoPointLighting : PointLight -> PointLight -> Lighting
twoPointLighting =
    TwoPointLighting


threePointLighting : PointLight -> PointLight -> PointLight -> Lighting
threePointLighting =
    ThreePointLighting


fourPointLighting : PointLight -> PointLight -> PointLight -> PointLight -> Lighting
fourPointLighting =
    FourPointLighting


meshVertexShader : MeshVertexShader a
meshVertexShader =
    [glsl|
        attribute vec3 vertexPosition;
        attribute vec3 vertexNormal;

        uniform mat4 modelMatrix;
        uniform mat4 modelViewMatrix;
        uniform mat4 modelViewProjectionMatrix;
        uniform mat4 normalMatrix;

        varying vec3 position;
        varying vec3 normal;

        void main () {
          gl_Position = modelViewProjectionMatrix * vec4(vertexPosition, 1.0);
          position = (modelMatrix * vec4(vertexPosition, 1.0)).xyz;
          normal = (normalMatrix * vec4(vertexNormal, 0.0)).xyz;
        }
    |]


primitiveVertexShader : PrimitiveVertexShader
primitiveVertexShader =
    [glsl|
        attribute vec3 vertexPosition;

        uniform mat4 modelViewProjectionMatrix;

        void main () {
          gl_Position = modelViewProjectionMatrix * vec4(vertexPosition, 1.0);
        }
    |]


primitiveFragmentShader : PrimitiveFragmentShader
primitiveFragmentShader =
    [glsl|
        precision mediump float;

        uniform vec4 color;

        void main () {
            gl_FragColor = color;
        }
    |]
