module Spheres exposing (..)

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as Attributes
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.SceneGraph as SceneGraph
import OpenSolid.SceneGraph.Lighting as Lighting
import OpenSolid.SceneGraph.Material as Material
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d
import Sphere
import WebGL


rgb : ( Float, Float, Float ) -> Color
rgb ( red, green, blue ) =
    Color.rgb (round (255 * red)) (round (255 * green)) (round (255 * blue))


gold : SceneGraph.Material
gold =
    Material.physicallyBased
        { baseColor = rgb ( 1, 0.86, 0.57 )
        , roughness = 0.25
        , metallic = 1
        }


aluminum : SceneGraph.Material
aluminum =
    Material.physicallyBased
        { baseColor = rgb ( 0.96, 0.96, 0.97 )
        , roughness = 0.5
        , metallic = 1
        }


blackPlastic : SceneGraph.Material
blackPlastic =
    Material.physicallyBased
        { baseColor = Color.black
        , roughness = 0.25
        , metallic = 0
        }


whitePlastic : SceneGraph.Material
whitePlastic =
    Material.physicallyBased
        { baseColor = Color.white
        , roughness = 0.25
        , metallic = 0
        }


direction phi theta =
    Direction3d
        ( -(sin phi * cos theta)
        , -(sin phi * sin theta)
        , -(cos phi)
        )


lightDirection =
    direction (degrees 30) (degrees 90)


lightColor =
    rgb ( 0.5, 0.5, 0.5 )


lighting =
    Lighting.single (Lighting.directional lightColor lightDirection)


goldSphere =
    Sphere.sphere (Point3d ( 2, 2, 0 )) 1
        |> SceneGraph.shaded gold lighting


aluminumSphere =
    Sphere.sphere (Point3d ( 2, -2, 0 )) 1
        |> SceneGraph.shaded aluminum lighting


blackPlasticSphere =
    Sphere.sphere (Point3d ( -2, -2, 0 )) 1
        |> SceneGraph.shaded blackPlastic lighting


whitePlasticSphere =
    Sphere.sphere (Point3d ( -2, 2, 0 )) 1
        |> SceneGraph.shaded whitePlastic lighting


scene =
    SceneGraph.group
        [ goldSphere
        , aluminumSphere
        , blackPlasticSphere
        , whitePlasticSphere
        ]


cameraFrame =
    Frame3d.lookAt
        { focalPoint = Point3d.origin
        , eyePoint = Point3d ( 10, 10, 10 )
        , upDirection = Direction3d.positiveZ
        }


width =
    1024


height =
    768


camera =
    Camera.perspective
        { frame = cameraFrame
        , screenWidth = width
        , screenHeight = height
        , verticalFov = degrees 30
        , zNear = 0.1
        , zFar = 100
        }


main : Html Never
main =
    WebGL.toHtml
        [ Attributes.width width, Attributes.height height ]
        (SceneGraph.toEntities camera scene)
