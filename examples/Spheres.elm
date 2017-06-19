module Spheres exposing (..)

import Color exposing (Color)
import Html exposing (Html)
import Html.Attributes as Attributes
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Point3d as Point3d
import OpenSolid.Scene as Scene
import OpenSolid.Scene.Light as Light
import OpenSolid.Scene.Lighting as Lighting
import OpenSolid.Scene.Material as Material
import OpenSolid.WebGL.Camera as Camera
import OpenSolid.WebGL.Frame3d as Frame3d
import Sphere
import WebGL


gold : Scene.Material
gold =
    Material.physicallyBased
        { baseColor = Color.rgb 255 219 145
        , roughness = 0.4
        , metallic = True
        }


aluminum : Scene.Material
aluminum =
    Material.physicallyBased
        { baseColor = Color.rgb 245 245 247
        , roughness = 0.6
        , metallic = True
        }


blackPlastic : Scene.Material
blackPlastic =
    Material.physicallyBased
        { baseColor = Color.black
        , roughness = 0.25
        , metallic = False
        }


whitePlastic : Scene.Material
whitePlastic =
    Material.physicallyBased
        { baseColor = Color.white
        , roughness = 0.25
        , metallic = False
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
    Color.rgb 127 127 127


lighting =
    Lighting.singleLight (Light.directional lightColor lightDirection)


goldSphere =
    Sphere.sphere (Point3d ( 2, 2, 0 )) 1
        |> Scene.shaded gold lighting


aluminumSphere =
    Sphere.sphere (Point3d ( 2, -2, 0 )) 1
        |> Scene.shaded aluminum lighting


blackPlasticSphere =
    Sphere.sphere (Point3d ( -2, -2, 0 )) 1
        |> Scene.shaded blackPlastic lighting


whitePlasticSphere =
    Sphere.sphere (Point3d ( -2, 2, 0 )) 1
        |> Scene.shaded whitePlastic lighting


scene =
    Scene.group
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
        (Scene.toEntities camera scene)
