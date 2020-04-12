module Scene3d exposing
    ( toHtml, unlit, sunny, cloudy, office
    , Option
    , multisampling, supersampling, dynamicRange
    , Entity
    , quad, block, sphere, cylinder
    , mesh
    , group, nothing
    , shadow, withShadow
    , rotateAround, translateBy, translateIn, scaleAbout, mirrorAcross
    , placeIn, relativeTo
    , transparentBackground, whiteBackground, blackBackground, backgroundColor, transparentBackgroundColor
    , Light, directionalLight, pointLight, overheadLighting, ambientLighting, softLighting, disabledLight
    , CastsShadows, Yes, No, castsShadows, doesNotCastShadows
    , Lights, noLights, oneLight, twoLights, threeLights, fourLights, fiveLights, sixLights, sevenLights, eightLights
    , Chromaticity
    , chromaticity, daylight, sunlight, blueSky, incandescentLighting, fluorescentLighting, colorTemperature, xyChromaticity
    , Exposure
    , exposureValue, maxLuminance, photographicExposure
    , toWebGLEntities
    )

{-| Top-level functionality for rendering a 3D scene.

Note that the way `elm-3d-scene` is designed, functions in this module are
generally 'cheap' and can safely be used in your `view` function directly. For
example, you can safely have logic in your `view` function that enables and
disables lights, moves objects around by translating/rotating/mirroring them,
or even changes the material used to render a particular object with. In
contrast, creating meshes using the functions in the [`Mesh`](Scene3d-Mesh)
module is 'expensive'; meshes should generally be created once and then stored
in your model.

@docs toHtml, unlit, sunny, cloudy, office


# Options

@docs Option

@docs multisampling, supersampling, dynamicRange


# Entities

@docs Entity


## Basic shapes

`elm-3d-scene` includes a handful of basic shapes which you can draw directly
without having to create and store a separate [`Mesh`](Scene3d-Mesh). In
general, for all of the basic shapes you can specify whether or not it should
cast a shadow (assuming there is shadow-casting light in the scene!) and can
specify a material to use. However, different shapes support different kinds of
materials; `quad`s and `sphere`s support all materials, while `block`s and
`cylinder`s only support uniform (non-textured) materials.

@docs quad, block, sphere, cylinder


## Meshes

@docs mesh


## Grouping and toggling

@docs group, nothing


# Shadows

@docs shadow, withShadow


# Transformations

These transformations are 'cheap' in that they don't actually transform the
underlying mesh; under the hood they use a WebGL [transformation matrix](https://learnopengl.com/Getting-started/Transformations)
to change where that mesh gets rendered.

@docs rotateAround, translateBy, translateIn, scaleAbout, mirrorAcross


# Coordinate conversions

You're unlikely to need these functions right away but they can be very useful
when setting up more complex scenes.

@docs placeIn, relativeTo


# Background

@docs transparentBackground, whiteBackground, blackBackground, backgroundColor, transparentBackgroundColor


# Lighting

Lighting in `elm-3d-scene` is a combination of _direct_ and _environmental_
(indirect, ambient) lighting. Direct lighting comes from specific lights like
the sun or a light bulb; you can currently have up to eight separate lights
in a scene. Environmental lighting accounts for light coming from all around an
object; for example, if you are outside, you might be getting direct light from
the sun but you're also getting light reflected from other surfaces around you.

Generally, direct lighting gives nice highlights but can be very harsh; it's
usually best to use a combination of some soft indirect lighting to ensure the
scene as a whole is reasonably well lit, and then use one or more point or
directional lights to provide highlights.


## Lights

@docs Light, directionalLight, pointLight, overheadLighting, ambientLighting, softLighting, disabledLight

@docs CastsShadows, Yes, No, castsShadows, doesNotCastShadows

@docs Lights, noLights, oneLight, twoLights, threeLights, fourLights, fiveLights, sixLights, sevenLights, eightLights


## Chromaticity

@docs Chromaticity

@docs chromaticity, daylight, sunlight, blueSky, incandescentLighting, fluorescentLighting, colorTemperature, xyChromaticity


## Exposure

@docs Exposure

@docs exposureValue, maxLuminance, photographicExposure


# Advanced

@docs toWebGLEntities

-}

import Angle exposing (Angle)
import Axis3d exposing (Axis3d)
import Block3d exposing (Block3d)
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Color.Transparent
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Frame3d exposing (Frame3d)
import Geometry.Interop.LinearAlgebra.Frame3d as Frame3d
import Geometry.Interop.LinearAlgebra.Point3d as Point3d
import Html exposing (Html)
import Html.Attributes
import Html.Keyed
import Illuminance exposing (Illuminance)
import Length exposing (Length, Meters)
import Luminance exposing (Luminance)
import LuminousFlux exposing (LuminousFlux)
import Math.Matrix4 exposing (Mat4)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Pixels exposing (Pixels, inPixels)
import Plane3d exposing (Plane3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Rectangle2d
import Scene3d.ColorConversions as ColorConversions
import Scene3d.Entity as Entity
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Scene3d.Transformation as Transformation exposing (Transformation)
import Scene3d.Types as Types exposing (Bounds, DrawFunction, LightMatrices, LinearRgb(..), Material(..), Node(..))
import Sphere3d exposing (Sphere3d)
import Temperature exposing (Temperature)
import Vector3d exposing (Vector3d)
import Viewpoint3d exposing (Viewpoint3d)
import WebGL
import WebGL.Matrices as WebGL
import WebGL.Settings
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Settings.StencilTest as StencilTest
import WebGL.Texture exposing (Texture)



----- ENTITIES -----


{-| An `Entity` is a shape or group of shapes in a scene.
-}
type alias Entity coordinates =
    Types.Entity coordinates


{-| A dummy entity for which nothing will be drawn.
-}
nothing : Entity coordinates
nothing =
    Entity.empty


{-| Draw a 'quad' such as a rectangle, rhombus or parallelogram by providing its
four vertices in counterclockwise order.

Normal vectors will be automatically computed at each vertex which are
perpendicular to the two adjoining edges. (The four vertices should usually
be coplanar, in which case all normal vectors will be the same.) The four
vertices will also be given the UV (texture) coordinates (0,0), (1,0), (1,1)
and (0,1) respectively; this means that if you specify vertices counterclockwise
from the bottom left corner of a rectangle, a texture will map onto the
rectangle basically the way you would expect.

-}
quad :
    CastsShadows a
    -> Material.Textured coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Point3d Meters coordinates
    -> Entity coordinates
quad (CastsShadows shadowFlag) givenMaterial p1 p2 p3 p4 =
    Entity.quad shadowFlag givenMaterial p1 p2 p3 p4


{-| Draw a sphere using the [`Sphere3d`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Sphere3d)
type from `elm-geometry`.

The sphere will have texture (UV) coordinates based on an [equirectangular
projection](https://wiki.panotools.org/Equirectangular_Projection) where
positive Z is up. This sounds complex but really just means that U corresponds
to angle around the sphere and V corresponds to angle up the sphere, similar to
the diagrams shown [here](https://en.wikipedia.org/wiki/Spherical_coordinate_system)
except that V is measured up from the bottom (negative Z) instead of down from
the top (positive Z).

Note that this projection, while simple, means that the texture used will get
'squished' near the poles of the sphere.

-}
sphere : CastsShadows a -> Material.Textured coordinates -> Sphere3d Meters coordinates -> Entity coordinates
sphere (CastsShadows shadowFlag) givenMaterial givenSphere =
    Entity.sphere shadowFlag givenMaterial givenSphere


{-| Draw a rectangular block using the [`Block3d`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Block3d)
type from `elm-geometry`.
-}
block : CastsShadows a -> Material.Uniform coordinates -> Block3d Meters coordinates -> Entity coordinates
block (CastsShadows shadowFlag) givenMaterial givenBlock =
    Entity.block shadowFlag givenMaterial givenBlock


{-| Draw a cylinder using the [`Cylinder3d`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Cylinder3d)
type from `elm-geometry`.
-}
cylinder : CastsShadows a -> Material.Uniform coordinates -> Cylinder3d Meters coordinates -> Entity coordinates
cylinder (CastsShadows shadowFlag) givenMaterial givenCylinder =
    Entity.cylinder shadowFlag givenMaterial givenCylinder


{-| Draw the given mesh (shape) with the given material. Check out the [`Mesh`](Scene3d-Mesh)
and [`Material`](Scene3d-Material) modules for how to define meshes and
materials. Note that the mesh and material types must line up, and this is
checked by the compiler; for example, a textured material that requires UV
coordinates can only be used on a mesh that includes UV coordinates!

If you want to also draw the shadow of a given object, you'll need to use
[`shadow`](#shadow) or [`withShadow`](#withShadow).

-}
mesh : Material coordinates attributes -> Mesh coordinates attributes -> Entity coordinates
mesh givenMaterial givenMesh =
    Entity.mesh givenMaterial givenMesh


{-| Group a list of entities into a single entity. This combined entity can then
be transformed, grouped with other entities, etc.
-}
group : List (Entity coordinates) -> Entity coordinates
group entities =
    Entity.group entities


{-| Draw the _shadow_ of an object. Note that this means you can choose to
render an object and its shadow, and object without its shadow, or even render
an object's shadow without the object itself for [maximum creepiness](https://en.wikipedia.org/wiki/Identity_Crisis_(Star_Trek:_The_Next_Generation)).
-}
shadow : Mesh.Shadow coordinates -> Entity coordinates
shadow givenShadow =
    Entity.shadow givenShadow


{-| Convenience function for rendering an object and its shadow;

    Scene3d.withShadow shadow entity

is shorthand for

    Scene3d.group [ entity, Scene3d.shadow shadow ]

-}
withShadow : Mesh.Shadow coordinates -> Entity coordinates -> Entity coordinates
withShadow givenShadow givenEntity =
    group [ givenEntity, shadow givenShadow ]


{-| Rotate an entity around a given axis by a given angle.
-}
rotateAround : Axis3d Meters coordinates -> Angle -> Entity coordinates -> Entity coordinates
rotateAround axis angle entity =
    Entity.rotateAround axis angle entity


{-| Translate (move) an entity by a given displacement vector.
-}
translateBy : Vector3d Meters coordinates -> Entity coordinates -> Entity coordinates
translateBy displacement entity =
    Entity.translateBy displacement entity


{-| Translate an entity in a given direction by a given distance.
-}
translateIn : Direction3d coordinates -> Length -> Entity coordinates -> Entity coordinates
translateIn direction distance entity =
    Entity.translateIn direction distance entity


{-| Scale an entity about a given point by a given scale. The given point will
remain fixed in place and all other points on the entity will be stretched away
from that point (or contract towards that point, if the scale is less than one).

`elm-3d-scene` tries very hard to do the right thing here even if you use a
_negative_ scale factor, but that flips the mesh inside out so I don't really
recommend it.

-}
scaleAbout : Point3d Meters coordinates -> Float -> Entity coordinates -> Entity coordinates
scaleAbout centerPoint scale entity =
    Entity.scaleAbout centerPoint scale entity


{-| Mirror an entity across a plane.
-}
mirrorAcross : Plane3d Meters coordinates -> Entity coordinates -> Entity coordinates
mirrorAcross plane entity =
    Entity.mirrorAcross plane entity


{-| Take an entity that is defined in a local coordinate system and convert it
to global coordinates. This can be useful if you have some entities which are
defined in some local coordinate system like inside a car, and you want to
render them within a larger world.
-}
placeIn : Frame3d Meters coordinates { defines : localCoordinates } -> Entity localCoordinates -> Entity coordinates
placeIn frame entity =
    Entity.placeIn frame entity


{-| Take an entity that is defined in global coordinates and convert into a
local coordinate system. This is even less likely to be useful than `placeIn`,
but may be useful if you are (for example) rendering an office scene (and
working primarily in local room coordinates) but want to incorporate some entity
defined in global coordinates like a bird flying past the window.
-}
relativeTo : Frame3d Meters coordinates { defines : localCoordinates } -> Entity coordinates -> Entity localCoordinates
relativeTo frame entity =
    Entity.relativeTo frame entity



----- LIGHT SOURCES -----
--
-- type:
--   0 : disabled
--   1 : directional (XYZ is direction to light, i.e. reversed light direction)
--   2 : point (XYZ is light position)
--   3 : soft lighting (XYZ is up direction, parameter is ratio of below luminance to above)
--
-- [ x_i     r_i          x_j     r_j         ]
-- [ y_i     g_i          y_j     g_j         ]
-- [ z_i     b_i          z_j     b_j         ]
-- [ type_i  parameter_i  type_j  parameter_j ]


{-| A `Light` represents a single source of light in the scene, such as the sun
or a light bulb. Lights are not rendered themselves; they can only be seen by
how they interact with objects in the scene.
-}
type alias Light coordinates castsShadows =
    Types.Light coordinates castsShadows


{-| A `Lights` value represents the set of all lights in a scene. There are a
couple of current limitations to note in `elm-3d-scene`:

  - No more than eight lights can exist in the scene
  - Only _one_ of those lights can cast shadows

(The reason there is a separate `Lights` type, instead of just using a list of
`Light` values, is so that the type system can be used to guarantee these
constraints are satisfied.)

-}
type Lights coordinates
    = SingleUnshadowedPass LightMatrices
    | SingleShadowedPass LightMatrices
    | TwoPasses LightMatrices LightMatrices


disabledLight : Light coordinates castsShadows
disabledLight =
    Types.Light
        { type_ = 0
        , castsShadows = False
        , x = 0
        , y = 0
        , z = 0
        , r = 0
        , g = 0
        , b = 0
        , parameter = 0
        }


lightPair : Light coordinates firstCastsShadows -> Light coordinates secondCastsShadows -> Mat4
lightPair (Types.Light first) (Types.Light second) =
    Math.Matrix4.fromRecord
        { m11 = first.x
        , m21 = first.y
        , m31 = first.z
        , m41 = first.type_
        , m12 = first.r
        , m22 = first.g
        , m32 = first.b
        , m42 = first.parameter
        , m13 = second.x
        , m23 = second.y
        , m33 = second.z
        , m43 = second.type_
        , m14 = second.r
        , m24 = second.g
        , m34 = second.b
        , m44 = second.parameter
        }


lightingDisabled : LightMatrices
lightingDisabled =
    { lights12 = lightPair disabledLight disabledLight
    , lights34 = lightPair disabledLight disabledLight
    , lights56 = lightPair disabledLight disabledLight
    , lights78 = lightPair disabledLight disabledLight
    }


{-| No lights at all! You don't need lights if you're only using materials
like [`color`](Material#color) or [`emissive`](Material#emissive) (since those
materials don't react to light anyways).
-}
noLights : Lights coordinates
noLights =
    SingleUnshadowedPass lightingDisabled


lightCastsShadows : Light coordinates castsShadows -> Bool
lightCastsShadows (Types.Light properties) =
    properties.castsShadows


oneLight : Light coordinates (CastsShadows a) -> Lights coordinates
oneLight light =
    let
        lightMatrices =
            { lights12 = lightPair light disabledLight
            , lights34 = lightPair disabledLight disabledLight
            , lights56 = lightPair disabledLight disabledLight
            , lights78 = lightPair disabledLight disabledLight
            }
    in
    if lightCastsShadows light then
        SingleShadowedPass lightMatrices

    else
        SingleUnshadowedPass lightMatrices


twoLights :
    Light coordinates (CastsShadows a)
    -> Light coordinates (CastsShadows No)
    -> Lights coordinates
twoLights first second =
    eightLights
        first
        second
        disabledLight
        disabledLight
        disabledLight
        disabledLight
        disabledLight
        disabledLight


threeLights :
    Light coordinates (CastsShadows a)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Lights coordinates
threeLights first second third =
    eightLights
        first
        second
        third
        disabledLight
        disabledLight
        disabledLight
        disabledLight
        disabledLight


fourLights :
    Light coordinates (CastsShadows a)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Lights coordinates
fourLights first second third fourth =
    eightLights
        first
        second
        third
        fourth
        disabledLight
        disabledLight
        disabledLight
        disabledLight


fiveLights :
    Light coordinates (CastsShadows a)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Lights coordinates
fiveLights first second third fourth fifth =
    eightLights
        first
        second
        third
        fourth
        fifth
        disabledLight
        disabledLight
        disabledLight


sixLights :
    Light coordinates (CastsShadows a)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Lights coordinates
sixLights first second third fourth fifth sixth =
    eightLights
        first
        second
        third
        fourth
        fifth
        sixth
        disabledLight
        disabledLight


sevenLights :
    Light coordinates (CastsShadows a)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Lights coordinates
sevenLights first second third fourth fifth sixth seventh =
    eightLights
        first
        second
        third
        fourth
        fifth
        sixth
        seventh
        disabledLight


eightLights :
    Light coordinates (CastsShadows a)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Light coordinates (CastsShadows No)
    -> Lights coordinates
eightLights first second third fourth fifth sixth seventh eigth =
    if lightCastsShadows first then
        TwoPasses
            { lights12 = lightPair first second
            , lights34 = lightPair third fourth
            , lights56 = lightPair fifth sixth
            , lights78 = lightPair seventh eigth
            }
            { lights12 = lightPair second third
            , lights34 = lightPair fourth fifth
            , lights56 = lightPair sixth seventh
            , lights78 = lightPair eigth disabledLight
            }

    else
        SingleUnshadowedPass
            { lights12 = lightPair first second
            , lights34 = lightPair third fourth
            , lights56 = lightPair fifth sixth
            , lights78 = lightPair seventh eigth
            }


type CastsShadows a
    = CastsShadows Bool


type Yes
    = Yes


type No
    = No


castsShadows : CastsShadows Yes
castsShadows =
    CastsShadows True


doesNotCastShadows : CastsShadows No
doesNotCastShadows =
    CastsShadows False


directionalLight :
    CastsShadows a
    ->
        { chromaticity : Chromaticity
        , intensity : Illuminance
        , direction : Direction3d coordinates
        }
    -> Light coordinates (CastsShadows a)
directionalLight (CastsShadows shadowFlag) light =
    let
        { x, y, z } =
            Direction3d.unwrap light.direction

        (LinearRgb rgb) =
            ColorConversions.chromaticityToLinearRgb light.chromaticity

        lux =
            Illuminance.inLux light.intensity
    in
    Types.Light
        { type_ = 1
        , castsShadows = shadowFlag
        , x = -x
        , y = -y
        , z = -z
        , r = lux * Math.Vector3.getX rgb
        , g = lux * Math.Vector3.getY rgb
        , b = lux * Math.Vector3.getZ rgb
        , parameter = 0
        }


pointLight :
    CastsShadows a
    ->
        { chromaticity : Chromaticity
        , intensity : LuminousFlux
        , position : Point3d Meters coordinates
        }
    -> Light coordinates (CastsShadows a)
pointLight (CastsShadows shadowFlag) light =
    let
        (LinearRgb rgb) =
            ColorConversions.chromaticityToLinearRgb light.chromaticity

        lumens =
            LuminousFlux.inLumens light.intensity

        { x, y, z } =
            Point3d.unwrap light.position
    in
    Types.Light
        { type_ = 2
        , castsShadows = shadowFlag
        , x = x
        , y = y
        , z = z
        , r = lumens * Math.Vector3.getX rgb
        , g = lumens * Math.Vector3.getY rgb
        , b = lumens * Math.Vector3.getZ rgb
        , parameter = 0
        }


softLighting :
    { upDirection : Direction3d coordinates
    , chromaticity : Chromaticity
    , intensityAbove : Illuminance
    , intensityBelow : Illuminance
    }
    -> Light coordinates (CastsShadows No)
softLighting light =
    if light.intensityAbove == Quantity.zero && light.intensityBelow == Quantity.zero then
        disabledLight

    else if
        Quantity.abs light.intensityBelow
            |> Quantity.greaterThan (Quantity.abs light.intensityAbove)
    then
        softLighting
            { upDirection = Direction3d.reverse light.upDirection
            , chromaticity = light.chromaticity
            , intensityAbove = light.intensityBelow
            , intensityBelow = light.intensityAbove
            }

    else
        let
            (LinearRgb rgb) =
                ColorConversions.chromaticityToLinearRgb light.chromaticity

            nitsAbove =
                abs (Illuminance.inLux light.intensityAbove / pi)

            nitsBelow =
                abs (Illuminance.inLux light.intensityBelow / pi)

            { x, y, z } =
                Direction3d.unwrap light.upDirection
        in
        Types.Light
            { type_ = 3
            , castsShadows = False
            , x = x
            , y = y
            , z = z
            , r = nitsAbove * Math.Vector3.getX rgb
            , g = nitsAbove * Math.Vector3.getY rgb
            , b = nitsAbove * Math.Vector3.getZ rgb
            , parameter = nitsBelow / nitsAbove
            }


overheadLighting :
    { upDirection : Direction3d coordinates
    , chromaticity : Chromaticity
    , intensity : Illuminance
    }
    -> Light coordinates (CastsShadows No)
overheadLighting arguments =
    softLighting
        { upDirection = arguments.upDirection
        , chromaticity = arguments.chromaticity
        , intensityAbove = arguments.intensity
        , intensityBelow = Quantity.zero
        }


ambientLighting :
    { chromaticity : Chromaticity
    , intensity : Illuminance
    }
    -> Light coordinates (CastsShadows No)
ambientLighting arguments =
    softLighting
        { upDirection = Direction3d.z
        , chromaticity = arguments.chromaticity
        , intensityAbove = arguments.intensity
        , intensityBelow = arguments.intensity
        }



----- BACKGROUND -----


type Background coordinates
    = BackgroundColor Color.Transparent.Color


transparentBackground : Background coordinates
transparentBackground =
    BackgroundColor <|
        Color.Transparent.fromRGBA
            { red = 0
            , green = 0
            , blue = 0
            , alpha = Color.Transparent.transparent
            }


blackBackground : Background coordinates
blackBackground =
    BackgroundColor <|
        Color.Transparent.fromRGBA
            { red = 0
            , green = 0
            , blue = 0
            , alpha = Color.Transparent.opaque
            }


whiteBackground : Background coordinates
whiteBackground =
    BackgroundColor <|
        Color.Transparent.fromRGBA
            { red = 255
            , green = 255
            , blue = 255
            , alpha = Color.Transparent.opaque
            }


backgroundColor : Color -> Background coordinates
backgroundColor color =
    BackgroundColor (Color.Transparent.fromColor Color.Transparent.opaque color)


transparentBackgroundColor : Color.Transparent.Color -> Background coordinates
transparentBackgroundColor transparentColor =
    BackgroundColor transparentColor



----- RENDERING -----


type alias RenderPass =
    LightMatrices -> List WebGL.Settings.Setting -> WebGL.Entity


type alias RenderPasses =
    { meshes : List RenderPass
    , shadows : List RenderPass
    , points : List RenderPass
    }


createRenderPass : Mat4 -> Mat4 -> Mat4 -> Transformation -> DrawFunction -> RenderPass
createRenderPass sceneProperties viewMatrix projectionMatrix transformation drawFunction =
    let
        normalSign =
            if transformation.isRightHanded then
                1

            else
                -1

        modelScale =
            Math.Vector4.vec4
                transformation.scaleX
                transformation.scaleY
                transformation.scaleZ
                normalSign
    in
    drawFunction
        sceneProperties
        modelScale
        (Transformation.modelMatrix transformation)
        transformation.isRightHanded
        viewMatrix
        projectionMatrix


collectRenderPasses : Mat4 -> Mat4 -> Mat4 -> Transformation -> Node -> RenderPasses -> RenderPasses
collectRenderPasses sceneProperties viewMatrix projectionMatrix currentTransformation node accumulated =
    case node of
        EmptyNode ->
            accumulated

        Transformed transformation childNode ->
            collectRenderPasses
                sceneProperties
                viewMatrix
                projectionMatrix
                (Transformation.compose transformation currentTransformation)
                childNode
                accumulated

        MeshNode _ meshDrawFunction ->
            let
                updatedMeshes =
                    createRenderPass
                        sceneProperties
                        viewMatrix
                        projectionMatrix
                        currentTransformation
                        meshDrawFunction
                        :: accumulated.meshes
            in
            { meshes = updatedMeshes
            , shadows = accumulated.shadows
            , points = accumulated.points
            }

        PointNode _ pointDrawFunction ->
            let
                updatedPoints =
                    createRenderPass
                        sceneProperties
                        viewMatrix
                        projectionMatrix
                        currentTransformation
                        pointDrawFunction
                        :: accumulated.points
            in
            { meshes = accumulated.meshes
            , shadows = accumulated.shadows
            , points = updatedPoints
            }

        ShadowNode shadowDrawFunction ->
            let
                updatedShadows =
                    createRenderPass
                        sceneProperties
                        viewMatrix
                        projectionMatrix
                        currentTransformation
                        shadowDrawFunction
                        :: accumulated.shadows
            in
            { meshes = accumulated.meshes
            , shadows = updatedShadows
            , points = accumulated.points
            }

        Group childNodes ->
            List.foldl
                (collectRenderPasses
                    sceneProperties
                    viewMatrix
                    projectionMatrix
                    currentTransformation
                )
                accumulated
                childNodes



-- ## Overall scene Properties
--
-- projectionType:
--   0: perspective (camera XYZ is eye position)
--   1: orthographic (camera XYZ is direction to screen)
--
-- [ clipDistance  cameraX         whiteR        supersampling ]
-- [ aspectRatio   cameraY         whiteG        *             ]
-- [ kc            cameraZ         whiteB        *             ]
-- [ kz            projectionType  dynamicRange  *             ]


defaultBlend : WebGL.Settings.Setting
defaultBlend =
    Blend.custom
        { r = 0
        , g = 0
        , b = 0
        , a = 0
        , color = Blend.customAdd Blend.srcAlpha Blend.oneMinusSrcAlpha
        , alpha = Blend.customAdd Blend.one Blend.oneMinusSrcAlpha
        }


depthTestDefault : List WebGL.Settings.Setting
depthTestDefault =
    [ DepthTest.default, defaultBlend ]


outsideStencil : List WebGL.Settings.Setting
outsideStencil =
    [ DepthTest.lessOrEqual { write = True, near = 0, far = 1 }
    , StencilTest.test
        { ref = 0
        , mask = 0xFF
        , test = StencilTest.equal
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.keep
        , writeMask = 0x00
        }
    , defaultBlend
    ]


insideStencil : List WebGL.Settings.Setting
insideStencil =
    [ DepthTest.lessOrEqual { write = True, near = 0, far = 1 }
    , StencilTest.test
        { ref = 0
        , mask = 0xFF
        , test = StencilTest.notEqual
        , fail = StencilTest.keep
        , zfail = StencilTest.keep
        , zpass = StencilTest.keep
        , writeMask = 0x00
        }
    , defaultBlend
    ]


createShadowStencil : List WebGL.Settings.Setting
createShadowStencil =
    -- Note that the actual stencil test is added by each individual shadow
    -- entity (in Scene3d.Entity.shadowSettings) since the exact test to perform
    -- depends on whether the current model matrix is right-handed or
    -- left-handed
    [ DepthTest.less { write = False, near = 0, far = 1 }
    , WebGL.Settings.colorMask False False False False
    ]


call : List RenderPass -> LightMatrices -> List WebGL.Settings.Setting -> List WebGL.Entity
call renderPasses lightMatrices settings =
    renderPasses
        |> List.map (\renderPass -> renderPass lightMatrices settings)


getMaxDepth : Bounds -> Axis3d Meters coordinates -> Float -> Float -> Float -> Length
getMaxDepth bounds viewAxis scaleX scaleY scaleZ =
    let
        centerPoint =
            Point3d.fromMeters bounds.centerPoint

        viewDir =
            Direction3d.unwrap (Axis3d.direction viewAxis)

        (Quantity d0) =
            Point3d.signedDistanceAlong viewAxis centerPoint

        dX =
            abs (bounds.halfX * viewDir.x * scaleX)

        dY =
            abs (bounds.halfY * viewDir.y * scaleY)

        dZ =
            abs (bounds.halfZ * viewDir.z * scaleZ)
    in
    Quantity (d0 + dX + dY + dZ)


getFarClipDepth : Axis3d Meters coordinates -> Length -> Float -> Float -> Float -> List Node -> Length
getFarClipDepth viewAxis currentValue scaleX scaleY scaleZ nodes =
    case nodes of
        first :: rest ->
            case first of
                Types.EmptyNode ->
                    getFarClipDepth viewAxis currentValue scaleX scaleY scaleZ rest

                Types.MeshNode bounds _ ->
                    let
                        updatedValue =
                            Quantity.max currentValue
                                (getMaxDepth bounds viewAxis scaleX scaleY scaleZ)
                    in
                    getFarClipDepth viewAxis updatedValue scaleX scaleY scaleZ rest

                Types.ShadowNode _ ->
                    getFarClipDepth viewAxis currentValue scaleX scaleY scaleZ rest

                Types.PointNode bounds _ ->
                    let
                        updatedValue =
                            Quantity.max currentValue
                                (getMaxDepth bounds viewAxis scaleX scaleY scaleZ)
                    in
                    getFarClipDepth viewAxis updatedValue scaleX scaleY scaleZ rest

                Types.Group childNodes ->
                    getFarClipDepth
                        viewAxis
                        (getFarClipDepth viewAxis currentValue scaleX scaleY scaleZ childNodes)
                        scaleX
                        scaleY
                        scaleZ
                        rest

                Types.Transformed transformation childNode ->
                    let
                        localScaleX =
                            scaleX * transformation.scaleX

                        localScaleY =
                            scaleY * transformation.scaleY

                        localScaleZ =
                            scaleZ * transformation.scaleZ

                        localViewAxis =
                            viewAxis
                                |> Axis3d.relativeTo (Transformation.placementFrame transformation)
                    in
                    getFarClipDepth
                        viewAxis
                        (getFarClipDepth
                            localViewAxis
                            currentValue
                            localScaleX
                            localScaleY
                            localScaleZ
                            [ childNode ]
                        )
                        scaleX
                        scaleY
                        scaleZ
                        rest

        [] ->
            currentValue


toWebGLEntities :
    { lights : Lights coordinates
    , camera : Camera3d Meters coordinates
    , clipDepth : Length
    , exposure : Exposure
    , dynamicRange : Float
    , whiteBalance : Chromaticity
    , aspectRatio : Float
    , supersampling : Float
    }
    -> List (Entity coordinates)
    -> List WebGL.Entity
toWebGLEntities arguments drawables =
    let
        viewpoint =
            Camera3d.viewpoint arguments.camera

        (Types.Entity rootNode) =
            Entity.group drawables

        viewAxis =
            Axis3d.through (Viewpoint3d.eyePoint viewpoint) (Viewpoint3d.viewDirection viewpoint)

        nearClipDepth =
            Quantity.abs arguments.clipDepth

        farClipDepth =
            getFarClipDepth viewAxis nearClipDepth 1 1 1 [ rootNode ]
                -- Add 1% extra to make sure objects right at the back don't get
                -- culled (also to make sure the far clip depth is not exactly
                -- equal to the near clip depth)
                |> Quantity.multiplyBy 1.01

        projectionMatrix =
            WebGL.projectionMatrix arguments.camera
                { aspectRatio = arguments.aspectRatio
                , nearClipDepth = nearClipDepth
                , farClipDepth = farClipDepth
                }

        projectionType =
            (Math.Matrix4.toRecord projectionMatrix).m44

        eyePointOrDirectionToCamera =
            if projectionType == 0 then
                Point3d.toMeters (Viewpoint3d.eyePoint viewpoint)

            else
                Viewpoint3d.viewDirection viewpoint
                    |> Direction3d.reverse
                    |> Direction3d.unwrap

        (LinearRgb linearRgb) =
            ColorConversions.chromaticityToLinearRgb arguments.whiteBalance

        (Exposure (Quantity nits)) =
            arguments.exposure

        sceneProperties =
            Math.Matrix4.fromRecord
                { m11 = 0
                , m21 = 0
                , m31 = 0
                , m41 = 0
                , m12 = eyePointOrDirectionToCamera.x
                , m22 = eyePointOrDirectionToCamera.y
                , m32 = eyePointOrDirectionToCamera.z
                , m42 = projectionType
                , m13 = nits * Math.Vector3.getX linearRgb
                , m23 = nits * Math.Vector3.getY linearRgb
                , m33 = nits * Math.Vector3.getZ linearRgb
                , m43 = arguments.dynamicRange
                , m14 = arguments.supersampling
                , m24 = 0
                , m34 = 0
                , m44 = 0
                }

        viewMatrix =
            WebGL.viewMatrix viewpoint

        renderPasses =
            collectRenderPasses
                sceneProperties
                viewMatrix
                projectionMatrix
                Transformation.identity
                rootNode
                { meshes = []
                , shadows = []
                , points = []
                }
    in
    case arguments.lights of
        SingleUnshadowedPass lightMatrices ->
            List.concat
                [ call renderPasses.meshes lightMatrices depthTestDefault
                , call renderPasses.points lightingDisabled depthTestDefault
                ]

        SingleShadowedPass lightMatrices ->
            List.concat
                [ call renderPasses.meshes lightingDisabled depthTestDefault
                , call renderPasses.shadows lightMatrices createShadowStencil
                , call renderPasses.meshes lightMatrices outsideStencil
                , call renderPasses.points lightingDisabled depthTestDefault
                ]

        TwoPasses allLightMatrices unshadowedLightMatrices ->
            List.concat
                [ call renderPasses.meshes allLightMatrices depthTestDefault
                , call renderPasses.shadows allLightMatrices createShadowStencil
                , call renderPasses.meshes unshadowedLightMatrices insideStencil
                , call renderPasses.points lightingDisabled depthTestDefault
                ]


toHtml :
    List Option
    ->
        { lights : Lights coordinates
        , camera : Camera3d Meters coordinates
        , clipDepth : Length
        , exposure : Exposure
        , whiteBalance : Chromaticity
        , dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
        , background : Background coordinates
        }
    -> List (Entity coordinates)
    -> Html msg
toHtml options arguments drawables =
    let
        ( width, height ) =
            arguments.dimensions

        widthInPixels =
            inPixels width

        heightInPixels =
            inPixels height

        (BackgroundColor givenBackgroundColor) =
            arguments.background

        backgroundColorString =
            Color.Transparent.toRGBAString givenBackgroundColor

        optionValues =
            collectOptionValues options

        commonWebGLOptions =
            [ WebGL.depth 1
            , WebGL.stencil 0
            , WebGL.alpha True
            , WebGL.clearColor 0 0 0 0
            ]

        webGLOptions =
            if optionValues.multisampling then
                WebGL.antialias :: commonWebGLOptions

            else
                commonWebGLOptions

        -- Force WebGL context to be recreated if multisampling option changes
        key =
            if optionValues.multisampling then
                "1"

            else
                "0"

        widthCss =
            Html.Attributes.style "width" (String.fromFloat widthInPixels ++ "px")

        heightCss =
            Html.Attributes.style "height" (String.fromFloat heightInPixels ++ "px")
    in
    Html.Keyed.node "div" [ Html.Attributes.style "padding" "0px", widthCss, heightCss ] <|
        [ ( key
          , WebGL.toHtmlWith webGLOptions
                [ Html.Attributes.width (round (widthInPixels * optionValues.supersampling))
                , Html.Attributes.height (round (heightInPixels * optionValues.supersampling))
                , widthCss
                , heightCss
                , Html.Attributes.style "display" "block"
                , Html.Attributes.style "background-color" backgroundColorString
                ]
                (toWebGLEntities
                    { lights = arguments.lights
                    , camera = arguments.camera
                    , clipDepth = arguments.clipDepth
                    , exposure = arguments.exposure
                    , dynamicRange = optionValues.dynamicRange
                    , whiteBalance = arguments.whiteBalance
                    , aspectRatio = Quantity.ratio width height
                    , supersampling = optionValues.supersampling
                    }
                    drawables
                )
          )
        ]


type Option
    = Multisampling Bool
    | Supersampling Float
    | DynamicRange Float


multisampling : Bool -> Option
multisampling =
    Multisampling


supersampling : Float -> Option
supersampling =
    Supersampling


dynamicRange : Float -> Option
dynamicRange =
    DynamicRange


type alias OptionValues =
    { multisampling : Bool
    , supersampling : Float
    , dynamicRange : Float
    }


defaultOptionValues : OptionValues
defaultOptionValues =
    { multisampling = True
    , supersampling = 1
    , dynamicRange = 1
    }


collectOptionValues : List Option -> OptionValues
collectOptionValues options =
    List.foldl setOption defaultOptionValues options


setOption : Option -> OptionValues -> OptionValues
setOption option currentValues =
    case option of
        Multisampling value ->
            { currentValues | multisampling = value }

        Supersampling value ->
            { currentValues | supersampling = value }

        DynamicRange value ->
            { currentValues | dynamicRange = value }



----- CHROMATICITY -----


{-| [Chromaticity](https://en.wikipedia.org/wiki/Chromaticity) is a precise way
of describing color independent of brightness. You can think of it as roughly
hue and saturation without value or lightness.

Chromaticity is used for specifying the color of individual lights as well as
the white balance to use for the overall scene.

-}
type alias Chromaticity =
    Types.Chromaticity


{-| The approximate chromaticity of noon daylight; this is a combination of
direct sunlight and blue sky, so is slightly cooler than pure [sunlight](#sunlight).
As a result, this is a good default choice for white balance, but for _light_
color you'll likely want to use [`sunlight`](#sunlight), [`incandescentLighting`](#incandescentLighting)
or [`fluorescentLighting`](#fluorescentLighting) instead.

This is standardized as [Illuminant D65, "Noon Daylight"](https://en.wikipedia.org/wiki/Standard_illuminant),
and is the 'white' color of a properly-calibrated [sRGB](https://en.wikipedia.org/wiki/SRGB)
monitor.

-}
daylight : Chromaticity
daylight =
    xyChromaticity 0.31271 0.32902


{-| An approximate chromaticity value for direct daytime sunlight. This doesn't
seem to have an offically standardized value, but 'sunlight' film is apparently
calibrated to a color temperature of 5600 K so that is what is used here. (This
falls at low end of ['vertical daylight'](https://en.wikipedia.org/wiki/Color_temperature)
and above 'horizon daylight', so should be a decent representative value.)
-}
sunlight : Chromaticity
sunlight =
    colorTemperature (Temperature.kelvins 5600)


{-| An approximate chromaticity value for clear blue sky (12000 K).
-}
blueSky : Chromaticity
blueSky =
    colorTemperature (Temperature.kelvins 12000)


{-| The chromaticity of typical incandescent/tungsten lighting. This is
standardized as [Illuminant A, "Incandescent/Tungsten"](https://en.wikipedia.org/wiki/Standard_illuminant).
-}
incandescentLighting : Chromaticity
incandescentLighting =
    xyChromaticity 0.44757 0.40745


{-| The chromaticity of typical fluorescent lighting. This is standardized as
[Illuminant F2, "Cool White Fluorescent"](https://en.wikipedia.org/wiki/Standard_illuminant).
-}
fluorescentLighting : Chromaticity
fluorescentLighting =
    xyChromaticity 0.37208 0.37529


{-| Specify chromaticity by its _xy_ coordinates in the [CIE xyY color space](https://en.wikipedia.org/wiki/CIE_1931_color_space#CIE_xy_chromaticity_diagram_and_the_CIE_xyY_color_space).
-}
xyChromaticity : Float -> Float -> Chromaticity
xyChromaticity x y =
    Types.Chromaticity { x = x, y = y }


{-| Specify chromaticity by providing a [color temperature](https://en.wikipedia.org/wiki/Color_temperature).
For example, `Scene3d.daylight` is equivalent to

    Scene3d.colorTemperature (Temperature.kelvins 5600)

See [here](https://en.wikipedia.org/wiki/Standard_illuminant#White_points_of_standard_illuminants)
for the color temperatures of many common sources of light.

-}
colorTemperature : Temperature -> Chromaticity
colorTemperature temperature =
    let
        t =
            clamp 1667 25000 (Temperature.inKelvins temperature)

        x =
            if t <= 4000 then
                (-0.2661239 * 1.0e9 / (t * t * t))
                    - (0.2343589 * 1.0e6 / (t * t))
                    + (0.8776956 * 1.0e3 / t)
                    + 0.17991

            else
                (-3.0258469 * 1.0e9 / (t * t * t))
                    + (2.1070379 * 1.0e6 / (t * t))
                    + (0.2226347 * 1.0e3 / t)
                    + 0.24039

        y =
            if t <= 2222 then
                (-1.1063814 * (x * x * x))
                    - (1.3481102 * (x * x))
                    + (2.18555832 * x)
                    - 0.20219683

            else if t <= 4000 then
                (-0.9549476 * (x * x * x))
                    - (1.37418593 * (x * x))
                    + (2.09137015 * x)
                    - 0.16748867

            else
                (3.081758 * (x * x * x))
                    - (5.8733867 * (x * x))
                    + (3.75112997 * x)
                    - 0.37001483
    in
    xyChromaticity x y


{-| Extract the chromaticity of a given color. Note that this is a lossy
conversion since it throws away any lightness/brightness information. For
example, any greyscale color value will have chromaticity equal to
`Scene3d.daylight` (since that is the standard 'white' chromaticity for the
sRGB color space).
-}
chromaticity : Color -> Chromaticity
chromaticity color =
    let
        (Types.CieXyz bigX bigY bigZ) =
            ColorConversions.colorToCieXyz color

        sum =
            bigX + bigY + bigZ
    in
    xyChromaticity (bigX / sum) (bigY / sum)



----- EXPOSURE -----


type Exposure
    = Exposure Luminance


exposureValue : Float -> Exposure
exposureValue ev100 =
    -- from https://media.contentapi.ea.com/content/dam/eacom/frostbite/files/course-notes-moving-frostbite-to-pbr-v2.pdf
    Exposure (Luminance.nits (1.2 * 2 ^ ev100))


maxLuminance : Luminance -> Exposure
maxLuminance givenMaxLuminance =
    Exposure (Quantity.abs givenMaxLuminance)


photographicExposure : { fStop : Float, shutterSpeed : Duration, isoSpeed : Float } -> Exposure
photographicExposure { fStop, shutterSpeed, isoSpeed } =
    let
        t =
            Duration.inSeconds shutterSpeed
    in
    -- from https://media.contentapi.ea.com/content/dam/eacom/frostbite/files/course-notes-moving-frostbite-to-pbr-v2.pdf
    exposureValue (logBase 2 ((100 * fStop ^ 2) / (t * isoSpeed)))



----- PRESETS -----


unlit :
    List Option
    ->
        { dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
        , camera : Camera3d Meters coordinates
        , clipDepth : Length
        , background : Background coordinates
        }
    -> List (Entity coordinates)
    -> Html msg
unlit options arguments entities =
    toHtml options
        { lights = noLights
        , camera = arguments.camera
        , clipDepth = arguments.clipDepth
        , exposure = maxLuminance (Luminance.nits 80) -- sRGB standard monitor brightness
        , whiteBalance = daylight
        , dimensions = arguments.dimensions
        , background = arguments.background
        }
        entities


sunny :
    List Option
    ->
        { upDirection : Direction3d coordinates
        , sunlightDirection : Direction3d coordinates
        , shadows : Bool
        , dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
        , camera : Camera3d Meters coordinates
        , clipDepth : Length
        , background : Background coordinates
        }
    -> List (Entity coordinates)
    -> Html msg
sunny options arguments entities =
    let
        lightProperties =
            { direction = arguments.sunlightDirection
            , intensity = Illuminance.lux 80000
            , chromaticity = sunlight
            }

        sky =
            overheadLighting
                { upDirection = arguments.upDirection
                , chromaticity = blueSky
                , intensity = Illuminance.lux 20000
                }

        environment =
            overheadLighting
                { upDirection = Direction3d.reverse arguments.upDirection
                , chromaticity = daylight
                , intensity = Illuminance.lux 15000
                }

        lights =
            if arguments.shadows then
                threeLights (directionalLight castsShadows lightProperties)
                    sky
                    environment

            else
                threeLights (directionalLight doesNotCastShadows lightProperties)
                    sky
                    environment
    in
    toHtml options
        { lights = lights
        , camera = arguments.camera
        , clipDepth = arguments.clipDepth
        , exposure = exposureValue 15
        , whiteBalance = daylight
        , dimensions = arguments.dimensions
        , background = arguments.background
        }
        entities


cloudy :
    List Option
    ->
        { dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
        , upDirection : Direction3d coordinates
        , camera : Camera3d Meters coordinates
        , clipDepth : Length
        , background : Background coordinates
        }
    -> List (Entity coordinates)
    -> Html msg
cloudy options arguments entities =
    toHtml options
        { lights =
            oneLight <|
                softLighting
                    { upDirection = arguments.upDirection
                    , chromaticity = daylight
                    , intensityAbove = Illuminance.lux 1000
                    , intensityBelow = Illuminance.lux 200
                    }
        , camera = arguments.camera
        , clipDepth = arguments.clipDepth
        , exposure = exposureValue 13
        , whiteBalance = daylight
        , dimensions = arguments.dimensions
        , background = arguments.background
        }
        entities


office :
    List Option
    ->
        { lights : Lights coordinates
        , upDirection : Direction3d coordinates
        , dimensions : ( Quantity Float Pixels, Quantity Float Pixels )
        , camera : Camera3d Meters coordinates
        , clipDepth : Length
        , background : Background coordinates
        }
    -> List (Entity coordinates)
    -> Html msg
office options arguments entities =
    toHtml options
        { lights =
            oneLight <|
                softLighting
                    { upDirection = arguments.upDirection
                    , chromaticity = fluorescentLighting
                    , intensityAbove = Illuminance.lux 400
                    , intensityBelow = Illuminance.lux 100
                    }
        , camera = arguments.camera
        , clipDepth = arguments.clipDepth
        , exposure = exposureValue 7
        , whiteBalance = fluorescentLighting
        , dimensions = arguments.dimensions
        , background = arguments.background
        }
        entities
