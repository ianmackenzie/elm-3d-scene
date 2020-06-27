# `elm-3d-scene` tutorial

This tutorial certainly won't cover every feature of `elm-3d-scene`, but it
should give you a decent grasp of the core concepts that are involved in setting
up _any_ `elm-3d-scene`...scene. We'll be rendering a simple blue square - it
will look like this:

![Unlit square](https://ianmackenzie.github.io/elm-3d-scene/images/1.0.0/unlit-square.png)

## Setup

`elm-3d-scene` makes extensive use of a few other `ianmackenzie/*` geometry-related packages:

- [`elm-geometry`](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/)
  for modules such as `Point3d`, `Direction3d`, `Axis3d`, `LineSegment3d`, and
  `Triangle3d`
- [`elm-units`](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/)
  for modules such as `Length`, `Angle`, `Pixels`, and `Luminance`
- [`elm-3d-camera`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest/)
  for `Camera3d` and `Viewpoint3d`

You'll also need [`avh4/elm-color`](https://package.elm-lang.org/packages/avh4/elm-color/latest/).
As a result, when setting up pretty much any app that uses `elm-3d-scene`, you
will need to run something like:

```
elm install ianmackenzie/elm-3d-scene
elm install ianmackenzie/elm-geometry
elm install ianmackenzie/elm-units
elm install ianmackenzie/elm-3d-camera
elm install avh4/elm-color
```

In more sophisticated apps, you will also need to install [`elm-triangular-mesh`](https://package.elm-lang.org/packages/ianmackenzie/elm-triangular-mesh/latest/).

## Entities

Entities are the visible objects in a scene. In general, they have some geometry
that defines their shape and a 'material' that defines their color (which can
include things like texturing and reflectivity). In this scene, there's only one
entity:

```elm
entities =
    [ Scene3d.quad (Material.color Color.blue)
        (Point3d.meters -1 -1 0)
        (Point3d.meters 1 -1 0)
        (Point3d.meters 1 1 0)
        (Point3d.meters -1 1 0)
    ]
```

Here, we've constructed a single blue square by calling [`Scene3d.quad`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d#quad)
with a [constant color material](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d-Material#color)
and the four corner [points](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Point3d)
of the square in counterclockwise order. (`Scene3d.quad` can also be used to
create rectangles, [parallelograms](https://en.wikipedia.org/wiki/Parallelogram),
[trapezoids](https://en.wikipedia.org/wiki/Trapezoid) etc.)

Note that entities are always constructed using real-world units - this example
uses meters but you can also use feet, centimeters etc. Using real-world units
becomes very important later on when using realistic lighting!

## Cameras

Once we have some entities to render, we need to define a virtual 'camera' that
will be used to render the scene:

```elm
camera =
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { eyePoint = Point3d.meters 5 2 3
                , focalPoint = Point3d.origin
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 30
        }
```

Here, we've used the `Camera3d` and `Viewpoint3d` modules from [`elm-3d-camera`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest)
to define a camera:

- Located at the point with coordinates (5, 2, 3) in meters;
- Looking towards the origin point (the point with coordinates (0, 0, 0));
- Oriented so that the positive Z [direction](https://package.elm-lang.org/packages/ianmackenzie/elm-geometry/latest/Direction3d)
  appears to be up;
- And using [perspective projection](http://glasnost.itcarlow.ie/~powerk/GeneralGraphicsNotes/projection/perspective_projection.html),
  with a vertical [field of view](https://en.wikipedia.org/wiki/Field_of_view)
  of 30 [degrees](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Angle)
  (a reasonable default for realistic rendering).

![Camera diagram](https://ianmackenzie.github.io/elm-3d-scene/images/1.0.0/camera.png)

The [`Viewpoint3d`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest/Viewpoint3d)
module has several additional functions for defining the position and orientation of
cameras, and you can use [`Camera3d.orthographic`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-camera/latest/Camera3d#orthographic)
if you want to use [orthographic projection](https://en.wikipedia.org/wiki/Orthographic_projection)
instead.

## Rendering

Finally, to actually render the scene into an HTML element we can include on our
page, we can use [`Scene3d.unlit`](https://package.elm-lang.org/packages/ianmackenzie/elm-3d-scene/latest/Scene3d#unlit)
since our scene doesn't involve any lighting effects. Here's the complete
program:

```elm
module HelloWorld exposing (main)

import Angle
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d


main : Html msg
main =
    Scene3d.unlit
        { entities =
            [ Scene3d.quad (Material.color Color.blue)
                (Point3d.meters -1 -1 0)
                (Point3d.meters 1 -1 0)
                (Point3d.meters 1 1 0)
                (Point3d.meters -1 1 0)
            ]
        , camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 5 2 3
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
        , clipDepth = Length.meters 1
        , background = Scene3d.transparentBackground
        , dimensions = ( Pixels.pixels 800, Pixels.pixels 600 )
        }
```

Note that `Scene3d.unlit` takes a few more parameters in addition to `entities`
and `camera`: 

- `clipDepth` defines how close objects can get to the camera before they're
  cut off. In this example, you can imagine the camera having an invisible
  screen one [meter](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length)
  in front of the camera that would crop anything that got too close.
  ![Clip depth diagram](https://ianmackenzie.github.io/elm-3d-scene/images/1.0.0/clip-depth.png)
- `background` specifies the background color to use when rendering. In this 
  case we've used a transparent background so that the background color of the
  HTML underneath (or any other HTML content that happens to be underneath!)
  will show through.
- `dimensions` defines the size in [pixels](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Pixels)
  that the scene should be rendered at. Here we use a hardcoded size for
  simplicity.

## Next steps

This has just scratched the surface of `elm-3d-scene` - check out the [examples](https://github.com/ianmackenzie/elm-3d-scene/tree/master/examples/README.md)
to see how to:

- Add animation to your scene (using Elm techniques you already know!)
- Add interactivity such as an orbiting camera
- Use built-in shapes such as balls and blocks
- Add realistic lighting and shadows
- Define custom shapes using triangular meshes
- Apply textures to your objects
- Support high dynamic range (HDR) scenes
