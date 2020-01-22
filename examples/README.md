## `elm-3d-scene` examples

This directory contains several examples of using `elm-3d-scene` to create
different kinds of 3D graphics. From (roughly) simplest to most complex:

- `Points.elm`
- `LineSegments.elm`
- `Triangles.elm`
- `Translation.elm`
- `Rotation.elm`
- `Orbiting.elm`
- `Sphere.elm`
- `Spheres.elm`
- `OrbitingTexture.elm`
- `TexturedSphere.elm`
- `BallsAndBlocks.elm`
- `Lack.elm`

To get started, check out this repository and start up `elm reactor` in this
directory, then start playing around with the different examples (or add your
own). Try animating things like color, translation, rotation, or lighting! If
you have questions, reach out in the **#webgl** channel or to **@ianmackenzie**
on the [Elm Slack](https://elmlang.herokuapp.com).

### Writing your own

One note on efficiency if you write your own test scene: it's important for
smooth rendering to only generate `Mesh` values as needed (usually in `update`,
`init` or at the top level, and almost never from `view`) and then cache them as
much as possible (store them in your model, or have them as top-level
constants). `elm-3d-scene` is set up so that functions in the `Mesh` module are
'expensive' (should not be called directly or indirectly from `view`) while
functions in the `Scene3d` module are 'cheap' (so you can freely change what
objects are rendered, what transformations are applied to them, what colors to
use for them etc. every frame). This will eventually be explained fully in the
docs, but just something to keep in mind for now if you try writing your own
test scene and get low frame rates. (All the existing examples are written with
this advice in mind, so you can use them as inspiration.)
