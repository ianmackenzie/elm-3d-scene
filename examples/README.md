## `elm-3d-scene` examples

This directory contains several examples of using `elm-3d-scene` to create
different kinds of 3D graphics. To get started, check out the `elm-3d-scene`
repository and start up `elm reactor` in this directory, then start playing
around with the different examples (or create your own!). If you have questions,
please reach out in the **#webgl** channel or to **@ianmackenzie** on the [Elm Slack](https://elmlang.herokuapp.com).

### Beginner

These examples (in rough order of complexity) introduce various core rendering
techniques and concepts.

- [`HelloWorld.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/HelloWorld.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/hello-world.html))
  (adapted from the [tutorial](https://github.com/ianmackenzie/elm-3d-scene/blob/master/TUTORIAL.md)) 
- [`Triangles.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Triangles.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/triangles.html))
- [`Points.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Points.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/points.html))
- [`PhysicallyBasedRendering.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/PhysicallyBasedRendering.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/physically-based-rendering.html))
- [`Rotation.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Rotation.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/rotation.html))
- [`Meshes.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Meshes.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/meshes.html))
- [`LightingAndShadows.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/LightingAndShadows.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/lighting-and-shadows.html))

### Intermediate

These examples incorporate The Elm Architecture (TEA) in order to implement
animation, interactivity, and texturing (which requires an HTTP request, and
therefore some state handling).

- [`Texture.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Texture.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/texture.html))
- [`Animation.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Animation.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/animation.html))
- [`DynamicBackground.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/DynamicBackground.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/dynamic-background.html))
- [`OrbitingCamera.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/OrbitingCamera.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/orbiting-camera.html))

### Advanced

Finally, these examples generally combine two or more techniques/concepts to
create more complex and interesting scenes. 

- [`TexturedSphere.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/TexturedSphere.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/textured-sphere.html))
- [`MultipleShadows.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/MultipleShadows.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/multiple-shadows.html))
- [`ExposureAndToneMapping.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/ExposureAndToneMapping.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/exposure-and-tone-mapping.html))
