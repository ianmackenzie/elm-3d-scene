## `elm-3d-scene` examples

This directory contains several examples of using `elm-3d-scene` to create
different kinds of 3D graphics. Click the 'HTML' link of any example to see a
live version. To edit/play around with the examples, there are two main options:

- Click the 'Ellie' link of any example to edit/remix it on [Ellie](https://ellie-app.com)
- Clone the `elm-3d-scene` repository and start up `elm reactor` in this
  directory, then start playing around with the different examples (or create
  your own!)
  
If you have questions, please reach out in the **#webgl** channel or to
**@ianmackenzie** on the [Elm Slack](https://elmlang.herokuapp.com).

### Beginner

These examples (in rough order of complexity) introduce various core rendering
techniques and concepts.

- [`HelloWorld.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/HelloWorld.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/hello-world.html), [Ellie](https://ellie-app.com/9g2P5dZ2tT9a1))
  (adapted from the [tutorial](https://github.com/ianmackenzie/elm-3d-scene/blob/master/TUTORIAL.md)) 
- [`Triangles.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Triangles.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/triangles.html), [Ellie](https://ellie-app.com/9g2Rndnhtxsa1))
- [`Points.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Points.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/points.html), [Ellie](https://ellie-app.com/9g2QyXDBLPha1))
- [`PhysicallyBasedRendering.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/PhysicallyBasedRendering.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/physically-based-rendering.html), [Ellie](https://ellie-app.com/9g2QgTKc9sya1))
- [`Rotation.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Rotation.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/rotation.html), [Ellie](https://ellie-app.com/9g2QLCp29yPa1))
- [`Meshes.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Meshes.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/meshes.html), [Ellie](https://ellie-app.com/9g2PxLDWHjZa1))
- [`LightingAndShadows.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/LightingAndShadows.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/lighting-and-shadows.html), [Ellie](https://ellie-app.com/9g2Pjt4Dgyga1))

### Intermediate

These examples incorporate The Elm Architecture (TEA) in order to implement
animation, interactivity, and texturing (which requires an HTTP request, and
therefore some state handling).

- [`Texture.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Texture.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/texture.html), [Ellie](https://ellie-app.com/9g2QWLphZVma1))
- [`Animation.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Animation.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/animation.html), [Ellie](https://ellie-app.com/9g2NcRZPsXva1))
- [`DynamicBackground.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/DynamicBackground.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/dynamic-background.html), [Ellie](https://ellie-app.com/9g2NtVNqz7ca1))
- [`OrbitingCamera.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/OrbitingCamera.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/orbiting-camera.html), [Ellie](https://ellie-app.com/9g2Q5BLWpm8a1))

### Advanced

Finally, these examples generally combine two or more techniques/concepts to
create more complex and interesting scenes. 

- [`TexturedSphere.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/TexturedSphere.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/textured-sphere.html), [Ellie](https://ellie-app.com/9g2R9VDG6NHa1))
- [`MultipleShadows.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/MultipleShadows.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/multiple-shadows.html), [Ellie](https://ellie-app.com/9g2PQxgqk49a1))
- [`ExposureAndToneMapping.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/ExposureAndToneMapping.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/exposure-and-tone-mapping.html), [Ellie](https://ellie-app.com/9g2NQtQNxXpa1))
- [`Overlay.elm`](https://github.com/ianmackenzie/elm-3d-scene/blob/master/examples/Overlay.elm)
  ([HTML](https://ianmackenzie.github.io/elm-3d-scene/examples/1.0.0/overlay.html), [Ellie](https://ellie-app.com/bdkzqjBxCtQa1))
