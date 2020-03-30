# elm-3d-scene

`elm-3d-scene` is a high-level Elm package for producing 3D graphics. It aims to
make creating 3D graphics as easy and enjoyable as possible, without having to
worry about low-level details like shader programs and transformation matrices
but still including powerful features such as:

- Efficient animated transformations
- Physically-based materials
- Multiple light sources
- Accurate shadows

The documentation for `elm-3d-scene` (including this README!) is still being
worked on, but once that is done (soon!) it will be published as a normal Elm
package. In the meantime, you can [watch my elm-conf talk](https://www.youtube.com/watch?v=Htqc64s5qYU)
and try out the package by checking out this repository and then firing up `elm
reactor` in [the examples folder](https://github.com/ianmackenzie/elm-3d-scene/tree/master/examples).

Any questions/feedback, please open an issue or reach out to **@ianmackenzie**
on the [Elm Slack](https://elmlang.herokuapp.com)!

## Contributing

Contributions are very welcome! Here are some ideas for some that would be very
useful at this stage:

- More examples to add to the `examples` directory, showing how to create
  specific kinds of scenes or how to integrate `elm-3d-scene` with other
  packages, such as:
  - An example combining `elm-3d-scene` and [`elm-gamepad`](https://package.elm-lang.org/packages/xarvh/elm-gamepad/latest/),
    perhaps using the gamepad to walk around a scene
  - An example with an `elm-3d-scene` scene within an [`elm-ui`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/)
    layout
  - A simple example using `elm-3d-scene` with just plain colors (no lighting)
    to create a small 3D loading spinner widget
  - An example with shadows cast from a moving light source (I think something
    like a point light flying in and amongst a bunch of cubes could look pretty
    cool)
  - Anything you think would look cool/illustrate part of the API missed by
    other examples/help others understand something you struggled with
- Separate packages that parse different 3D model formats so they can be loaded
  into `elm-3d-scene`, e.g.:
  - [OBJ](https://en.wikipedia.org/wiki/Wavefront_.obj_file)
  - [STL](https://en.wikipedia.org/wiki/STL_(file_format))
  - [DAE](https://en.wikipedia.org/wiki/COLLADA)

  I think it makes sense for each file format to have a dedicated package 
  separate from `elm-3d-scene`. Ideally, I think each package should focus on
  simply parsing a file into a nice Elm data structure (instead of directly into
  an `elm-3d-scene` `Entity`, for example) so that the data could also be used
  in other ways (custom rendering engines, 3D printing, analysis etc.).
  
  Note that some work has already started on [GLTF](https://en.wikipedia.org/wiki/GlTF)
  loading, so reach out to me (@ianmackenzie) on the [Elm Slack](http://elmlang.herokuapp.com/)
  if that's something you're interested in working on.

Once the internals of `elm-3d-scene` stabilize a bit, there will be lots more
opportunities to contribute to the core rendering engine, to add features such
as:

- Textured backgrounds (https://github.com/ianmackenzie/elm-3d-scene/issues/53)
- Rectangular/polygonal light sources (https://github.com/ianmackenzie/elm-3d-scene/issues/41)
- 3D-rendered text (https://github.com/ianmackenzie/elm-3d-scene/issues/37)
- Procedural textures (https://github.com/ianmackenzie/elm-3d-scene/issues/22)
- Debug rendering options (https://github.com/ianmackenzie/elm-3d-scene/issues/21)
- Funky material types like water and ceramics (https://github.com/ianmackenzie/elm-3d-scene/issues/5)

For now, though, the internals are changing rapidly enough that
coordination/collaboration on these kinds of issues would be tricky.

## Climate action

I would like for the projects I work on to be as helpful as possible in
addressing the climate crisis. If

- you are working on a project that helps address the climate crisis (clean
  energy, public transit, reforestation, sustainable agriculture etc.) either as
  an individual, as part of an non-profit organization or even as part of a
  for-profit company, and
- there is a new feature you would find helpful for that work (or a bug you need
  fixed) in any of my open-source projects, then

please [open a new issue](https://github.com/ianmackenzie/elm-3d-scene/issues),
describe briefly what you're working on and I will treat that issue as high
priority.

![Physics simulation](https://ianmackenzie.github.io/elm-3d-scene/images/1.0.0/physics-background.png)
