module Animation exposing (main)

{-| The example uses an onAnimationFrameDelta subscription to implement a simple
loading spinner, and shows how you can incorporate elm-3d-scene into an elm-ui
layout.
-}

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Events
import Camera3d
import Color
import Direction3d
import Duration exposing (Duration)
import Element
import Html exposing (Html)
import Length
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Material as Material
import Viewpoint3d


type WorldCoordinates
    = WorldCoordinates


{-| Store the rotation angle of a cube to use as a loading spinner
-}
type alias Model =
    { angle : Angle
    }


{-| Receive a Tick message on every animation frame with elapsed duration since
last frame (should usually be around 16 milliseconds)
-}
type Msg
    = Tick Duration


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init () =
    ( { angle = Quantity.zero }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update (Tick duration) model =
    let
        -- Speed at which the cube rotates; Angle, Quantity and Duration are
        -- all modules from elm-units
        rotationRate =
            Angle.degrees 90 |> Quantity.per Duration.second

        -- Update the current angle by adding a delta equal to the rotation rate
        -- multiplied by elapsed time since the last animation frame
        updatedAngle =
            model.angle |> Quantity.plus (rotationRate |> Quantity.for duration)
    in
    ( { model | angle = updatedAngle }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Subscribe to animation frames and wrap each time step (a number of
    -- milliseconds) into a Duration value and then into a Tick message
    Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)


{-| Create a cube entity by constructing six square faces with different colors
-}
initialCube : Scene3d.Entity WorldCoordinates
initialCube =
    let
        -- Define the negative and positive X/Y/Z coordinates of a 16 'pixel'
        -- wide cube centered at the origin (see https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/Length#cssPixels)
        negative =
            Length.cssPixels -8

        positive =
            Length.cssPixels 8

        -- Define the eight vertices of the cube
        p1 =
            Point3d.xyz negative negative negative

        p2 =
            Point3d.xyz positive negative negative

        p3 =
            Point3d.xyz positive positive negative

        p4 =
            Point3d.xyz negative positive negative

        p5 =
            Point3d.xyz negative negative positive

        p6 =
            Point3d.xyz positive negative positive

        p7 =
            Point3d.xyz positive positive positive

        p8 =
            Point3d.xyz negative positive positive

        -- Create the six faces with different colors
        bottom =
            Scene3d.quad (Material.color Color.blue) p1 p2 p3 p4

        top =
            Scene3d.quad (Material.color Color.blue) p5 p6 p7 p8

        front =
            Scene3d.quad (Material.color Color.orange) p2 p3 p7 p6

        back =
            Scene3d.quad (Material.color Color.orange) p1 p4 p8 p5

        left =
            Scene3d.quad (Material.color Color.green) p1 p2 p6 p5

        right =
            Scene3d.quad (Material.color Color.green) p4 p3 p7 p8
    in
    -- Combine all faces into a single entity
    Scene3d.group [ bottom, top, front, back, left, right ]


view : Model -> Html Msg
view model =
    let
        -- Define the angled axis that the cube rotates around by specifying a
        -- point and a direction
        rotationAxis =
            Axis3d.through Point3d.origin <|
                -- A direction in the XZ plane, 45 degrees up from X towards Z
                Direction3d.xz (Angle.degrees 45)

        -- Rotate the initial cube around the rotation axis by the current angle
        rotatedCube =
            initialCube |> Scene3d.rotateAround rotationAxis model.angle

        -- Create an isometric camera
        camera =
            Camera3d.orthographic
                { viewpoint =
                    Viewpoint3d.isometric
                        { focalPoint = Point3d.origin
                        , distance = Length.cssPixels 100
                        }
                , viewportHeight = Length.cssPixels 32
                }
    in
    -- Create a little loading spinner layout using elm-ui
    Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
        Element.row [ Element.centerX, Element.centerY ]
            [ Element.html <|
                Scene3d.unlit
                    { camera = camera
                    , dimensions = ( Pixels.int 32, Pixels.int 32 )
                    , entities = [ rotatedCube ]
                    , clipDepth = Length.cssPixels 10
                    , background = Scene3d.transparentBackground
                    }
            , Element.text "Loading..."
            ]
