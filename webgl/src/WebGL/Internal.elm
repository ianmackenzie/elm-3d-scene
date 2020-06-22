module WebGL.Internal exposing
    ( Option(..)
    , Setting(..)
    , enableOption
    , enableSetting
    )

import Elm.Kernel.WebGL


type Option
    = Alpha Bool
    | Depth Float
    | Stencil Int
    | Antialias
    | ClearColor Float Float Float Float
    | PreserveDrawingBuffer


enableOption : () -> Option -> ()
enableOption ctx option =
    case option of
        Alpha _ ->
            Elm.Kernel.WebGL.enableAlpha ctx option

        Depth _ ->
            Elm.Kernel.WebGL.enableDepth ctx option

        Stencil _ ->
            Elm.Kernel.WebGL.enableStencil ctx option

        Antialias ->
            Elm.Kernel.WebGL.enableAntialias ctx option

        ClearColor _ _ _ _ ->
            Elm.Kernel.WebGL.enableClearColor ctx option

        PreserveDrawingBuffer ->
            Elm.Kernel.WebGL.enablePreserveDrawingBuffer ctx option


type Setting
    = Blend Int Int Int Int Int Int Float Float Float Float
    | DepthTest Int Bool Float Float
    | StencilTest Int Int Int Int Int Int Int Int Int Int Int
    | Scissor Int Int Int Int
    | ColorMask Bool Bool Bool Bool
    | CullFace Int
    | PolygonOffset Float Float
    | SampleCoverage Float Bool
    | SampleAlphaToCoverage


enableSetting : () -> Setting -> ()
enableSetting cache setting =
    case setting of
        Blend _ _ _ _ _ _ _ _ _ _ ->
            Elm.Kernel.WebGL.enableBlend cache setting

        DepthTest _ _ _ _ ->
            Elm.Kernel.WebGL.enableDepthTest cache setting

        StencilTest _ _ _ _ _ _ _ _ _ _ _ ->
            Elm.Kernel.WebGL.enableStencilTest cache setting

        Scissor _ _ _ _ ->
            Elm.Kernel.WebGL.enableScissor cache setting

        ColorMask _ _ _ _ ->
            Elm.Kernel.WebGL.enableColorMask cache setting

        CullFace _ ->
            Elm.Kernel.WebGL.enableCullFace cache setting

        PolygonOffset _ _ ->
            Elm.Kernel.WebGL.enablePolygonOffset cache setting

        SampleCoverage _ _ ->
            Elm.Kernel.WebGL.enableSampleCoverage cache setting

        SampleAlphaToCoverage ->
            Elm.Kernel.WebGL.enableSampleAlphaToCoverage cache
