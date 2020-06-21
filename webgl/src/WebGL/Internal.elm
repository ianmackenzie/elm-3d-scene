module WebGL.Internal exposing
    ( Option(..)
    , Setting(..)
    , disableSetting
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


enableSetting : () -> () -> Setting -> ()
enableSetting gl glSettings setting =
    case setting of
        Blend _ _ _ _ _ _ _ _ _ _ ->
            Elm.Kernel.WebGL.enableBlend gl glSettings setting

        DepthTest _ _ _ _ ->
            Elm.Kernel.WebGL.enableDepthTest gl glSettings setting

        StencilTest _ _ _ _ _ _ _ _ _ _ _ ->
            Elm.Kernel.WebGL.enableStencilTest gl glSettings setting

        Scissor _ _ _ _ ->
            Elm.Kernel.WebGL.enableScissor gl glSettings setting

        ColorMask _ _ _ _ ->
            Elm.Kernel.WebGL.enableColorMask gl glSettings setting

        CullFace _ ->
            Elm.Kernel.WebGL.enableCullFace gl glSettings setting

        PolygonOffset _ _ ->
            Elm.Kernel.WebGL.enablePolygonOffset gl glSettings setting

        SampleCoverage _ _ ->
            Elm.Kernel.WebGL.enableSampleCoverage gl glSettings setting

        SampleAlphaToCoverage ->
            Elm.Kernel.WebGL.enableSampleAlphaToCoverage gl glSettings setting


disableSetting : () -> Setting -> ()
disableSetting cache setting =
    case setting of
        Blend _ _ _ _ _ _ _ _ _ _ ->
            Elm.Kernel.WebGL.disableBlend cache

        DepthTest _ _ _ _ ->
            Elm.Kernel.WebGL.disableDepthTest cache

        StencilTest _ _ _ _ _ _ _ _ _ _ _ ->
            Elm.Kernel.WebGL.disableStencilTest cache

        Scissor _ _ _ _ ->
            Elm.Kernel.WebGL.disableScissor cache

        ColorMask _ _ _ _ ->
            Elm.Kernel.WebGL.disableColorMask cache

        CullFace _ ->
            Elm.Kernel.WebGL.disableCullFace cache

        PolygonOffset _ _ ->
            Elm.Kernel.WebGL.disablePolygonOffset cache

        SampleCoverage _ _ ->
            Elm.Kernel.WebGL.disableSampleCoverage cache

        SampleAlphaToCoverage ->
            Elm.Kernel.WebGL.disableSampleAlphaToCoverage cache
