module Common.Materials exposing
    ( aluminum
    , blackPlastic
    , chromium
    , copper
    , gold
    , whitePlastic
    )

import Color exposing (Color)
import Scene3d.Drawable exposing (Material)


gold : Material
gold =
    { baseColor = Color.fromRGB ( 255, 195, 86 ), roughness = 0.4, metallic = True }


aluminum : Material
aluminum =
    { baseColor = Color.fromRGB ( 233, 235, 236 ), roughness = 0.6, metallic = True }


copper : Material
copper =
    { baseColor = Color.fromRGB ( 244, 162, 137 ), roughness = 0.25, metallic = True }


chromium : Material
chromium =
    { baseColor = Color.fromRGB ( 140, 142, 141 ), roughness = 0.5, metallic = True }


blackPlastic : Material
blackPlastic =
    { baseColor = Color.fromRGB ( 0, 0, 0 ), roughness = 0.5, metallic = False }


whitePlastic : Material
whitePlastic =
    { baseColor = Color.fromRGB ( 255, 255, 255 ), roughness = 0.25, metallic = False }
