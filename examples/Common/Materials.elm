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
    { baseColor = Color.rgb255 255 195 86, roughness = 0.4, metallic = True }


aluminum : Material
aluminum =
    { baseColor = Color.rgb255 233 235 236, roughness = 0.6, metallic = True }


copper : Material
copper =
    { baseColor = Color.rgb255 244 162 137, roughness = 0.25, metallic = True }


chromium : Material
chromium =
    { baseColor = Color.rgb255 140 142 141, roughness = 0.5, metallic = True }


blackPlastic : Material
blackPlastic =
    { baseColor = Color.black, roughness = 0.5, metallic = False }


whitePlastic : Material
whitePlastic =
    { baseColor = Color.white, roughness = 0.25, metallic = False }
