module Common.Materials exposing
    ( aluminum
    , blackPlastic
    , chromium
    , copper
    , gold
    , whitePlastic
    )

import Color exposing (Color)
import Scene3d.Material as Material exposing (Material)


gold : Material.Uniform coordinates
gold =
    Material.metal
        { baseColor = Color.fromRGB ( 255, 195, 86 )
        , roughness = 0.4
        }


aluminum : Material.Uniform coordinates
aluminum =
    Material.metal
        { baseColor = Color.fromRGB ( 233, 235, 236 )
        , roughness = 0.6
        }


copper : Material.Uniform coordinates
copper =
    Material.metal
        { baseColor = Color.fromRGB ( 244, 162, 137 )
        , roughness = 0.25
        }


chromium : Material.Uniform coordinates
chromium =
    Material.metal
        { baseColor = Color.fromRGB ( 140, 142, 141 )
        , roughness = 0.5
        }


blackPlastic : Material.Uniform coordinates
blackPlastic =
    Material.nonmetal
        { baseColor = Color.fromRGB ( 0, 0, 0 )
        , roughness = 0.5
        }


whitePlastic : Material.Uniform coordinates
whitePlastic =
    Material.nonmetal
        { baseColor = Color.fromRGB ( 255, 255, 255 )
        , roughness = 0.25
        }
