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
import Scene3d.Mesh exposing (Yes)


gold : Material { hasNormals : Yes }
gold =
    Material.metal
        { baseColor = Color.fromRGB ( 255, 195, 86 )
        , roughness = 0.4
        }


aluminum : Material { hasNormals : Yes }
aluminum =
    Material.metal
        { baseColor = Color.fromRGB ( 233, 235, 236 )
        , roughness = 0.6
        }


copper : Material { hasNormals : Yes }
copper =
    Material.metal
        { baseColor = Color.fromRGB ( 244, 162, 137 )
        , roughness = 0.25
        }


chromium : Material { hasNormals : Yes }
chromium =
    Material.metal
        { baseColor = Color.fromRGB ( 140, 142, 141 )
        , roughness = 0.5
        }


blackPlastic : Material { hasNormals : Yes }
blackPlastic =
    Material.nonmetal
        { baseColor = Color.fromRGB ( 0, 0, 0 )
        , roughness = 0.5
        }


whitePlastic : Material { hasNormals : Yes }
whitePlastic =
    Material.nonmetal
        { baseColor = Color.fromRGB ( 255, 255, 255 )
        , roughness = 0.25
        }
