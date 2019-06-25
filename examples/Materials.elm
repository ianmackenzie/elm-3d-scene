module Materials exposing
    ( aluminum
    , blackPlastic
    , chromium
    , copper
    , gold
    , whitePlastic
    )

import Color
import Scene3d.Material as Material exposing (Material)


gold : Material
gold =
    Material.metal { color = Color.rgb255 255 195 86, roughness = 0.4 }


aluminum : Material
aluminum =
    Material.metal { color = Color.rgb255 233 235 236, roughness = 0.6 }


copper : Material
copper =
    Material.metal { color = Color.rgb255 244 162 137, roughness = 0.25 }


chromium : Material
chromium =
    Material.metal { color = Color.rgb255 140 142 141, roughness = 0.5 }


blackPlastic : Material
blackPlastic =
    Material.nonmetal { color = Color.black, roughness = 0.5 }


whitePlastic : Material
whitePlastic =
    Material.nonmetal { color = Color.white, roughness = 0.25 }
