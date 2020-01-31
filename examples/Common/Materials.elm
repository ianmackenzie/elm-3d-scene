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


gold : Material.Pbr
gold =
    { baseColor = Color.fromRGB ( 255, 195, 86 )
    , roughness = 0.4
    , metallic = 1
    }


aluminum : Material.Pbr
aluminum =
    { baseColor = Color.fromRGB ( 233, 235, 236 )
    , roughness = 0.6
    , metallic = 1
    }


copper : Material.Pbr
copper =
    { baseColor = Color.fromRGB ( 244, 162, 137 )
    , roughness = 0.25
    , metallic = 1
    }


chromium : Material.Pbr
chromium =
    { baseColor = Color.fromRGB ( 140, 142, 141 )
    , roughness = 0.5
    , metallic = 1
    }


blackPlastic : Material.Pbr
blackPlastic =
    { baseColor = Color.fromRGB ( 0, 0, 0 )
    , roughness = 0.5
    , metallic = 0
    }


whitePlastic : Material.Pbr
whitePlastic =
    { baseColor = Color.fromRGB ( 255, 255, 255 )
    , roughness = 0.25
    , metallic = 0
    }
