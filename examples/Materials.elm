module Materials
    exposing
        ( aluminum
        , blackPlastic
        , chromium
        , copper
        , gold
        , whitePlastic
        )

import Math.Vector3 exposing (vec3)
import OpenSolid.Scene.Material as Material exposing (Material)


gold : Material
gold =
    Material.metal { color = vec3 1 0.766 0.336, roughness = 0.4 }


aluminum : Material
aluminum =
    Material.metal { color = vec3 0.913 0.921 0.925, roughness = 0.6 }


copper : Material
copper =
    Material.metal { color = vec3 0.955 0.637 0.538, roughness = 0.25 }


chromium : Material
chromium =
    Material.metal { color = vec3 0.55 0.556 0.554, roughness = 0.5 }


blackPlastic : Material
blackPlastic =
    Material.nonmetal { color = vec3 0 0 0, roughness = 0.5 }


whitePlastic : Material
whitePlastic =
    Material.nonmetal { color = vec3 1 1 1, roughness = 0.25 }
