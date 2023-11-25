{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        elmScriptSrc = pkgs.fetchgit {
          url = "https://github.com/ianmackenzie/elm-script";
          rev = "cc0fb47dda8b2787baee76410b3a0fbbce867944";
          sha256 = "sha256-/K0s7CYhXhtfTrokudxieUygK0yDbD370FFCEPasVbs=";
        };
        pict = pkgs.stdenv.mkDerivation {
          name = "pict";
          version = "3.7.4";
          src = pkgs.fetchgit {
            url = "https://github.com/microsoft/pict";
            rev = "d529bb1535b725c8f29704f183b23dd344895d02";
            sha256 = "sha256-APL1DHwJAOqk1dmWtGL7cPlMfnSfsiIpap3CyBu7fbg=";
          };
          nativeBuildInputs = [ pkgs.cmake ];
        };
        elm-script = pkgs.runCommand "elm-script" { buildInputs = [ pkgs.makeWrapper ]; }
          ''
            makeWrapper ${pkgs.deno}/bin/deno $out/bin/elm-script \
              --add-flags "run --quiet --allow-all --no-config ${elmScriptSrc}/runner/main.js"
          '';
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            elm-script
            pict
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-test
            pkgs.elmPackages.elm-json
          ];
          shellHook = ''
            rm -f elm-script
            ln -s ${elmScriptSrc} elm-script
          '';
        };
      });
}
