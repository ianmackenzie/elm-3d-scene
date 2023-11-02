{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = [
            pict
            pkgs.elmPackages.elm
            pkgs.elmPackages.elm-format
            pkgs.elmPackages.elm-test
            pkgs.elmPackages.elm-json
          ];
        };
      });
}
