{ pkgs ? import <nixpkgs> {} }:

# Regenerate the "$project.cabal" file before running the build
let package = pkgs.haskellPackages.callCabal2nix "name" ./. {};
in pkgs.haskell.lib.overrideCabal package (original: {
    preBuild = ''
        echo "hi"
        ${original.preBuild or ""}
    '';
})