with import <nixpkgs> {};
let
  bludigon = haskellPackages.callCabal2nix "bludigon" ./. {};
in
  pkgs.mkShell {
    buildInputs = [
      cabal-install
    ];
    inputsFrom = [
      bludigon.env
    ];
  }
