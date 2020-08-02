with import <nixpkgs> {};
let
  bludigon = haskellPackages.callCabal2nix "bludigon" ./. {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      hlint
    ];
    inputsFrom = [
      bludigon.env
    ];
  }
