with import <nixpkgs> {};
let
  blucontrol = haskellPackages.callCabal2nix "blucontrol" ./. {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      hlint
    ];
    inputsFrom = [
      blucontrol.env
    ];
  }
