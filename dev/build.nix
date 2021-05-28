let
  nixpkgs = import ./nixpkgs.nix;
in
  nixpkgs.callPackage (import ../default.nix) {}
