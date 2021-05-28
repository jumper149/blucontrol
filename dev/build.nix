let
  nixpkgs = import ./nixpkgs.nix;
  haskellPackages = nixpkgs.haskell.packages.ghc8104;
in
  nixpkgs.callPackage (import ../default.nix) { haskellPackages = haskellPackages; }
