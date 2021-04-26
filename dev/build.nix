with import <nixpkgs> {};
callPackage (import ../default.nix) { haskellPackages = haskell.packages.ghc8104; }
