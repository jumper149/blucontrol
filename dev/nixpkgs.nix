let
  overlay-chooseGhc = self: super: {
    haskellPackages = super.haskell.packages.ghc8104;
  };
in
  import <nixpkgs> { overlays = [ overlay-chooseGhc ]; }
