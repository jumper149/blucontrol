{ ghcVersion ? "ghc8104"
, nixpkgs ? (import ./nix/nixpkgs.nix)
, packages ? (_:[])
}:
let
  build = import ./nix/build.nix;
  attrs = {
    inherit (nixpkgs) lib nix-gitignore stdenv makeWrapper;
    inherit packages;
    haskellPackages = nixpkgs.haskell.packages."${ghcVersion}";
  };
in
  build.blucontrolWrapped attrs
