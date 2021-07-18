{ ghcVersion ? "ghc8104"
, nixpkgs ? import ./nix/nixpkgs.nix
, packages ? (_:[])
}:
let
  build = import ./nix/build.nix;
  pkgs = nixpkgs { };
  attrs = {
    inherit (pkgs) lib nix-gitignore stdenv makeWrapper;
    inherit packages;
    haskellPackages = pkgs.haskell.packages."${ghcVersion}";
  };
in
  build.blucontrolWrapped attrs
