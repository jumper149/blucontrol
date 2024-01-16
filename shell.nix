{ ghcVersion ? "ghc948"
, nixpkgs ? import ./nix/nixpkgs.nix
, simple ? false
}:
let
  build = import ./nix/build.nix;
  pkgs = nixpkgs { };
  attrs = {
    inherit (pkgs) nix-gitignore;
    haskellPackages = pkgs.haskell.packages."${ghcVersion}";
  };
in
  if simple
  then build.blucontrolShellSimple (attrs // { inherit (pkgs) mkShell; })
  else build.blucontrolShell (attrs // { inherit (pkgs) rnix-lsp; })
