{ ghcVersion ? "ghc8104"
, nixpkgs ? (import ./nix/nixpkgs.nix)
, simple ? false
}:
let
  build = import ./nix/build.nix;
  attrs = {
    inherit (nixpkgs) nix-gitignore;
    haskellPackages = nixpkgs.haskell.packages."${ghcVersion}";
  };
in
  if simple
  then build.blucontrolShellSimple (attrs // { inherit (nixpkgs) mkShell; })
  else build.blucontrolShell (attrs // { inherit (nixpkgs) rnix-lsp; })
