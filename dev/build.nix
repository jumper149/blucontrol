with import <nixpkgs> {};
(import ./default.nix) {
  inherit stdenv makeWrapper;
  makeBinPath = lib.makeBinPath;
  ghcWithPackages = haskellPackages.ghcWithPackages;
}
