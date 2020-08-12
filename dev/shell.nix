with import <nixpkgs> {};
let
  blucontrol = haskellPackages.callCabal2nix "blucontrol" ../. {};
in
  pkgs.mkShell {
    buildInputs = with pkgs; [
      cabal-install
      hlint
      haskellPackages.ghcide
      haskellPackages.implicit-hie
    ];
    inputsFrom = [
      blucontrol.env
    ];
    shellHook = ''
      EXPORTS="$(cat $(which ghc) | grep '^export' | sed "s/'//g")"

      echo "found exports:"
      echo "$EXPORTS"

      _getExportedVariable() {
        echo "$EXPORTS" | grep "^export $1" | sed "s/export $1=//g"
      }

      export NIX_GHC="$(_getExportedVariable "NIX_GHC")"
      export NIX_GHCPKG="$(_getExportedVariable "NIX_GHCPKG")"
      export NIX_GHC_DOCDIR="$(_getExportedVariable "NIX_GHC_DOCDIR")"
      export NIX_GHC_LIBDIR="$(_getExportedVariable "NIX_GHC_LIBDIR")"
    '';
  }
