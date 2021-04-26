with import <nixpkgs> {};
let
  packages = haskell.packages.ghc8104;
  blucontrol = packages.callCabal2nix "blucontrol" ../. {};
in
  pkgs.mkShell {
    buildInputs = with packages; [
      cabal-install
      hlint
      haskell-language-server
      implicit-hie
    ];
    inputsFrom = [
      blucontrol.env
    ];
    shellHook = ''
      EXPORTS="$(cat $(which ghc) | grep '^export' | sed "s/'//g")"

      _getExportedVariable() {
        echo "$EXPORTS" | grep "^export $1=" | sed "s/export $1=//g"
      }

      echo "found exports:"
      echo "$EXPORTS"

      echo "applying exports:"

      export NIX_GHC="$(_getExportedVariable "NIX_GHC")"
      echo "export NIX_GHC=$NIX_GHC"

      export NIX_GHCPKG="$(_getExportedVariable "NIX_GHCPKG")"
      echo "export NIX_GHCPKG=$NIX_GHCPKG"

      export NIX_GHC_DOCDIR="$(_getExportedVariable "NIX_GHC_DOCDIR")"
      echo "export NIX_GHC_DOCDIR=$NIX_GHC_DOCDIR"

      export NIX_GHC_LIBDIR="$(_getExportedVariable "NIX_GHC_LIBDIR")"
      echo "export NIX_GHC_LIBDIR=$NIX_GHC_LIBDIR"
    '';
  }
