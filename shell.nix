let
  nixpkgs = import ./nix/nixpkgs.nix;
  blucontrol = haskellPackages: haskellPackages.callCabal2nix "blucontrol" ./. {};
in
  nixpkgs.haskellPackages.shellFor {
    buildInputs = with nixpkgs.haskellPackages; [
      haskell-language-server
      hlint
      implicit-hie
    ];
    packages = haskellPackages: [
      (blucontrol haskellPackages)
    ];
    withHoogle = true;
  }

# TODO: workaround to use ghc-9.0.1
#  nixpkgs.mkShell {
#    inputsFrom = [
#      (nixpkgs.haskell.packages.ghc901.callCabal2nix "blucontrol" ./. {}).env
#    ];
#  }
