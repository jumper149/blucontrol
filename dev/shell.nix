let
  nixpkgs = import ./nixpkgs.nix;
  haskellPackages = nixpkgs.haskell.packages.ghc8104;
  blucontrol = p: p.callCabal2nix "blucontrol" ../. {};
in
  haskellPackages.shellFor {
    buildInputs = with haskellPackages; [
      cabal-install
      haskell-language-server
      hlint
      implicit-hie
    ];
    packages = p: [
      (blucontrol p)
    ];
    withHoogle = true;
  }
