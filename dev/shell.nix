let
  nixpkgs = import ./nixpkgs.nix;
  blucontrol = haskellPackages: haskellPackages.callCabal2nix "blucontrol" ../. {};
in
  nixpkgs.haskellPackages.shellFor {
    buildInputs = with nixpkgs.haskellPackages; [
      cabal-install
      haskell-language-server
      hlint
      implicit-hie
    ];
    packages = haskellPackages: [
      (blucontrol haskellPackages)
    ];
    withHoogle = true;
  }
