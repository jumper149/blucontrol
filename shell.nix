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
