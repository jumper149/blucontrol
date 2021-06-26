let
  nixpkgs = import ./nixpkgs.nix;
in
  {
    simpleShell = { ghc ? "ghc8104" }: nixpkgs.mkShell {
      inputsFrom = [
        (nixpkgs.haskell.packages."${ghc}".callCabal2nix "blucontrol" ../. {}).env
      ];
    };
  }
