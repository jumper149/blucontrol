{
  description = "Configurable blue light filter";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixpkgs-unstable";
    };
  };

  outputs = { self, nixpkgs }: {

    checks.x86_64-linux.blucontrol = self.packages.x86_64-linux.blucontrol;
    defaultPackage.x86_64-linux = self.packages.x86_64-linux.blucontrol;

    packages.x86_64-linux.blucontrol =
      with import nixpkgs { system = "x86_64-linux"; };
      (import ./nix/build.nix).blucontrolWrapped { inherit stdenv lib makeWrapper nix-gitignore; haskellPackages = haskell.packages.ghc963; };

    devShell.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      (import ./nix/build.nix).blucontrolShell { inherit rnix-lsp nix-gitignore; haskellPackages = haskell.packages.ghc963; };

  };
}
