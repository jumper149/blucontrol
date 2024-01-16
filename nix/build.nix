rec {
  blucontrol = { haskellPackages, nix-gitignore }:
    let src = nix-gitignore.gitignoreSource [] ../.;
    in haskellPackages.callCabal2nix "blucontrol" src {};

  blucontrolEnv = { haskellPackages, nix-gitignore, packages ? (_:[]) }:
    haskellPackages.ghcWithPackages (
      self: [
        (blucontrol { inherit haskellPackages nix-gitignore; })
      ] ++ packages self
    );

  blucontrolWrapped = { stdenv, lib, makeWrapper, nix-gitignore, haskellPackages, packages ? (_:[]) }:
    let blucontrolEnv' = blucontrolEnv { inherit haskellPackages nix-gitignore packages; };
    in stdenv.mkDerivation {
      name = "blucontrol-with-packages-${blucontrolEnv'.version}";
      nativeBuildInputs = [ makeWrapper ];
      buildCommand = ''
        makeWrapper ${blucontrolEnv'}/bin/blucontrol $out/bin/blucontrol \
          --prefix PATH : ${lib.makeBinPath [ blucontrolEnv' ]}
      '';
      preferLocalBuild = true;
      allowSubstitues = false;
    };

  blucontrolShell = { rnix-lsp, haskellPackages, nix-gitignore }:
    haskellPackages.shellFor {
      buildInputs = with haskellPackages; [
        cabal-install
        haskell-language-server
        hlint
        hnix
        implicit-hie
        rnix-lsp
      ];
      packages = haskellPackages: [
        (blucontrol { inherit haskellPackages nix-gitignore; })
      ];
      withHoogle = true;
    };

  blucontrolShellSimple = { haskellPackages, mkShell, nix-gitignore }:
    mkShell {
      inputsFrom = [
        (blucontrol { inherit haskellPackages nix-gitignore; }).env
      ];
    };
}
