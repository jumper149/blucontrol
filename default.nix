let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.extend (
      self: super: {
        blumon = super.callCabal2nix "blumon" ./. {};
      }
    );
  };
in
  with import <nixpkgs> { overlays = [ overlay ]; };
  let
    blumonEnv' = packages: pkgs.haskellPackages.ghcWithPackages (self: [ self.blumon ] ++ packages self);
    blumonEnv = blumonEnv' (_: []);
  in
    stdenv.mkDerivation {
      name = "blumon-with-packages-${pkgs.haskellPackages.blumon.version}-${blumonEnv.version}";
      nativeBuildInputs = [ makeWrapper ];
      buildCommand = ''
        makeWrapper ${blumonEnv}/bin/blumon $out/bin/blumon \
          --prefix PATH : ${lib.makeBinPath [ blumonEnv ]}
      '';
      preferLocalBuild = true;
      allowSubstitues = false;
    }
