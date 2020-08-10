let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.extend (
      self: super: {
        blucontrol = super.callCabal2nix "blucontrol" ./. {};
      }
    );
  };
in
  with import <nixpkgs> { overlays = [ overlay ]; };
  let
    blucontrolEnv' = packages: pkgs.haskellPackages.ghcWithPackages (self: [ self.blucontrol ] ++ packages self);
    blucontrolEnv = blucontrolEnv' (_: []);
  in
    stdenv.mkDerivation {
      name = "blucontrol-with-packages-${pkgs.haskellPackages.blucontrol.version}-${blucontrolEnv.version}";
      nativeBuildInputs = [ makeWrapper ];
      buildCommand = ''
        makeWrapper ${blucontrolEnv}/bin/blucontrol $out/bin/blucontrol \
          --prefix PATH : ${lib.makeBinPath [ blucontrolEnv ]}
      '';
      preferLocalBuild = true;
      allowSubstitues = false;
    }
