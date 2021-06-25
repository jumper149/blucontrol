{ stdenv, lib, makeWrapper, haskellPackages, packages ? (_:[]) }:
let
  blucontrolEnv = haskellPackages.ghcWithPackages (
    self: [
      (self.callCabal2nix "blucontrol" ./. {})
    ] ++ packages self
  );
in
  stdenv.mkDerivation {
    name = "blucontrol-with-packages-${blucontrolEnv.version}";
    nativeBuildInputs = [ makeWrapper ];
    buildCommand = ''
      makeWrapper ${blucontrolEnv}/bin/blucontrol $out/bin/blucontrol \
        --prefix PATH : ${lib.makeBinPath [ blucontrolEnv ]}
    '';
    preferLocalBuild = true;
    allowSubstitues = false;
  }
