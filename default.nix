{ stdenv, makeWrapper, makeBinPath, ghcWithPackages, packages ? (_:[]) }:
let
  blucontrolEnv = ghcWithPackages (
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
        --prefix PATH : ${makeBinPath [ blucontrolEnv ]}
    '';
    preferLocalBuild = true;
    allowSubstitues = false;
  }
