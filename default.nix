let
  overlay = self: super: {
    haskellPackages = super.haskellPackages.extend (
      self: super: {
        bludigon = super.callCabal2nix "bludigon" ./. {};
      }
    );
  };
in
  with import <nixpkgs> { overlays = [ overlay ]; };
  let
    bludigonEnv' = packages: pkgs.haskellPackages.ghcWithPackages (self: [ self.bludigon ] ++ packages self);
    bludigonEnv = bludigonEnv' (_: []);
  in
    stdenv.mkDerivation {
      name = "bludigon-with-packages-${pkgs.haskellPackages.bludigon.version}-${bludigonEnv.version}";
      nativeBuildInputs = [ makeWrapper ];
      buildCommand = ''
        makeWrapper ${bludigonEnv}/bin/bludigon $out/bin/bludigon \
          --prefix PATH : ${lib.makeBinPath [ bludigonEnv ]}
      '';
      preferLocalBuild = true;
      allowSubstitues = false;
    }
