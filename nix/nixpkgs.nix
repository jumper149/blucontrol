let
  commit = "0b8b127125e5271f5c8636680b6fe274844aaa9d";

  # !!! Requires change, when the commit is changed !!!
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  hash = "1rjb1q28ivaf20aqj3v60kzjyi5lqb3krag0k8wwjqch45ik2f86";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = hash;
  };
in
  import nixpkgs {
    overlays = [ (import ./overlay.nix) ];
  }
