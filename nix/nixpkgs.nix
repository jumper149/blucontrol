let
  commit = "e0629618b4b419a47e2c8a3cab223e2a7f3a8f97";


  # !!! Requires change, when the commit is changed !!!
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  hash = "sha256-loWkd7lUzSvGBU9xnva37iPB2rr5ulq1qBLT44KjzGA=";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = hash;
  };
in
  import nixpkgs
