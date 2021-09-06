let
  commit = "08ef0f28e3a41424b92ba1d203de64257a9fca6a";


  # !!! Requires change, when the commit is changed !!!
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  hash = "1mql1gp86bk6pfsrp0lcww6hw5civi6f8542d4nh356506jdxmcy";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = hash;
  };
in
  import nixpkgs
