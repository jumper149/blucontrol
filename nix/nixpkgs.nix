let
  commit = "fd9984fd9a950686e7271ecf01893987a42cdf14";

  # !!! Requires change, when the commit is changed !!!
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  hash = "1rx155w12nlmhfjhw4z1fxg0ph30cjdfx0gr9qzvp9jqfcljqih8";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = hash;
  };
in
  import nixpkgs
