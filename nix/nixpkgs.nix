let
  commit = "2489eb5e4516aab575ab114b7e0a3e1b5c5daca7";

  # !!! Requires change, when the commit is changed !!!
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  hash = "0snsicv0i50d0cp2lhjgjqi8qkhv37lx9dy3sg6jqnr5mllwbgad";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${commit}.tar.gz";
    sha256 = hash;
  };
in
  import nixpkgs
