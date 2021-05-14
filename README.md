# blucontrol

[![Hackage](https://img.shields.io/hackage/v/blucontrol.svg?style=for-the-badge)](http://hackage.haskell.org/package/blucontrol)
[![AUR](https://img.shields.io/aur/version/blucontrol.svg?style=for-the-badge)](https://aur.archlinux.org/packages/blucontrol)
[![Travis CI](https://img.shields.io/travis/com/jumper149/blucontrol?style=for-the-badge)](https://travis-ci.com/github/jumper149/blucontrol)
[![License](https://img.shields.io/github/license/jumper149/blucontrol?style=for-the-badge)](./LICENSE)

## Configuration

Read the [Haddock documentation](http://hackage.haskell.org/package/blucontrol-0.3.0.0/docs/Blucontrol.html) and use [Main.hs](./Main.hs) as a template.

## Install

### Nix

Install blucontrol with the nix package manager.

```bash
nix-env --install blucontrol-with-packages
```
Watch out: `haskellPackages.blucontrol` is just the library, but doesn't add GHC to the `PATH`.
You need `blucontrol-with-packages` to be able to compile your configuration.

You can also install it from the cloned repository.

```bash
git clone https://github.com/jumper149/blucontrol.git
cd blucontrol
nix-env --install --file dev/build.nix
```

### AUR

Build blucontrol with makepkg and install with pacman.
You might need to install some dependencies from the AUR.

```bash
git clone https://aur.archlinux.org/blucontrol.git
cd blucontrol
makepkg --syncdeps --install
```

### Cabal

Make sure to have dependencies that are not managed by cabal installed.
- libx11-dev
- libxrandr-dev

If you want to actually use blucontrol without installing I recommend editing `Main.hs` in the source tree instead of using `$XDG_CONFIG_HOME/blucontrol/blucontrol.hs`.
This way you avoid problems with finding libraries, since blucontrol invokes GHC to compile the configuration at `$XDG_CONFIG_HOME/blucontrol/blucontrol.hs`.

```bash
git clone https://github.com/jumper149/blucontrol.git
cd blucontrol
$EDITOR Main.hs
cabal run blucontrol -- --ignore-config
```

## Development

### Nix

Enter the nix-shell for development.

```bash
git clone https://github.com/jumper149/blucontrol.git
cd blucontrol
nix-shell dev/shell.nix
```
