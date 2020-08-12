# blucontrol

[![Hackage](https://img.shields.io/hackage/v/blucontrol.svg?style=for-the-badge)](http://hackage.haskell.org/package/blucontrol)
[![AUR](https://img.shields.io/aur/version/blucontrol.svg?style=for-the-badge)](https://aur.archlinux.org/packages/blucontrol)
[![Travis CI](https://img.shields.io/travis/com/jumper149/blucontrol?style=for-the-badge)](https://travis-ci.com/github/jumper149/blucontrol)
[![License](https://img.shields.io/github/license/jumper149/blucontrol?style=for-the-badge)](./LICENSE)

## Install

### Nix

Install blucontrol with the nix package manager.

```bash
git clone https://github.com/jumper149/blucontrol.git
cd blucontrol
nix-env -i -f default.nix
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
The reason for this is to avoid problems with finding libraries.

```bash
git clone https://github.com/jumper149/blucontrol.git
cd blucontrol
$EDITOR Main.hs
cabal v2-run blucontrol
```

## Development

### Nix

Enter the nix-shell for development.

```bash
git clone https://github.com/jumper149/blucontrol.git
cd blucontrol
nix-shell dev/shell.nix
```
