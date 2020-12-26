# hellsmack

![CI](https://github.com/amesgen/hellsmack/workflows/CI/badge.svg)

Some Minecraft stuff as a CLI:

 - No GUI
 - uNiX pHiLoSoPhY
 - Launch Minecraft
    - works at least for recent-ish versions (>= 1.7)
    - automatic Forge installation (in particular for > 1.12)
 - Manage CurseForge mods
    - install
    - update
 - Install CurseForge modpacks

## Usage

Use the help menu for now. 

## Installation

Fully static binaries for Linux x86_64 can be found [here](https://github.com/amesgen/hellsmack/releases) ([Latest release](https://github.com/amesgen/hellsmack/releases/latest/download/hellsmack-Linux)).

Bash/zsh/fish completions are also available, run one of
```bash
hellsmack --bash-completion-script /path/to/hellsmack
hellsmack --zsh-completion-script  /path/to/hellsmack
hellsmack --fish-completion-script /path/to/hellsmack
```

## Building

Install suitable versions of GHC and Cabal (e.g. via [ghcup](https://gitlab.haskell.org/haskell/ghcup-hs/)) and run (optionally set `STATIC_BUILD`)
```bash
bash build.sh
```
The output binary is in `artifacts`.

