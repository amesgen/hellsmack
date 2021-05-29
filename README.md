# hellsmack

![CI](https://github.com/amesgen/hellsmack/workflows/CI/badge.svg)

Some Minecraft stuff as a CLI:

 - No GUI
 - uNiX pHiLoSoPhY
 - Does work offline if all necessary have been downloaded previously
 - Launch Minecraft
    - client and server
    - works at least for recent-ish versions (>= 1.7)
    - automatic Forge installation (in particular for > 1.12)
    - automatic Fabric installation
 - Manage CurseForge mods
    - install
    - update
    - deduplicate
 - Install and update CurseForge modpacks

## Usage

### Launching

Launch Vanilla MC:
```bash
hellsmack launch client 1.16.5 /path/to/game/dir
```

Launch MC 1.12.2 with latest Forge:
```bash
hellsmack launch client 1.12.2 -f latest /path/to/game/dir
```
One can also use `recommended`.

Launch MC 1.16.5 with specific Forge version (see [Forge download page](https://files.minecraftforge.net/)):
```bash
hellsmack launch client 1.16.5 -f 35.1.13 /path/to/game/dir
```

Similarly, you can use a specific [Fabric](https://fabricmc.net/) version:
```bash
hellsmack launch client 1.16.5 --fabric 0.11.3 /path/to/game/dir
```
Again, you can use `recommended` or `latest` to select the latest stable or latest version, resp.

In each case, one can use `server` instead of `client`.

#### Authentication

By default, the client is launched in offline mode. To log in with a Mojang account, use
```bash
hellsmack auth login ibims@gugel.com correcthorsebatterystaple
```
and then use the flag `-a`/`--authenticate`:
```bash
hellsmack launch -a client 1.16.5 /path/to/game/dir
```

### CurseForge

#### Mods

Search for "openc" in MC 1.12.2 on CurseForge and install by selecting e.g. OpenComputers:
```bash
hellsmack curse mods search-install 1.12.2 openc
```

Install OpenComputers-MC1.12.2-1.7.5.192 (https://www.curseforge.com/minecraft/mc-mods/opencomputers/files/2828357) via its file ID (also accepts multiple arguments):
```bash
hellsmack curse mods install 2828357
```

Interactively update mods in current directory (has multiple options):
```bash
hellsmack curse mods update .
```

Deduplicate different versions of the same mod in the current directory:
```bash
hellsmack curse mods deduplicate .
```

#### Modpacks

Search and install a client modpack (has multiple filtering options) into the current directory:
```bash
hellsmack curse modpacks search-install client "all the mods"
```

Install a modpack (https://www.curseforge.com/minecraft/modpacks/all-the-mods-6/files/3145702) by file ID into the current directory:
```bash
hellsmack curse modpacks install client 3145702
```

In each case, one can use `server` instead of `client` (if a server pack is available).

To update a modpack:

 1. Backup your saves and customizations in `options.txt`, `config/` etc.
 2. Remove all folders except `mods`.
 3. Install the new modpack version like above (this will take care of only downloading new/changed mods in the `mods` folder).
 4. Reapply your customizations.

## Installation

Pre-built binaries can be found on the [release page](https://github.com/amesgen/hellsmack/releases). In particular:

 - [Linux x86_64 (fully static)](https://github.com/amesgen/hellsmack/releases/latest/download/hellsmack-Linux)
 - [macOS x86_64](https://github.com/amesgen/hellsmack/releases/latest/download/hellsmack-macOS)
 - [Windows x86_64](https://github.com/amesgen/hellsmack/releases/latest/download/hellsmack-Windows)

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

