# hellsmack

![CI](https://github.com/amesgen/hellsmack/workflows/CI/badge.svg)

Some Minecraft stuff as a CLI:

 - No GUI
 - uNiX pHiLoSoPhY
 - Launch Minecraft
    - client and server
    - works at least for recent-ish versions (>= 1.7)
    - automatic Forge installation (in particular for > 1.12)
 - Manage CurseForge mods
    - install
    - update
 - Install CurseForge modpacks

## Usage

### Launching

Launch Vanilla MC:
```bash
hellsmack launch client 1.16.4 /path/to/game/dir
```

Launch MC 1.12.2 with latest Forge:
```bash
hellsmack launch client 1.12.2 -f latest /path/to/game/dir
```
One can also use `recommended`.

Launch MC 1.16.4 with specific Forge version (see [Forge download page](https://files.minecraftforge.net/)):
```bash
hellsmack launch client 1.16.4 -f 35.1.13 /path/to/game/dir
```

In each case, one can use `server` instead of `client`.

#### Authentication

By default, the client is launched in offline mode. To log in with a Mojang account, use
```bash
hellsmack auth login ibims@gugel.com correcthorsebatterystaple
```
and then use the flag `-a`/`--authenticate`:
```bash
hellsmack launch -a client 1.16.4 /path/to/game/dir
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
hellsmack curse modpacks install client 3145702 .
```

In each case, one can use `server` instead of `client` (if a server pack is available).

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

