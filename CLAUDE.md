# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Personal dotfiles repository (fork of Hashrocket dotmatrix). It symlinks configuration files into `$HOME` and provides a personal command toolkit called `jd`.

## Installation

```bash
bin/install          # Link dotfiles, copy .local files, prune dead links; safe to re-run
bin/install -f       # Also replace symlinks that point somewhere else
bin/uninstall        # Remove all symlinks that point into this repo
bin/upgrade          # git pull --rebase, then bin/install
```

`bin/install`:
1. Copies `*.local` at the repo root to `~/.<name>` if absent (never overwrites)
2. Symlinks each entry from `bin/file_list.sh` → `~/.<entry>`: everything in `dotfiles/` (linked whole), plus each file under `config/` (skipping dot-paths), so `config/bat/config` → `~/.config/bat/config`. A `FILES` file at the repo root, if present, replaces this list. `bin/uninstall` uses the same list.
3. Removes symlinks in `~` and `~/.config` that point at files no longer in the repo

Real files are never replaced, even with `-f`. Run the scripts from the repo root.

## Architecture

### dotfiles/
Configuration files that get symlinked into `$HOME` with a `.` prefix.

- **`emacs.d/`** — Emacs config (relies on built-in `use-package`, so 29+; currently run on 31). See below.
- **`zshenv`, `zprofile`, `zshrc`, `bashrc`, `bash_profile`, `sharedrc`, `path`** — Shell config. See "Shell startup" below.
- **`zsh/functions/`** — Vendored zsh completions (added to `fpath` in `zshrc`). Only for tools that can't generate their own; those that can are listed in `jd/libexec/regen-zsh-completions`, which writes them to `~/.cache/zsh/completions` (also on `fpath`) and runs as part of `jd update`.
- **`gitconfig`** — Git aliases and settings; includes `~/.gitconfig.local` for identity. `cvsignore` is its global excludes file.

### config/
Files symlinked into `~/.config/`: `bat/config`, `ghostty/config`, `starship.toml`.

### jd/
Personal command toolkit, dispatched by [zub](https://github.com/jgdavey/zub) (installed via mise) using `jd/zub.yml`. `jd/bin/jd` is a thin wrapper around `zub`.

- Each command is a script at `jd/libexec/<name>`; subcommands live in subdirectories (e.g. `jd/libexec/git/sync-all` → `jd git sync-all`).
- Most scripts declare `#@ summary:` and `#@ usage:` comments (`;;@` in babashka scripts), which zub uses for help; `atomically` uses a [usage](https://usage.jdx.dev) spec instead.
- `jd new <command>` generates a new command.
- Shell integration comes from `jd init - <shell>`, evaluated in `sharedrc`. It finds the repo by following the `~/.sharedrc` symlink, so the checkout can live anywhere. It needs `zub` on PATH, so mise must be activated before `sharedrc` is sourced.
- Completions: `jd/completions/`.

## Shell startup

PATH ordering is fragile on macOS: `/etc/zprofile` (zsh) and `/etc/profile` (bash) run `path_helper` in login shells, which moves the system dirs ahead of anything set earlier. Every new terminal tab is a login shell, as is the shell `exec-path-from-shell` starts for Emacs.

- **`path`** — Sets PATH; *moves* its dirs to the front even if already present, so it can be re-sourced after `path_helper`. Shared by bash and zsh.
- **`zshenv`** (all zsh shells) — Sources `~/.path`. Non-interactive shells load mise with `mise env` (not `mise activate`, which also runs `compinit`).
- **`zprofile`** (zsh login shells, after `/etc/zprofile`) — Re-sources `~/.path`, and `mise env` again for non-interactive shells.
- **`zshrc`** (interactive zsh) — Runs `compinit` once, *then* `mise activate zsh` (mise skips its own `compinit` when `compdef` exists), then `sharedrc`.
- **`bash_profile` → `bashrc`** — `mise activate bash`, then `sharedrc`.
- **`sharedrc`** — Aliases, `jd init`, `EDITOR`. Shared by bash and zsh.

mise should run once per shell. `mise env` calls set `MISE_ACTIVATE_AGGRESSIVE=1` so mise's paths land ahead of Homebrew's in nested shells. Machine-specific settings go in `~/.zshenv.local`, `~/.zprofile.local`, `~/.zshrc.local` and `~/.bashrc.local` (not in the repo).

To check a change, compare a fresh login shell with a nested one, e.g. `env -i HOME=$HOME USER=$USER TERM=xterm /bin/zsh -lc 'whence java jq openssl'` vs `zsh -c 'whence java jq openssl'`.

## Emacs

`init.el` sets up packages and the core editing/completion stack (Vertico, Consult, Orderless, Marginalia, Embark, Corfu), then `lib/layers.el` loads every file in `layers/`, plus `local-layers.el`, `local.el` and `custom.el` if present. Each layer is a standalone elisp file for one domain:

| Layer | Purpose |
|-------|---------|
| `prog.el` | LSP, flycheck, dumb-jump, assorted language modes, mise |
| `clojure.el` | clojure-mode/clojure-ts-mode, CIDER, inf-clojure, clj-kondo, jet |
| `git.el` / `forge.el` | Magit / Forge (GitHub) |
| `buffers.el` | buffer-move, winner, comint |
| `org.el` | Org-mode and babel |
| `agents.el` | Claude Code IDE integration (`C-c C-'`) |
| `evil.el` | Evil mode (vim keybindings) |

Other layers: `gui`, `javascript`, `misc`, `ruby`, `rust`, `sql`, `writing`.

## Dependencies

Managed via `Brewfile` at the repo root. Many language tools (Java, Node, Leiningen, clj-kondo, clojure-lsp, zub, etc.) come from mise rather than Homebrew.
