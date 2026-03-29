# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Personal dotfiles repository (fork of Hashrocket dotmatrix). It symlinks configuration files into `$HOME` and installs a personal command toolkit called `jd`.

## Installation

```bash
bin/install          # Link dotfiles, copy .local files, install jd; safe to re-run
bin/install -f       # Force overwrite externally-managed files
bin/uninstall        # Remove all managed symlinks
```

`bin/install` does three things:
1. Symlinks `dotfiles/<name>` → `~/.<name>` for each file in `bin/file_list.sh`
2. Symlinks `config/<path>` → `~/.config/<path>` for all files under `config/`
3. Runs `jd autoinstall` to set up the `jd` toolkit

## Architecture

### dotfiles/
Configuration files that get symlinked into `$HOME` with a `.` prefix.

- **`emacs.d/`** — Emacs 29+ config. `init.el` bootstraps package management and loads `layers/*.el`. Each layer is a standalone elisp file handling one domain (clojure, ruby, git, org, etc.).
- **`zsh/`** and **`sharedrc`** — Shell config; `sharedrc` is sourced by both bash and zsh.
- **`gitconfig`** — Git aliases and settings; 30+ aliases including `git tree`, `git bs` (branch status).

### config/
Files symlinked into `~/.config/`. Contains starship prompt config, bat config, clj-kondo linting rules, and LSP settings.

### jd/
Personal command toolkit dispatched via `jd <command>`. Each command is a script in `jd/libexec/jd-<name>`. Install with `jd autoinstall` (adds `jd/bin` to PATH via shell config).

Notable commands: `jd git-sync-all`, `jd clone`, `jd mux`, `jd new`, `jd battery`.

## Emacs Layers

The Emacs config uses a layered approach — `init.el` loads each file from `dotfiles/emacs.d/layers/`:

| Layer | Purpose |
|-------|---------|
| `prog.el` | LSP, tree-sitter, flycheck, common programming setup |
| `clojure.el` | CIDER, clj-kondo, babashka |
| `git.el` | Magit, Forge (GitHub) |
| `buffers.el` | Vertico, Consult, Embark, Corfu completion |
| `org.el` | Org-mode configuration |
| `agents.el` | Claude Code IDE integration (`C-c C-'`) |
| `evil.el` | Evil mode (vim keybindings) |

## Dependencies

Managed via `Brewfile` at the repo root. Key taps: `d12frosted/emacs-plus`, `borkdude/brew`, `clojure-lsp/brew`.
