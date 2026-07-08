# Emacs config (Doom-inspired, Elpaca)

Parallel trial config: Elpaca + vendored Doom macro layer. Lives entirely in
this directory, launched with `emacs --init-directory ~/.config/emacs-lab`.
The directory name is incidental; never leak it (or any invented project
name) into code or prose.

## Hard boundaries

- NEVER touch `~/.emacs.d` (live Doom install) or `~/.doom.d` (live Doom
  config) - both belong to the still-active daily driver.
- NEVER load lab files into a running Doom session: same-named macros and
  remapped dir variables will corrupt it. Verify via the smoke-boot pattern
  below.

## Naming: plain names, no invented prefixes

- Never name functions/variables with Doom's `+prefix` or `+module/fn`
  conventions (`+corfu-quit`, `+default/search-project`). That style marks
  "user config vs package" for novices; it is noise here.
- No made-up namespace prefixes either (`lab-`, `my-`, project names). This
  is a personal config; the global namespace is fine.
- Plain descriptive names: `search-in-project`, `alternate-buffer`,
  `window-cleanup+`. A trailing `+` is the local idiom for "enhanced variant
  of an existing function" - keep it, never a leading one. Hook functions
  keep the `-h` suffix, advice `-a`.
- When porting Doom-named functions, rename them and update every reference
  (bindings tree, `consult-customize` lists, hooks, advice). Files vendored
  from Doom itself (see below) are exempt until their planned rename sweep.

## Nothing writes inside this directory

- All caches/state/downloads live outside: `doom-cache-dir`
  (~/.cache/emacs-lab/), `doom-data-dir`, `doom-state-dir`,
  `doom-local-dir` (~/.local/share/emacs-lab/ - elpaca repos/builds, eln via
  early-init redirect).
- New package writing into `user-emacs-directory`? Redirect its path variable
  in the quarantine section of `lisp/doom-compat.el`.
- `.gitignore` is a whitelist: new top-level tracked entries need a `!/name`
  line; state files can never sneak in.

## Module layout

`modules/NAME/`:
- `config.el` - settings, hooks, keybindings, `use-package` blocks
- `autoload/*.el` - functions, loaded lazily via generated loaddefs
  (`lab--generate-loaddefs` in init.el; regenerates in a batch subprocess on
  mtime change). Files may `require` packages freely - they only load in full
  on first call.
- `autoload.el` (single file) - loaded eagerly; must be load-safe.
- No `packages.el`: recipes go inline via `:ensure (name :host ... :repo ...)`.
  EmacsWiki/codeberg packages need explicit recipes (no MELPA entry or
  unreliable host - use emacsmirror).
- New module: add to `active-modules` in init.el (explicit order, never
  globs) and, if guarded by `modulep!`, to `doom-modules-enabled`.

## Doom compat layer

- Use the vendored macros: `map!`, `after!`, `add-hook!`, `defadvice!`,
  `cmd!`, `letf!`, `setq-hook!`, plus `doom-first-{input,file,buffer}-hook`,
  `:after-call`, `:defer-incrementally`. Doom's AGENTS.md conventions for
  these macros apply here too.
- `elpaca (pkg :wait t)` only for init-time needs (general, evil). Everything
  else defers via hooks/autoloads.
- Near-verbatim vendored files (keep diffs minimal, note deviations in the
  header): `lisp/doom-compat.el`, `lisp/doom-keybinds.el`,
  `modules/bindings/config.el`.
- User layering mirrors Doom: modules load in `active-modules` order, root
  `config.el` loads last and always wins.

## Verification

- Smoke boot: write checks to a /tmp elisp file, run
  `TERM=xterm-256color emacs -nw --init-directory ~/.config/emacs-lab -l
  /tmp/check.el` in a kitty tab; the file writes a marker on
  `elpaca-after-init-hook` and `kill-emacs`. Always capture `*Warnings*` and
  elpaca statuses (`(elpaca--queued)` is an alist of `(ID . E)`; status via
  `elpaca<-status`) - no `ignore-errors` around checks.
- `check-parens` every edited file (`eca--check-parens-file`).
- MIGRATION.org tracks the porting plan; update it as modules land.
