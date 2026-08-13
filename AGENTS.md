# Emacs config (Doom-inspired, Elpaca)

The daily-driver config: Elpaca + vendored Doom macro layer. Lives at
`~/.emacs.d`, launched with plain `emacs`; runs the Emacs server on the
default socket (elisp-eval MCP, mxp and Hammerspoon drive plain
emacsclient). Machine state (packages, cache, state) lives under
`~/.emacs.d/.local/` (git-ignored). Never leak an invented project name
into code or prose.

## Hard boundaries

- THIS session is the live working environment now - all live-session
  hygiene (elisp-eval cleanup, no destructive tests in-session, probes in
  throwaway `--init-directory` instances) protects it.
- `~/.doom.d` and `~/.emacs.d` are the retired porting reference: consult
  them as text only. NEVER modify them, and NEVER load their files into
  this session - Doom's real module/dir plumbing would fight the vendored
  compat layer the same way lab files used to corrupt Doom. The Doom
  instance normally doesn't run - it stays a text-only reference.

## Naming: plain names, no invented prefixes

- Never name functions/variables with Doom's `+prefix` or `+module/fn`
  conventions (`+corfu-quit`, `+default/search-project`). That style marks
  "user config vs package" for novices; it is noise here.
- No made-up namespace prefixes either (`lab-`, `my-`, project names). This
  is a personal config; the global namespace is fine.
- Plain descriptive names: `search-in-project`, `alternate-buffer`,
  `window-cleanup`. Never any `+` prefix or suffix on symbols; when a name
  would shadow the package function it wraps, pick a distinct descriptive
  name (`dired-remove-subtree` wrapping `dired-subtree-remove`). Hook
  functions keep the `-h` suffix, advice `-a`.
- When porting Doom-named functions, rename them and update every reference
  (bindings tree, `consult-customize` lists, hooks, advice). Files vendored
  from Doom itself (see below) are exempt until their planned rename sweep.

## Nothing writes into the tracked tree

- All caches/state/downloads live under `.local/` (git-ignored, relative to
  the config dir): `doom-local-dir` (.local/ - elpaca repos/builds),
  `doom-cache-dir` (.local/.cache/ - eln via early-init redirect,
  tree-sitter grammars, generated loaddefs), `doom-data-dir` (.local/etc/),
  `doom-state-dir` (.local/state/).
- New package writing into `user-emacs-directory`? Redirect its path variable
  in the quarantine section of `lisp/doom-compat.el`.
- Neither `-Q` nor `--batch` relocates `user-emacs-directory` - they only skip
  loading init files, and they skip early-init.el with it, so the eln
  redirect never fires. Every unset default (eln cache, `auto-save-list`,
  transient history, `package-user-dir`) then resolves against the real
  config dir. Every `emacs` invocation anywhere - probes, harnesses, other
  repos' Makefiles - takes `--init-directory` pointing at a sandbox; ad-hoc
  probes use `emacs -Q --init-directory=$(mktemp -d)`.
- The config's own batch entry points do this via the `emacs-batch` vector in
  bb.edn (`--init-directory` plus an explicit `startup-redirect-eln-cache`,
  since `--init-directory` alone would put the eln cache at the root);
  tests/helper.el redirects again into the test sandbox.
- `bb check-root` is the tripwire: anything at the root that is neither
  git-tracked nor a sanctioned exception fails it.
- `.gitignore` is a conventional blacklist (.local/, custom.el, per-tool
  droppings, *.elc/*.eln litter); anything unexpected dumped into the tree
  shows up in `git status` instead of being silently ignored.
- Sanctioned exceptions: `custom.el` stays in the config dir (edited in
  place by choice, explicitly git-ignored); `modules/writing/abbrev_defs`
  is tracked AND written in place by abbrev (`C-x a i g` curates it).
  Everything else follows the rule.

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
- Local checkouts: a bare local path in `:repo` (no `:host`) builds in place
  from that dir - the elpaca way to consume packages being developed locally.
  The cons form `:repo ("~/path" . "name")` clones instead; avoid it. See
  MIGRATION "Local :repo packages".
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
  `lisp/doom-defaults.el`. (`modules/bindings/config.el` was the leader
  tree's vendored base; the migration-conclusion sweep made it the lab's own
  - edit it freely.)
- User layering mirrors Doom: modules load in `active-modules` order, root
  `config.el` loads last and always wins.

## Verification

- Canonical entry points (same locally and in CI, see
  `.github/workflows/ci.yml`): `bb lint` (check-parens over tracked elisp),
  `bb test` (buttercup suites in `tests/`), `bb check-root` (nothing leaked
  into the config root), `bb smoke` (full elpaca boot in a pty; verdict comes
  from the marker written by `scripts/smoke-check.el`), `bb e2e` (the same
  boot, then real keypresses over real buffers; marker from
  `scripts/e2e-check.el`). Both pty probes share one CI job - smoke first,
  since e2e means nothing if the config does not boot.
- Every module port adds or extends a suite in `tests/`, which mirrors the
  source tree: `tests/MODULE/FILE-tests.el` per module source file (the
  `autoload/` level is flattened), `lisp/` sources under `tests/lisp/`.
  Suites load `tests/helper.el` (located via `locate-dominating-file`),
  which sandboxes the XDG dirs before doom-compat derives its paths - tests
  never touch the real cache/state.
- A `with-temp-buffer` spec runs in a replica of a buffer, not in one: no
  major mode was turned on, nothing is fontified, and no keymap was
  consulted. Behaviour that depends on any of those - thingatpt providers,
  overlays, text properties, or which target/keymap reaches a command -
  cannot fail there, so a suite covering it proves nothing. Enable the modes
  and fontify in the fixture, and add the flow to `tests/e2e/`: a scenario
  file registering a function on `e2e-scenarios`, whose cases run through
  `e2e-act-case` (real file in its own mode, fontified, displayed, driven by
  real keypresses). Scenario files must not be named `*-tests.el` - that is
  buttercup's discovery pattern. Link conversion shipped broken twice
  because both fixes were verified only against the replica.
- Ad-hoc smoke checks: the /tmp elisp file + kitty tab pattern still applies
  (`TERM=xterm-256color emacs -nw --init-directory ~/.emacs.d -l
  /tmp/check.el`, marker on `elpaca-after-init-hook`, `kill-emacs`). Always
  capture `*Warnings*` and elpaca statuses (`(elpaca--queued)` is an alist of
  `(ID . E)`; status via `elpaca<-status`) - no `ignore-errors` around checks.
- `check-parens` every edited file (`eca--check-parens-file`).
- Deleting an `autoload/*.el` does NOT invalidate the generated loaddefs
  cache (the mtime check only sees newer files) - also remove
  `~/.emacs.d/.local/.cache/autoloads/NAME.el`.
- MIGRATION.org tracks the porting plan; update it as modules land.
