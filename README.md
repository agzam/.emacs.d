# .emacs.d

Vanilla Emacs, [Elpaca](https://github.com/progfolio/elpaca) for packages, and the parts of [Doom](https://github.com/doomemacs/doomemacs) I actually missed.

## It started as an experiment

I ran Doom for years and [it served me well](https://github.com/agzam/.doom.d). Around summer of 2026 I started a side config to see whether I could live without it - not a rewrite, a trial. The bet was that I could keep Doom's macros and its `SPC` leader tree, drop everything else, and not hate my life. I gave it decent odds of failing, so I never wrote a README for it.

It stopped being a trial about a month later. This is the config I use every day now.

The whole port is logged in `MIGRATION.org` - every module, every decision, every thing that broke and why. It is long and it is not for you, but if you are doing the same migration it is the most useful file here.

## Why leave Doom

Mostly Elpaca.

Doom builds on straight.el plus a CLI. You edit `init.el`, run `doom sync`, restart Emacs. That loop is fine right up until you write your own packages. I maintain a bunch, and every small edit meant the sync dance.

Elpaca changes the shape of that:

- It installs and builds in parallel, in the background, while Emacs is already usable.
- It updates packages inside the running Emacs. Usually no restart.
- The recipe lives inline in the `use-package` block, so a package and its config sit in one place. No `packages.el` on the side.
- It builds local checkouts in place. A hook in `init.el` points my own packages at their checkouts under `~/GitHub/`; if the folder is there, Elpaca builds straight out of it - edit, `elpaca-rebuild`, done. If it is not there, it gets cloned first.

The other half of the reason is smaller but it adds up: there is no `$DOOMDIR` versus the Doom repo anymore. The config is the repo. And when something breaks I debug my code, not a framework's idea of my code. Three files of Doom are vendored here, I have read all of them, and the rest is mine.

## Why you might want to stay with Doom

Fair reasons, and I would not argue with any of them:

- It works on day one. Curated package combos that someone else has already tested together.
- Pinned versions. Here Elpaca tracks whatever upstream HEAD gives you - fresher, and occasionally that is your evening gone.
- `doom doctor`, `doom upgrade`, real docs, a module index, and a community that hits your bug before you do.
- Language modules you would otherwise wire by hand. I ported the ones I use; Doom ships dozens.
- The port itself costs real time. If Doom already fits you, all of this work buys you exactly nothing new. You just move the integration burden from Henrik onto yourself.

Go with Doom if you want a working editor. Go this way if you want to own every line.

## What I kept from Doom

Three near-verbatim files under `lisp/`, MIT-licensed, attribution kept in their headers:

- `doom-compat.el` - the macro layer: `map!`, `after!`, `add-hook!`, `defadvice!`, `cmd!`, `letf!`, `setq-hook!`, `modulep!`, the `use-package` extensions (`:defer-incrementally`, `:after-call`), and the `doom-first-{input,file,buffer}-hook` family.
- `doom-keybinds.el` - the leader machinery. `SPC` leader, `,` localleader, which-key labels.
- `doom-defaults.el` - the editor and UI baseline, `doom/escape`, `MODE-local-vars-hook`, built-in package setup.

Plus two ideas that were worth more than the code:

- The module layout. `modules/NAME/config.el` with lazy `autoload/*.el` beside it, loaded in an explicit order, and a user `config.el` that loads last and always wins.
- The startup tricks in `early-init.el` - GC deferral, the `file-name-handler-alist` dance, frame setup before the first draw.

## What is gone

- The `doom` CLI. No sync, no upgrade, no doctor, no env.
- The module system. `modulep!` survives as a shim so the vendored bindings still prune themselves, but a module here is just a folder listed in `active-modules`. No flags, no dependency resolution.
- Version pinning.
- Doom's docs, popup rules, dashboard, the `+flag` universe, and most of the module tree I never enabled.
- `$DOOMDIR`. One repo now.

## Layout

```
early-init.el    startup knobs: GC, file handlers, eln cache redirect
init.el          elpaca bootstrap, module list, load order
config.el        my layer - loads last, wins
custom.el        customize's scratchpad (never tracked)
lisp/            vendored Doom slice + small standalone helpers
modules/NAME/    config.el + autoload/*.el (lazy, via generated loaddefs)
tests/           buttercup suites, mirrors the source tree; tests/e2e/ drives real keys
scripts/         batch entry points behind the tasks
snippets/        yasnippet library
bb.edn           task runner
MIGRATION.org    the porting log
```

Load order: `early-init` -> Elpaca bootstrap -> `general` -> `doom-compat` -> `doom-keybinds` -> `doom-defaults` -> `lisp/functions` -> modules in their listed order -> `config.el` -> `custom.el`.

## Tasks

Linting, tests, a full boot in a pty, package updates and repairs all run through [babashka](https://babashka.org). `bb tasks` lists them with descriptions. CI runs the same ones on every push.


## Should you run this

Probably not as-is. It assumes macOS in places, wants my Hammerspoon setup, pulls a private repo or two, and is shaped around my hands. Read it, steal from it, ignore the rest. If you want a working Emacs today, use Doom - that is a completely reasonable choice, and this config exists because Doom made it easy to want more control, not less.
