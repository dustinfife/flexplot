# Local jamovi development on macOS

Run these commands from the flexplot checkout in Terminal. No package or app
restart is part of the fast test commands.

```sh
# About a second here: compile all definitions and bundles; exercise installed UI handlers.
Rscript --vanilla dev/jamovi.R ui

# About 48 seconds here: actual jamovi R6 analyses and rendering regression tests.
Rscript --vanilla dev/jamovi.R test

# Routine GUI iteration: isolated build/install, then a disposable preconfigured document.
Rscript --vanilla dev/jamovi.R install --open
```

From the RStudio console, the equivalent installation command is:

```r
source("dev/jamovi.R")
jamovi_dev_main(c("install", "--open"), normalizePath("."))
```

## What was verified locally

The installed versions were jamovi 2.7.41 and jmvtools 28.17. The supported
mechanism is **reinstalling the module while jamovi remains open**, not watching
R/YAML/JavaScript source files. jmvtools invokes the desktop's `--install`
command. The installed server restarts its R engines, installs the module, and
notifies clients about the update. It does not ask the desktop to quit.

Cached installations took about **21–35 seconds** in this checkout. The script
confirmed each installed build's unique `build-hash` in the installed module
metadata. A temporary, label-only build was used to inspect refresh behavior;
its changed dropdown label was observed in the GUI. The ordinary working build
was restored afterward. No probe edits were made to production source files.

**This is not a promise that every already-visible result has been recomputed.**
Saved `.omv` files contain cached results. Automatic replacement of a temporary
R-table label in an already-open document was not conclusively verified. An
experimental live update of an open options panel also left model terms empty
in one disposable test document. This is a GUI/integration finding, not evidence
that the underlying model calculations changed. No production fix was attempted
in this workflow task. A later extra probe was blocked by the tool's automatic
approval-review usage limit.

For predictable iteration, save important work, use `install --open`, and test
in its fresh disposable document. That command regenerates the default smoke
file with the **current** working-tree R6 calculations before opening it, so its
cached tables and images are current as well. Check that model terms remain
present when editing controls. If an active panel is stale, reopen a fresh copy
rather than recreating the analyses. A full app restart remains a fallback for
unresolved engine/UI caching; this script never force-quits jamovi.

The installed desktop also has development refresh shortcuts (Cmd+R for the
window; F9 for engines, subject to macOS function-key settings). They were
investigated, but are not automated or presented as a verified replacement for
installation and fresh-document checks.

## Choose the smallest useful check

| Change | First check | GUI check |
|---|---|---|
| R calculations or rendering callbacks | `test` | `install --open`; inspect model terms, estimates and curves |
| `.a.yaml` options | `ui`, then relevant R6 tests | `install --open`; verify defaults, selectors and saved options |
| `.u.yaml` layout or JavaScript controls | `ui` | `install --open`; exercise switching, reassignment and visibility |
| `.r.yaml` results tables | `ui` and `test` | `install --open`; inspect initial waiting state, columns, notes and errors |
| Full package regression check | `Rscript --vanilla -e 'devtools::test()'` | Separate from module installation |

`ui` compiles into a temporary directory and runs the existing Linear Model and
GLM UI-handler tests against the installed jamovi runtime. It does not launch,
install into, or restart jamovi. `test` also does not install anything. GUI
inspection is still needed for layout, focus, drag-and-drop and full saved-file
round trips; handler/R6 tests alone do not establish these behaviors.

## Preconfigured smoke document

The default file is `.jamovi-dev/smoke.omv`, generated on this Mac and actually
opened in jamovi with tables and four plot images. Its fixed synthetic dataset
has 120 rows, numeric `x` and `z`, a two-level `group`, and three outcomes.
The analyses appear in this order:

| Case | Model | Layout / things to check |
|---|---|---|
| Linear Model, main effects | `continuous ~ x + group` | Colored group curves, means/contrasts and R² |
| Linear Model, interaction | `continuous ~ x + group + z + x:group` | Group panels, interaction term and fitted slopes |
| GLM, logistic interaction | `binary ~ x + group + z + x:group` | Logistic overlay, probability scale, IRT and optional p-values |
| GLM, Poisson | `count ~ x + group + z` | Response counts, multipliers, uncertainty; binomial controls hidden |

The generator uses actual flexplot R6 analysis classes and plot callbacks,
jamovi's bundled `jmvReadWrite` and `RProtoBuf`, and jamovi's native option
serializer. It does not implement a second model-fitting or prediction engine.
The explicit option-serialization step matters: standalone jmvcore analyses can
serialize results without the host's option message. Such a file can display
saved results but lose its model specification upon a rerun.

```sh
# Make the initial file, or deliberately regenerate the reserved default fixture.
Rscript --vanilla dev/jamovi.R fixture
Rscript --vanilla dev/jamovi.R fixture --force

# Reuse the existing file without any build/install.
Rscript --vanilla dev/jamovi.R open

# Use your own GUI-saved golden file; the original is never overwritten.
Rscript --vanilla dev/jamovi.R open --file "/absolute/path/my-smoke.omv"
Rscript --vanilla dev/jamovi.R install --open --file "/absolute/path/my-smoke.omv"
```

Every open request copies the file to a unique name under
`.jamovi-dev/open-sessions/` and uses macOS's normal `open -a` document mechanism.
The installed desktop's file-open handler creates a new window; no close, quit
or replace-document commands are sent. Opening the generated fixture in the
already-running application was tested. New windows and the existing document
remained separate. Keep useful custom cases in your own golden `.omv`, rather
than saving over the reserved generated default.

For a GUI smoke pass, verify all four populated analyses, model-term retention,
plots, logistic/Poisson scales, p-value toggles, and switching between families.
The prepared file removes repeated setup, not the need to inspect the GUI.
The option-panel term-loss finding above should be treated as a failed smoke
check if it recurs, with the exact actions recorded for a separate fix.

## Installation safeguards and dependencies

The compiler writes generated headers, NAMESPACE additions, metadata and build
files. `install` copies source to a temporary staging directory first, preserving
all tracked and untracked working-tree edits. It uses the existing `build/`
dependency cache (or `.jamovi-dev/cache/`) and the installed compiler's
`--skip-deps --skip-remotes` flags by default. No new dependencies were added.

If dependencies genuinely changed, explicitly run:

```sh
Rscript --vanilla dev/jamovi.R install --deps --open
```

This permits the compiler's normal dependency installation and retains its cache
for subsequent quick builds. Missing dependencies otherwise produce a failed
command and the tail of `.jamovi-dev/install.log`; the script does not silently
fall back to downloading packages. Exit status 0 and a matching installed build
stamp mean installation was confirmed, not that a human GUI smoke pass succeeded.

Set `JAMOVI_HOME` if the application is elsewhere. The default is
`/Applications/jamovi.app`. Existing Node is discovered on PATH or under `~/.nvm`.
Fixture generation temporarily adds the application's bundled R libraries; it
does not install packages into your R environment. Its existing jmvcore theme
can emit an upstream `element_line(size=...)` deprecation warning; the local
`testthat` R-build-version warning is also unchanged.

Local logs, fixtures and disposable copies live in the git-ignored
`.jamovi-dev/`. Both `dev/` and `.jamovi-dev/` are excluded from R package builds.
You can remove old disposable copies after closing their windows. No script
cleans up an active document or overwrites a custom `.omv` automatically.

## Development-tool verification

```sh
# No install or GUI launch: source-generation isolation, error paths, safe file copies.
Rscript --vanilla dev/jamovi/check-workflow.R

# No GUI launch: restore all saved options through real R6 classes and check models.
Rscript --vanilla dev/jamovi/check-fixture.R
```

The fixture check compares restored interaction design matrices and GLM
coefficients against independent R models. The infrastructure check fingerprints
source headers and metadata before/after isolated generation. These are separate
from actual GUI observations and from the full package suite.

References: [jamovi Getting Started](https://dev.jamovi.org/tutorial/tuts0101-getting-started/)
and [Distributing Modules](https://dev.jamovi.org/tutorial/tuts0111-distributing-modules/).
Installed desktop/server/compiler source was also inspected to distinguish file
opening, module installation, engine refresh and saved-result caching.
