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

For an installation without a GUI check, the RStudio console equivalent is:

```r
source("dev/jamovi.R")
jamovi_dev_main("install", normalizePath("."))
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

Both `ui` and `test` first require the source-only menu-order, analysis-identity,
and icon checks in `dev/jamovi/tests/test-presentation.R` to pass. These checks
are separate from installed-package tests because the jamovi development
definitions are intentionally excluded from the R source package.

`ui` compiles into a temporary directory and runs the existing Linear Model and
GLM UI-handler tests against the installed jamovi runtime. It does not launch,
install into, or restart jamovi. `test` also does not install anything. GUI
inspection is still needed for layout, focus, drag-and-drop and full saved-file
round trips; handler/R6 tests alone do not establish these behaviors.

## Preconfigured smoke document

The default file is `.jamovi-dev/smoke.omv`, generated with eight analyses and
nine plot images. The original four cases were opened in jamovi; the newly added
GLM comparison is verified by automated saved-option restoration and still
requires a GUI smoke pass. Its fixed synthetic dataset
has 120 rows, numeric `x` and `z`, a two-level `group`, and three outcomes.
The analyses appear in this order:

| Case | Model | Layout / things to check |
|---|---|---|
| Linear Model, main effects | `continuous ~ x + group` | Colored group curves, means/contrasts and R² |
| Linear Model, interaction | `continuous ~ x + group + z + x:group` | Group panels, interaction term and fitted slopes |
| GLM, logistic interaction | `binary ~ x + group + z + x:group` | Logistic overlay, probability scale, IRT and optional p-values |
| GLM, Poisson | `count ~ x + group + z` | Response counts, multipliers, uncertainty; binomial controls hidden |
| GLM, logistic comparison | Interaction model versus `binary ~ x + group + z` | Two response-probability curves, descriptive observed proportions, shared population, likelihood ratio and BIC Bayes factors |
| Mixed, random intercept | `mixed ~ x + z + (1 | id)` | Three cluster panels; original-model conditional predictions and fixed/random estimates |
| Mixed, slope and quadratic | `mixed ~ x + z + I(x^2) + (0 + x | id)` | ML, intercept-free slopes, population mode and retained squared term |
| Mixed, interaction | `mixed ~ x + z + group + x:group + (1 + x | id)` | Separate group colors, three selected clusters and conditional predictions |

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

`install --open` installs the module, launches jamovi if necessary, and opens a
preconfigured smoke-test document. `open` opens that document without installing
the module. Both commands finish normally once the document window appears;
there is no prompt, terminal-session requirement, or exit-time document cleanup.
Leave the document open for manual inspection.

Each launch copies the source `.omv` to a uniquely named `flexplot-test-…` file
under `.jamovi-dev/open-sessions/`. Before opening another copy, the launcher
closes only the previously registered disposable document. Preexisting and
unregistered documents are never closed, and jamovi itself is never quit.
The ordinary `open -a` mechanism launches or reuses the application; `-n` is not
used. Save useful cases to a separate golden filename before replacing a test
copy, because disposable changes are discarded during cleanup.

A short-lived operation lock prevents concurrent installations, openings, and
cleanup commands. The persistent document registry is separate from this lock:
a document remaining open does not block the next installation or opening.
`ui`, `test`, and `fixture` do not open GUI windows.

To close the registered test document without opening another:

```sh
Rscript --vanilla dev/jamovi.R cleanup
```

Window verification and cleanup use macOS System Events and require the calling
terminal's Automation/Accessibility permission. The script does not change
permissions. If window access or cleanup fails, the command exits with failure,
retains the registry, and does not close unrelated documents. A failed cleanup
blocks replacement so repeated attempts cannot accumulate disposable windows.
A launch that succeeds but cannot be verified also retains its registration for
later cleanup. If jamovi is not running, cleanup simply clears the stale record.

The command lock is removed on ordinary errors and interrupts. After forced
termination, verify that no development command is running before removing a
leftover `.jamovi-dev/operation-lock` directory. Do not remove the document
registry as a substitute for closing a disposable window.

Running/stopped-app scenarios, install/open/cleanup control flow, permission
errors, concurrent commands, and preservation of user documents are verified
with mocked OS APIs, including execution of the actual window-helper script.
This repair did not launch additional GUI windows or reinstall the module;
actual macOS permissions and save-dialog behavior remain host-dependent.

For a GUI smoke pass, verify all eight populated analyses, model-term retention,
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
You can remove old disposable copies after closing their windows. Managed cleanup
never overwrites the original `.omv` or closes unregistered documents.

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

### Package tests versus development integration tests

`tests/testthat/` is shipped in the R source package. Its tests must use only
installed flexplot resources (`system.file()`), packaged test assets, temporary
files, and available declared R dependencies. Keep numerical R6, prediction,
reporting, and serialization tests that use only declared dependencies here. Do not discover dependencies by
adding an absolute jamovi application library or a local development module to
`.libPaths()`. Tests requiring jamovi's optional RProtoBuf protocol backend belong in development
validation too; both commands explicitly supply that existing host library without
adding it as a flexplot dependency.

Put UI compilation, YAML/JavaScript inspection, native installed-renderer tests,
and source-to-R6 integration tests in `dev/jamovi/tests/`. JavaScript integration
helpers live in its `js/` subdirectory. These files deliberately require the
checkout, `jmvtools`, Node, and the installed jamovi UI runtime. Both commands run
all of them, with missing prerequisites or skipped integration tests treated as
failures:

```sh
Rscript --vanilla dev/jamovi.R ui
Rscript --vanilla dev/jamovi.R test
```

Both commands compile all four analyses and validate citations before running
the source integration tests. `test` then runs the packaged jamovi R6 tests too.
Neither command installs a module or opens a GUI window.

The integration suite preserves the source-dependent assertions formerly mixed
into packaged tests:

| Integration coverage | Development test file |
| --- | --- |
| LM initialization and independent terms | `test-jamovi-glm-initialization.R` |
| LM polynomial handlers and fitted formulas | `test-jamovi-lm-quadratic.R` |
| GLM compiled headers and additive fitting | `test-jamovi-generalized.R` |
| GLM restoration, independent interactions/quadratics, repeated live edits | `test-jamovi-glm-restoration.R` |
| Mixed Model compilation and native server option batches | `test-jamovi-mixed.R` |
| Model-formula paragraph rendering | `test-jamovi-glm-comparison.R` |
| Menu identities and icons | `test-presentation.R` |

`dev/jamovi/test-boundaries.R` statically inspects every packaged R test/helper
and auxiliary JavaScript file. It rejects standard path constructions that reach
excluded resources (using `.Rbuildignore`), paths escaping the packaged test tree,
absolute checkout/host paths, raw checkout R sources, and the development UI
compiler, and undeclared test dependencies. Its regression tests include rejected examples and legitimate packaged
QML, SVG, temporary-file, and pure-JavaScript resources. This is a tripwire for
common dependency patterns, not a proof about arbitrary dynamically constructed
paths; an isolated built-package check remains required. Diagnostic messages
mentioning developer commands are allowed because they do not access files.

The old `a_testJASP_create_linmod.R` was an inactive fixture-loading script with
no assertions. It now lives at `dev/jasp/inspect-linmod-fixture.R`; it is not an
installed-package test or a substitute for JASP integration testing.

The boundary audit covered all remaining packaged R test/helper files, including
nonstandard filenames. Eleven absolute application-library fallbacks across eight
test files were removed. The eleven associated host-protocol tests and their
encoder now live in mandatory development validation; pure numerical assertions
and shared R6 fixtures remain packaged. Installed datasets and QML are read through
`system.file()`, SVG references remain packaged test assets, and isolated-process
prediction tests use temporary input/output files. No packaged test requires a
previously installed flexplot development module.

The host-protocol coverage includes Flexplot standard-error output and two-panel
restoration; independent GLM and LM polynomial saved options; CAS image-error
serialization; mixed comparison restoration, p-column visibility and ghost notes;
Model 2 table visibility; dynamic plot labels; and saved term-builder defaults.
These eleven tests retain all 199 original assertions. Together with the seven
UI/source tests, all 597 relocated assertions are mandatory in both commands.

### Modeling result specifications and ordering

LM and GLM render a top-level `specification` Html result from the fitted model
objects, including both formulas during comparison. GLM also identifies the
fitted family/link and binomial success event. Mixed Models retains its existing
single/comparison specification results, including random terms and estimation
methods; only one specification is visible at a time.

The declared sequence is specification and fitting warnings, main image and its
existing explanation, estimates, comparison image and its existing explanation,
comparison statistics, then optional LM residual diagnostics. Mixed Models still
uses its existing mutually exclusive standalone/comparison visualization modes.
Result names, options, table schemas, image sizes and plot calculations are
preserved. Only the redundant LM/GLM comparison-table formula notes have moved
to the top-level specification.

Native `Group$fromProtoBuf()` restores elements by name, and the installed
results renderer uses named result paths for annotation addresses. Reordering
therefore preserves the existing result and annotation identities. Dynamic
tables are rebuilt on recalculation. Cached saved output may retain its previous
presentation until recalculated; this change does not rewrite saved documents
or introduce a migration framework. The focused presentation tests exercise
restored options, reordered cached records and the installed group/Html rendering
methods, including preservation of annotation addresses.
