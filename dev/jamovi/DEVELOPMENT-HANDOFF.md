# jamovi modeling development handoff

Development branch: `ggplot-objects` in <https://github.com/dustinfife/flexplot>.
Continue on this branch; do not force-push or merge it into another branch as part
of a focused consistency stage. Ordinary Flexplot passed the user's recent manual
inspection. Keep stages small and preserve statistical behavior.

## Roadmap and completed work

| Stage | Work | Status / commit |
| --- | --- | --- |
| 1 | Model formulas and result ordering | Complete: `6d728b06` |
| 2 | Comparison visualization restrictions | Complete: `b14032b9` |
| 3 | Consistent Output Options | Complete: `c36f4b06` |
| 4 | Independent GLM visualization settings | Complete; implementation commit resolved below |
| 5 | Plot-control organization, Modify Plot Labels checkboxes, taller editors, readable dynamic legend descriptions | Remaining |
| 6 | Mixed Models estimates, confidence-interval controls, removal of permanently empty fixed-effect p-value columns | Remaining |
| 7 | Compact table notes, targeted native footnotes, conditional comparison-test explanations | Remaining |
| 8 | Ghost-line reference explanations and reduced plot-to-note spacing | Remaining |
| 9 | Independent Mixed Models main/comparison visualizations and label editors | Remaining |
| 10 | Automatic Binomial/logit selection for eligible binary outcomes, preserving explicit family choices | Remaining |
| 11 | Final shared-infrastructure review, GUI testing, release preparation | Remaining |

Resolve the Stage 4 implementation commit:

```sh
git log -1 --format='%H %s' --grep='Separate GLM ordinary and comparison visualization settings'
```

## Architecture and compatibility

- Analysis/UI/results definitions live in `jamovi/*.a.yaml`, `*.u.yaml`, and
  `*.r.yaml`. R6 adapters live in `R/*.b.R`; corresponding option/result classes
  are in `R/*.h.R`. Keep generated option classes aligned with their definitions.
- `jamovi/js/model-terms.js` and the R comparison-layout helpers enforce fitted
  predictor roles. Ordinary overlays reserve aesthetics for models; LM magnets
  and Mixed CAS retain supported grouping. Do not add comparison grouping to GLM.
- GLM retains ordinary `se`, `plotStyle`, `plotScale`; comparison uses
  `comparisonSe`, `comparisonPlotStyle`, `comparisonPlotScale`. Preview, callbacks,
  explanatory text and label contexts route through the matching output's options.
  Core fitting, prediction, interval and comparison functions are unchanged.
- `jamovi/js/glm-plot-controls.js` shares eligibility logic, with separate keys and
  request identities for the two outputs. Stale replies, including replies from
  an earlier restored workspace, cannot replace current state. Unavailable choices
  remain stored; R uses the established effective probability fallback.
- `output-options.js` and `R/jamovi_output_options.R` use the existing versioned
  option-migration pattern. Revision 1 preserves Stage 3 visibility; GLM revision
  2 copies legacy shared display settings once, even with comparison disabled.
  Current revision 2 choices are independent. Plain R constructors use new defaults.
- `jamovi_plot_*` helpers maintain independent GLM label stores. Keep all existing
  result names, schemas and Stage 1 ordering. Mixed still uses its existing mutually
  exclusive visualization modes; simultaneous plots belong to Stage 9.
- Keep conditional Supplier wrappers. A single-cell TargetLayoutBox inside a
  Supplier loses the named `aux` grid column and caused the Stage 2 loading
  placeholder. Stage 3 fixed this; never modify installed jamovi code or CSS.

## Verification and outstanding issues

Stage 4 focused checks cover R6 callbacks, rendered band/style/scale properties,
independent eligibility, rapid/out-of-order metadata, protobuf restoration, labels,
family changes, visibility and numerical/graphical invariance against Stage 3.
Exact matched-setting prediction/interval layers and reference-panel ghost
coordinates remain unchanged. Full package testing is not required for a
presentation-only stage unless shared statistical behavior changes.

Stage 4 validation passed 1,158 focused R6 assertions and 540 native assertions.
Live desktop jamovi 2.7.41 showed independent style/scale/band preferences and
rendered raw probability output without ordinary bands alongside comparison logit
output with bands. Both GLM title editors updated their intended plots; collapsing
the ordinary or comparison section left both result images visible. The Stage 3
supplier-wrapper fix continued to load correctly. Only the disposable review
window was closed, discarding its edits; other windows were preserved. Placement
on the secondary monitor could not be confirmed.

The broader
comprehensive GUI campaign remains incomplete. The user's reported
plot-label routing and collapse behavior received explicit positive live checks
for this GLM configuration; keep broader coverage across modules/layouts open. The stale E2E inventory
snapshot remains outstanding. JASP host compatibility testing remains outstanding.
Do not treat compiled handlers or headless checks as proof of comprehensive GUI
coverage. Secondary-monitor placement may not be observable through automation.

## Continue on another computer

From an existing checkout, preserve local work first, then run:

```sh
git status --short
git fetch origin
git switch ggplot-objects
git pull --ff-only origin ggplot-objects
git log -1 --oneline
```

For a fresh checkout:

```sh
git clone --branch ggplot-objects https://github.com/dustinfife/flexplot.git
cd flexplot
```

Install the existing R, Node, jmvtools and jamovi development prerequisites described
in `docs/jamovi-development.md`. Dependencies and native module caches are local
and are not transported by Git. On a computer without the dependency cache, run:

```sh
Rscript --vanilla dev/jamovi.R install --deps
```

With a cache, use `Rscript --vanilla dev/jamovi.R install`. `JAMOVI_HOME` can select
the local jamovi installation. The installer builds an isolated copy, preserves
working-tree headers and records a source fingerprint; verify it before GUI tests.
Do not commit build artifacts, credentials, private fixtures or `.DS_Store` files.

Focused R6 checks (with jamovi's host R libraries available):

```sh
Rscript --vanilla -e 'devtools::test(filter="jamovi-(glm-(independent-display|scale|panel-ghosts|output-options)$|plot-labels$|model-presentation$)", stop_on_failure=TRUE)'
```

Native source-integration tests are under `dev/jamovi/tests`; initialize jmvcore's
protobuf types before a filtered run. The Stage 4 native subset is
`generalized`, `glm-independent-display`, `glm-restoration`, `glm-scale`,
`glm-panel-ghosts`, and `output-visibility`. The broader `Rscript dev/jamovi.R ui`
command compiles all UI definitions and runs source integrations; the known stale
E2E inventory snapshot can fail that aggregate command and is not a Stage 4 fix.
Review diffs, run `git diff --check`, commit only the focused stage, and push the
current branch only when authorized. Verify the remote commit after pushing.
