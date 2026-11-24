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
| 4 | Independent GLM visualization settings | Complete: `9bab9cc0` |
| 5 | Plot-control organization, Modify Plot Labels checkboxes, taller editors, readable dynamic legend descriptions | Complete: `a306b18f` |
| 5B | Shared ordinary reference-panel ghost lines | Complete: `07a2ed5d` |
| 6 | Mixed Models estimates, confidence-interval controls, removal of permanently empty fixed-effect p-value columns | Complete: `dedf2a57` |
| 7 | Compact table notes, targeted native footnotes, conditional comparison-test explanations | Complete; implementation commit resolved below |
| 8 | Ghost-line reference explanations and reduced plot-to-note spacing | Remaining |
| 9 | Independent Mixed Models main/comparison visualizations and label editors | Remaining |
| 10 | Automatic Binomial/logit selection for eligible binary outcomes, preserving explicit family choices | Remaining |
| 11 | Final shared-infrastructure review, GUI testing, release preparation | Remaining |

Resolve the Stage 7 implementation commit:

```sh
git log -1 --format='%H %s' --grep='Standardize comparison notes and targeted statistical footnotes'
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

## Stage 5 architecture

- LM/GLM have separate Model Visualization and Model Comparison Visualization
  sections, independent default-off Modify Plot Labels preferences, and native
  `height: large` editors. Model specifications stay in Model Comparison.
- Mixed retains one active visualization and shared label store. Its combined
  Model Visualization / Model Comparison Visualization section has one native
  editor and a visibility preference per mode. The combined heading is deliberate:
  jamovi 2.7.41 CollapseBox labels do not update after creation. Do not use custom
  DOM/CSS updates or duplicate the shared controls to simulate Stage 9.
- `plot-label-editors.js` and `jamovi_plot_editor_migrate` use revision 1 of
  `plotLabelEditorVersion` to reveal legacy nonblank saved edits once, including
  temporarily unavailable retained rows. Current explicit hidden preferences
  survive. Result visibility never depends on editor visibility or collapse.
- Shared legend catalogs use displayed scale labels; committed custom text updates
  descriptions without changing keys or break values. Multiple guides include
  titles for disambiguation. Native TextBox change/blur/Enter drives recalculation;
  no per-keystroke rebuilding is introduced. Legacy Mixed legend discovery
  preserves RNG state. See `docs/jamovi-plot-labels.md`.

## Stage 5B architecture and verification

- `R/jamovi_model_ghosts.R` shares prepared-layout validation, supplied-coordinate
  copying, and styling. LM and standalone Mixed population/cluster plots reuse
  core reference selection and ghost construction without refitting at destination
  panels. Mixed wraps cached streams without changing the selected clusters or RNG.
- GLM keeps its renderer and probability/logit transform path; its old R/JS entry
  points forward to shared mechanics. `jamovi/js/model-ghosts.js` shares metadata
  eligibility and stale-request rejection. Server prepared-data validation remains
  authoritative. Unsupported grouping, categorical x, one prepared panel, absent
  reference combinations and unavailable finite curves retain an ordinary plot.
- All three ordinary controls use Show ghost lines, default unchecked, immediately
  after selectors. Automatic continuous x is supported; Mixed retains its existing
  first-predictor convention. Disabled contexts retain the preference. LM/Mixed
  legacy documents default off; GLM option identifiers/values are unchanged.
- Styling is #707070, width 1.2, opacity 0.85, dashed, above confidence bands. Core
  exploratory styling and directional Model 2 CAS ghosts remain unchanged.
- Ordinary CAS reference ghosts are deferred to Stage 9: adjusted observations
  and the separate fixed-effects curve lack the supplied prediction stream needed
  by this path. Equivalent reference coordinates require deliberate CAS geometry
  work. Mixed comparison modes continue their existing overlays with ordinary
  reference ghosts unavailable. Stage 8 reference explanations/spacing remain open.
- See `docs/jamovi-model-ghosts.md`. No result identifiers, table schemas, label
  stores, comparison display controls, dependencies or fitting/inference changed.

Stage 5B passed 359 existing GLM ghost assertions and 270 focused model-ghost,
independent-GLM-display and Mixed-note assertions. The native-handler/protobuf run
passed 308 assertions. After the final comparison-label routing review, the
affected native subset passed 243 assertions and installed-package real protobuf
restoration/rendering passed 105. Mixed/CAS/label regressions passed 674 assertions
(with the known jmvcore theme deprecation silenced in that test process).
Six standalone/comparison numerical table comparisons matched
Stage 5 exactly, as did tested GLM prediction/interval/ghost coordinates and Mixed
population/cluster/CAS geometry. Full package testing was not run.

Desktop jamovi 2.7.41 showed the LM checkbox after selectors with automatic x
left blank, working on/off ghosts and unchanged confidence bands. GLM probability
output retained its dashed gray reference curve. Mixed population and cluster
plots rendered reference ghosts, while CAS disabled the checkbox, retained the
preference and showed the unsupported-path diagnostic. The disposable review
was closed without saving; existing documents were preserved. Secondary-monitor
placement could not be confirmed. Live GUI checks did not cover every family,
layout or saved-document transition; focused renderer/native/protobuf tests cover
those paths. A stale Mixed-label test expectation was reproduced on Stage 5 and
updated to allow committed description refresh while checking stable saved fields.

## Stage 6 architecture and verification

- Mixed fixed/fixed2 retain the `p` column identifiers and types for compatibility,
  but declarations and R reporting/reset always hide them. The current adapter
  does not calculate fixed-effect p-values; this is not a general inference claim.
  No new denominator-df or p-value method is introduced.
- Include p-values remains in its established Output Options position and appears
  only with Model Comparison enabled. It controls the existing comparison column;
  toggling comparison retains its preference. All LRT/nesting/random-structure,
  convergence/singularity and boundary safeguards remain unchanged.
- The CI checkbox is removed. `includeCI` is hidden, still defaults TRUE, and
  continues to honor explicit FALSE in old saved documents. Both models use the
  unchanged normal-Wald limits. The fixed-effect note is now exactly
  "95% normal-Wald confidence intervals." and absent when intervals are hidden.
  Model 2 retains the existing omission of duplicate method notes. Obsolete fixed
  p-value notes are cleared without altering necessary diagnostics.
- Fresh native standalone/comparison output has no notes attached to random or
  random2. AX output shows jamovi's empty editable Annotation region after each
  table; screenshots show ordinary trailing space and the next model heading or
  reference block. No unnecessary adapter HTML was visible. The user's historical
  note could not be reproduced. Remaining native annotation/heading spacing is
  preserved; no CSS or fragile renderer workaround was added. Existing reset also
  removes legacy empty table notes on recalculation, covered by restoration tests.

Stage 6 focused checks passed 620 R6 assertions and 240 native assertions,
including installed handler/render-to-grid visibility, Model 1/2 table schemas,
real protobuf option restoration, legacy cached-column/note cleanup, default and
explicit CI preferences, comparison p-values, fit caching and diagnostics.
Six standalone/comparison numerical table comparisons matched Stage 5B exactly
apart from the intentionally hidden empty fixed-effect p columns. Comparison
p-values were included in that check. Tested prediction/interval/ghost coordinates
and Mixed population/cluster/CAS geometry also matched. Full package testing was
not run for this presentation-only change. Mixed/model-ghost regressions passed
542 assertions. The final installed-package protobuf options/results restoration
subset passed 108 assertions, including reuse of restored result objects.

The module was built and installed in jamovi 2.7.41. Live standalone Output Options
contained neither inference checkbox; fixed estimates showed normal-Wald limits,
the shortened note and no p column. Comparison Output Options showed Include
p-values and no CI checkbox. Both fixed-effect tables lacked p columns; toggling
the comparison checkbox hid/restored its actual p-value column and an unchecked
preference survived comparison off/on. Random tables had no explanatory notes.
The disposable review was closed without saving and existing documents preserved.
Secondary-monitor placement was unconfirmed. Legacy CI-off restoration was checked
through protobuf/R6 rather than a separate live saved-file scenario. Broader GUI,
stale E2E inventory and JASP verification remain outstanding as described below.

## Stage 7 architecture and verification

- Comparison p headings are `p` in declarations, checked-in headers and reporting
  setters (including restored cached headings). Available tests keep concise
  method identification: LM nested-model F-test; GLM likelihood-ratio/asymptotic
  chi-square; Mixed existing safeguarded fixed-effect LRT. Unavailable LM/GLM
  cases distinguish equivalent designs, nonnested designs and invalid comparisons
  (including identified invalid likelihood df or negative/nonfinite LR statistics).
  Mixed reasons, random-boundary/singularity/convergence safeguards and negative
  binomial separately estimated size qualifications remain intact.
- `R/jamovi_table_notes.R` provides native cell-note attachment, hidden-cell note
  clearing, comparison-reason presentation and one concise Bayes-factor definition:
  "Approximate BIC Bayes factor favoring the row model over the other model."
  LM/GLM directions retain existing rounded values; Mixed remains based on the ML
  comparison BIC values, not the original requested REML fits used for plots.
- GLM retains family/link/success context first on the first visible summary table.
  Supported `Table$addFootnote` cell markers cover raw SE/CI, standardized ratios,
  prediction differences, fitted contrasts, interacting coefficients, Gamma
  inverse reciprocals, negative-binomial uncertainty, IRT conditioning/slopes/
  thresholds, undefined blanks and marginal averaging. Effect-summary markers
  skip intercept predictions. No column-heading footnote API is assumed.
- Writers clear old rows/notes before repopulation, retain column/result IDs and
  replace stale cached headings. Hidden GLM tables clear cell notes; invalid/empty
  tables serialize no explanatory cells. Native cell/renderer de-duplication
  shares one explanation across repeated relevant markers. Generic definitions
  already conveyed by OR/1/OR and link labels are omitted. LM pairwise notes retain
  multiplicity methods/conditioning while dropping a repeated 95% prefix; the
  conditional-means note drops the redundant non-coefficient sentence.
- Stage 6's hidden fixed-effect p columns and saved CI preferences are preserved.
  No options, fitting, inference, estimates, result order, geometry or dependencies
  changed. See `docs/jamovi-generalized-models.md`.

Focused Stage 7 checks passed 1,048 R6 assertions and 495 native assertions,
covering all supported GLM families, available/unavailable tests, BIC direction,
repeated and invalid/valid transitions, hidden/empty sections, protobuf restoration,
restored result-object reuse, native cell serialization, superscripts, escaped
text, context ordering and one footer entry per repeated note. Six before/after
numerical table comparisons against Stage 6 matched exactly. Tested prediction,
interval, ghost and Mixed population/cluster/CAS coordinates also matched.
No full package suite was run. Final installed-package checks passed 425
assertions; the final focused LM wording/conditional-reason comparison check passed 386 assertions.

The module was built/installed in jamovi 2.7.41. Live checks showed LM's `p`
heading with short F-test and BIC footnotes; GLM had readable cell markers,
family/link/success context before specialized notes, distinct IRT and marginal
averaging footnotes; Mixed retained Stage 6 fixed-effect output and its ML/LRT
qualifications with the shortened BIC wording. Live coverage was representative,
not every family/invalid/restored configuration; native/R6 checks cover those
paths. Secondary-monitor placement was unconfirmed. At the user's request all
review windows were closed, saving pending Stage 1, Stage 2 and scale-check edits.
The broader GUI, stale E2E inventory and JASP requirements remain outstanding.

## Verification and outstanding issues

Stage 5 passed 628 focused R6 assertions and 286 native assertions before the
final heading adjustment; the final compiled-UI/protobuf subset passed 107
assertions afterward; installed-package protobuf restoration passed another 72.
All six standalone/comparison numerical table comparisons
matched Stage 4 (`9bab9cc0`) exactly. Comparison predictions/intervals, reference
panel ghost coordinates, and Mixed population/cluster/CAS geometry also matched.
No full package suite was run for this presentation-only stage.

The final module was built and installed in jamovi 2.7.41. Desktop checks verified
LM/GLM section separation, default-off checkboxes, large editors, the LM title's
ordinary-output routing, retained title after hiding the editor, stable cell focus
while typing, Model 1/Model 2 legend descriptions, and description refresh after
a committed custom entry. GLM main/comparison editors opened independently and
comparison logit descriptions remained correct. Mixed retained a shared axis edit
when comparison was enabled, revealed its comparison checkbox independently, and
preserved its checkbox through collapse/reopen. The final combined Mixed heading
was verified after reinstalling. Disposable review windows were closed without
saving; existing documents were preserved. Secondary-monitor placement could not
be confirmed. Full GUI coverage of every scale/layout/restoration path remains open;
focused native/R6/renderer checks cover those paths without claiming live coverage.

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
Rscript --vanilla -e 'devtools::test(filter="jamovi-(label-editors$|plot-labels$|glm-independent-display$|mixed-notes$|model-presentation$|comparison-roles$)", stop_on_failure=TRUE)'
```

Native source-integration tests are under `dev/jamovi/tests`; initialize jmvcore's
protobuf types before a filtered run. The Stage 5 native subset adds `glm-initialization`, `mixed`, and `label-editors`;
the Stage 4 native subset is
`generalized`, `glm-independent-display`, `glm-restoration`, `glm-scale`,
`glm-panel-ghosts`, and `output-visibility`. The broader `Rscript dev/jamovi.R ui`
command compiles all UI definitions and runs source integrations; the known stale
E2E inventory snapshot can fail that aggregate command and is not a Stage 4 fix.
Review diffs, run `git diff --check`, commit only the focused stage, and push the
current branch only when authorized. Verify the remote commit after pushing.
