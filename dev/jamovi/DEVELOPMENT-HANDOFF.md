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
| 7 | Compact table notes, targeted native footnotes, conditional comparison-test explanations | Complete: `f4cae51a` |
| 8 | Ghost-line reference explanations and reduced plot-to-note spacing | Complete: `843a1269` |
| 9 | Independent Mixed Models main/comparison visualizations and label editors | Complete: `9481f993` |
| 10 | Automatic Binomial/logit selection for eligible binary outcomes, preserving explicit family choices | Complete; implementation commit resolved below |
| 11 | Final shared-infrastructure review, GUI testing, release preparation | Remaining |

Resolve the Stage 10 implementation commit:

```sh
git log -1 --format='%H %s' --grep='Select GLM distributions automatically for binary outcomes'
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
  Mixed revision 2 copies legacy layout/prediction/cluster settings and both label
  arrays once. Current revision 2 choices are independent. Plain R constructors
  use new defaults.
- `jamovi_plot_*` helpers maintain independent GLM label stores. Keep all existing
  result names, schemas and Stage 1 ordering. Stage 9 separates Mixed main and
  comparison visualization settings and label arrays, retaining its legacy keys.
- Keep conditional Supplier wrappers. A single-cell TargetLayoutBox inside a
  Supplier loses the named `aux` grid column and caused the Stage 2 loading
  placeholder. Stage 3 fixed this; never modify installed jamovi code or CSS.

## Stage 5 architecture

- LM/GLM have separate Model Visualization and Model Comparison Visualization
  sections, independent default-off Modify Plot Labels preferences, and native
  `height: large` editors. Model specifications stay in Model Comparison.
- At Stage 5, Mixed retained one active visualization and a shared label store.
  Stage 9 supersedes that combined section with independent controls and stores,
  preserving the saved main keys through the versioned migration described below.
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

## Stage 8 architecture and verification

- Core ghost construction captures `ghost_reference` display metadata from the
  resolved **source** before replication into destination facets. It records
  original variable names, prepared display columns, categorical/bin labels,
  drawable-path status and styling. LM/Mixed supplied-prediction ghosts use the
  same source metadata helper. Neither pathway infers the source from destination
  ghost-layer facets. Reference selection and manual overrides are unchanged.
- Shared notes call the displayed plot's actual facet labeller after saved label
  edits, then escape its output for HTML. One/two panels, numeric bin labels and
  customized panel titles/levels therefore agree with the strips. A success note
  requires two finite distinct x coordinates in the same ghost path; unsupported,
  missing-reference and empty paths retain the existing failure explanation.
- Ordinary LM, GLM probability/logit and Mixed population/cluster plots share
  the concise dashed-gray reference explanation and existing #707070/dashed/
  1.2/.85 styling. Mixed conditional curves still include their original random
  effects. Directional CAS retains its separate Model 2 fixed-effects-on-Model 1
  explanation, gated by the actual directional overlay; ordinary CAS remains
  deferred to Stage 9.
- LM adds only `plotExplanation`, immediately after `plot`, and moves its ordinary
  ghost failure message there without duplication. Existing result/option IDs,
  table schemas and specifications are retained. GLM/Mixed reuse their existing
  adjacent explanation results. Hidden plots hide their notes. Named native
  restoration reuses result objects even when old cached LM output lacks the new
  Html result; annotation addresses and the Stage 1 order remain stable.
- Explanation paragraphs use module-owned inline `margin:0 0 4px 0`. Ordinary
  GLM/Mixed guides and the CAS success note use the same compact paragraphs.
  Installed native CSS still supplies 12px HTML bottom padding, and host editable
  annotation/result spacing remains. Image dimensions (LM 600x400; GLM/Mixed
  existing dimensions), aspect ratios, coordinates, plot margins and global CSS
  are unchanged. This reduces paragraph whitespace; it does not eliminate all
  host or image whitespace.

Focused verification passed the existing 446-assertion ordinary model/GLM ghost
checks and 2,373 core ghost, presentation/restoration and Mixed CAS assertions.
Additional checks cover source metadata versus actual fitted source curves,
explicit categorical/prepared-bin references, custom categorical/numeric facet
labels, same-path finite coordinates, hidden/off/unavailable ghosts, repeated
valid/invalid/valid changes, and conditional CAS notes (143 assertions in the
final focused note run; 111 in the updated shared ordinary-ghost checks). The final installed module
passed 278 native/protobuf/renderer assertions; all three UI definitions compiled
and passed their installed native automatic-handler/restoration checks. Native
HTML rendering retains the compact inline margins and literal escaped text.
Six complete numerical table comparisons against `f4cae51a` matched exactly;
comparison predictions/intervals, ordinary ghost coordinates, and Mixed
population/cluster/CAS plot data also matched. Existing source-curve tests compare
all base-layer data, supplied intervals, ghost coordinates and facet assignments.
No full package suite was run and no statistical behavior changed.

The final module was built/installed with source fingerprint
`9666cbc0c749e8596478a6cfdf0b064995b64337df4e3b7fa6a8be335dae9452`.
Live jamovi 2.7.41 review used one temporary saved workspace at normal results-pane
size: LM/GLM/Mixed two-panel notes displayed their actual prepared reference bins,
notes followed images above estimates, and LM ghost off/on and a custom reference
facet label updated correctly. GLM's probability explanation and Mixed population
semantics remained readable. Paragraph gaps were reduced; native padding and
annotation gaps remain visible. The temporary workspace was saved and closed.
Secondary-monitor placement could not be confirmed. Live coverage did not include
every family, cluster/logit/CAS mode or restoration transition; focused native/R6
checks cover those paths. Stages 9–11, broader GUI coverage, the stale E2E inventory
and JASP host verification remain outstanding.

## Stage 9 architecture and verification

- Mixed main and comparison visualization sections now have independent x/panels,
  grouping where supported, prediction type, cluster count, editor checkbox,
  visible label catalog and retained label store. Main retains `plotX`,
  `plotPanels`, `plotGroup`, `predictionType`, `clusters`, `plotLabels` and
  `plotLabelStore`. Comparison uses `comparisonX`, `comparisonPanels`,
  `comparisonGroup`, `comparisonPredictionType`, `comparisonClusters`,
  `comparisonPlotLabels` and `comparisonPlotLabelStore`. `casGhost` remains the
  directional CAS comparison preference; `ghostLines` remains main-only.
- Both existing image results can be visible simultaneously. Neither output's
  visibility affects comparison statistics or common-sample fitting. Existing
  result IDs/schemas/order/dimensions are retained. Each explanation stays adjacent
  to its image. Main failures hide stale explanatory content; comparison errors
  retain the established diagnostic. Collapsing UI sections is presentation only.
- With comparison enabled, main uses `comparison$first`, the requested-method
  Model 1 fit on the common sample. Comparison plots use requested-method
  `model1`/`model2`; only statistics use their existing ML copies. Fixed/random
  calculations, intervals, inference safeguards and common-sample policy are
  unchanged. Population, conditional and model-specific CAS adjustments retain
  their existing prediction functions and geometry.
- Separate main/comparison fit and plot state reuse keeps label-only changes from
  refitting or resampling. Common-sample main plot reuse checks the primary fit
  options and actual estimation data rather than serialized model environment
  identity. Restored native image states initialize both private caches. A change
  to one layout rebuilds only that plot; model/sample changes invalidate affected
  fits. Existing sampled clusters survive label edits and cached restoration.
- Output-options revision 2 initializes comparison layout, prediction type,
  cluster count and both label arrays from legacy effective shared values exactly
  once, in R and native JS handlers. Revision 1 independent visibility choices,
  model options, ordinary/directional ghosts and all retained label rows survive.
  Existing main keys stay unchanged. Current revision 2 settings are independent;
  direct new R constructors keep explicit comparison settings. Native migration is
  guarded against synchronous re-entry so Model 2-only legacy x/panel selections
  copy before the main controls normalize their narrower predictor roles. The shared label
  engine publishes each array through its own native options response.
- Main roles use only Model 1 predictors/random slopes and its cluster variable;
  comparison roles use the model union. Ordinary comparison grouping stays reserved
  for model identity; unsupported saved grouping is retained with the existing
  explanation/fallback. CAS comparison grouping remains legitimate. Native
  restoration does not append a cluster panel to an explicit saved layout;
  cluster-specific defaults add the cluster only where no saved panel/group
  selection supplies a layout.
- Ordinary CAS ghosts now reuse its existing single fitted-coordinate stream via
  the shared reference copier. No core CAS adjustment/prediction changes were
  needed. Continuous x, one/two prepared panels, dark-gray dashed styling and Stage
  8 source metadata/custom-label notes are supported. Second-axis grouping,
  categorical x, singleton/missing reference curves and other shared unsupported
  layouts retain failure notes. Directional CAS still uses the separate Model 2
  fixed curve and explanation, only when actually drawn.

Stage 9 focused checks passed 537 R6 assertions for independent fits/plots, role
restrictions and visibility; the final independent-output/cluster-selection run
passed 109 assertions (including its additional simultaneous conditional check). The installed module passed 698 native-handler,
protobuf, renderer, cache-restoration and migration assertions, including real
serialized fitted/plot states. Existing Mixed regression fixtures now specify the
intended comparison output explicitly; no visual baselines were changed. Six
numerical-table comparisons against `843a1269` matched exactly. Tested population,
cluster and CAS prediction/adjustment coordinates, comparison intervals and ghost
coordinates also matched; new common-sample ML/REML checks matched independently
fitted `lmer` models. Main CAS ghost checks preserve every base layer and adjusted
observation while copying the actual source curve without a source-panel duplicate.

The full package suite ran to completion: 23,030 assertions passed, with 24
failures in that run. Eight were old shared-layout assertions in comparison-role
fixtures; these were updated for the intended comparison output and passed the
final focused run. The remaining 16 failures were reproduced unchanged using the
Stage 8 source (`843a1269`) in an isolated baseline copy: 12 GLM comparison-note /
visibility expectations, one LM mocked-label render exception, two reset-image
path expectations, and one GLM table-note expectation. They remain documented
rather than changing unrelated LM/GLM behavior or approving new visual baselines.
A broader native Mixed integration run also exposed two pre-existing assertions
expecting visible fixed-effect p columns in `test-jamovi-mixed-tables.R:18`; the
same two failures reproduce against Stage 8. Stage 6's hidden columns remain
correct and unchanged. The final Stage 9 installed native selection excludes
that separately documented stale test. No E2E inventory snapshots were repaired.

The module is built/installed with source fingerprint
`964bbaf93cfdb0fe4144e2f740dae2707f4d86d1ebf85439c02ddeddb2aa44ec`.
A 240-observation/16-cluster random-slope/interaction/quadratic example prepared
both conditional-main/CAS-comparison plots in 6.65s; a label-only update took 0.48s
and state serialization 1.27s in this session. Its cached image states serialized
about 24.8MB and still produced the known size advisory. This alone was not treated
as a defect or used to justify a caching redesign.

Live jamovi 2.7.41 used one disposable 120-observation random-slope comparison
workspace at normal results-pane size. Both plots appeared; each visibility toggle
left the other output available. Separate sections and label editors retained
"Main time" versus "Comparison time". Population, ordinary CAS with reference
notes, cluster-specific main curves with selected cluster panels, and a directional
CAS comparison were observed. Main mode/panel edits left comparison CAS/labels
unchanged. An actual .omv save/close/application-relaunch/reopen restored both plots,
labels, settings and notes; a further main cluster-mode edit still left comparison
CAS unchanged. Saving remained responsive (compressed review file under 1MB).
The review was saved and closed. Secondary-monitor placement was unconfirmed.
Full GUI coverage of every mode/layout/family, the stale E2E inventory, JASP host
verification and remaining release work stay open for Stage 11. Automatic
distribution selection was subsequently added at Stage 10 below.

## Stage 10 architecture and verification

- `family` now includes explicit `auto` (Automatic), the default for new analyses.
  Every named family is a durable manual choice, including Gaussian; selecting
  Automatic again resumes detection. There is no timing-based manual-change flag
  and no automatic setter that rewrites `family` to Binomial. Thus backend updates
  cannot accidentally convert an automatic choice into a manual one. Existing
  family identifiers, links and mathematical implementations are unchanged.
- `R/jamovi_glm_family.R` resolves from actual nonmissing outcome observations,
  before predictor/common-sample omissions. It accepts exactly two observed
  categorical/ordered/character levels, logical TRUE/FALSE, or plain integer/double
  0/1. Numeric 3/7, singleton/all-missing, unsupported types, and unavailable data
  do not select Binomial. Unused declared factor levels do not count. Detection
  does not recode or reorder the response; existing Binomial encoding and fitted
  sample validation remain authoritative.
- A comparison resolves once from the full outcome and passes that family to both
  existing fit paths after common-sample selection. Losing one response level in
  the fitted sample still raises the existing Binomial error, rather than falling
  back to Gaussian. Reporting, event orientation, IRT, odds ratios, marginal
  probabilities, intervals, likelihood inference and plot geometry are unchanged.
- R publishes derived `automaticFamily` and `automaticFamilyOutcome` through the
  existing native options response, even before predictors are selected. These
  fields enable the established Binomial-only controls; they do not control user
  provenance. Plot metadata request keys include the current selector, effective
  family and outcome. Automatic display eligibility requires the derived outcome
  to match the current outcome, so stale replies cannot enable a newer layout.
  Manual selection bypasses derived metadata. The family resolver itself performs
  no asynchronous metadata requests; no delayed callback can overwrite `family`.
- Legacy named-family protobufs/.omv files retain their exact selection, including
  Gaussian on binary data. A legacy protobuf omitting the selector is conservatively
  assigned its historical Gaussian default once; the existing option publisher
  writes it back. No provenance migration framework or new revision is necessary:
  the new `auto` selector value is explicit, idempotent and persisted as-is. Native
  lifecycle guards preserve partially restored options. All plot preferences,
  ordinary/comparison settings, label editors and ghost options remain independent.

Focused R6 regression checks passed 1,122 assertions; the final expanded
Automatic/manual equivalence check passed 207 assertions, including missing
responses, predictor omissions, reversed/ordered event levels and common samples.
All coefficients, SE/CI, OR/IRT/marginal tables, fitted probabilities, comparisons,
probability/logit plot coordinates and ghost layers matched manual Binomial exactly
(tolerance zero). Existing named-family numerical/reference tests passed. Source
native-handler/protobuf/renderer/restoration checks passed 1,388 assertions;
installed handlers/restoration passed 1,316 and installed automatic numerical tests
passed 207. Native installed Option setters, real lifecycle restoration and delayed
plot metadata checks cover manual families, return to Automatic, rapid outcome/
family edits, stale replies, partial restoration and repeated save/reopen cycles.

The full package suite completed with 23,244 passing assertions and the same 16
pre-existing failures documented at Stage 9. Failure locations and counts match
that baseline exactly: 12 GLM comparison-note/visibility expectations, one LM
mocked-label render exception, two reset-image path expectations, and one GLM
table-note expectation. No new failures were introduced, no baseline failures were
repaired in this stage, and no approved visual baselines were changed. The separate
stale Mixed native table assertions and E2E inventory remain Stage 11 work.

The module was built/installed with source fingerprint
`c2c163c61c318af7371cf4d95d048524c96b44661e8312b00df1fec35af05f5f`.
Live jamovi 2.7.41 used one disposable workspace. A pre-Stage-10 Gaussian analysis
on numeric binary data retained Gaussian. A newly created GLM defaulted to
Automatic; a categorical binary outcome fitted Binomial/logit with success event
Yes, and enabled IRT/marginal and logistic probability/logit controls. Explicit
Gaussian persisted through changing the outcome to numeric 0/1; returning to
Automatic fitted Binomial with event 1. An actual .omv save/close/application-
relaunch/reopen preserved Automatic versus legacy Gaussian separately. A further
outcome change from numeric 0/1 to continuous data retained Automatic and resolved
Gaussian. The workspace was saved and closed. Live coverage was representative;
headless/native tests cover the remaining edge cases. No unrelated review windows
were opened. Stage 11's comprehensive GUI/JASP/release work and stale E2E inventory
remain outstanding; no approved visual baselines were changed.

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

## Stage 11A readiness audit

Stage 11A is complete as of 2026-11-24. See
[RELEASE-READINESS-AUDIT.md](RELEASE-READINESS-AUDIT.md) for the Intel laptop
findings and completed ARM64 desktop baseline against `9e14c032`.

The clean desktop full suite completed with **23,253 passed, 15 failed assertions,
2 errors, zero testthat warnings and zero skips**. All 16 historical unsuccessful
checks recur; the one additional failure is the strict inventory check for the
26 unapproved SVG artifacts tracked by the laptop commit. All **140 active SVG
comparisons pass** on the approved ARM64/R 4.5.0/vdiffr 1.0.8/ggplot2 4.0.3
environment. The laptop's forest rounding failure does not recur on this host.
All four UIs compile, a focused desktop native/protobuf smoke passes 533
assertions, and the existing installed Stage 10 module's source fingerprint and
archive/payload hashes match. Existing dependencies and caches were reused.

This completes the audit, not release validation. Stage 11B should first isolate
GLM comparison-layout errors from valid estimates/statistics and guard LM label
preview exceptions locally. Then correct verified stale assertions and portable
oracles, resolve the unapproved SVG inventory, update E2E transitions before
regenerating derived coverage, and rerun the package/native gates. The laptop's
complete native baseline remains 4,494 passed and 42 classified failures.
Comprehensive live jamovi coverage, isolated built-package checks and actual-host
JASP compatibility remain release requirements. No new desktop GUI or JASP
campaign was performed, and no production source, test expectations or approved
visual baselines were changed in this continuation.
