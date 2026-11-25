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
| 11A | Release-readiness audit | Complete; findings below |
| 11B.1 | GLM/LM plot-error isolation | Complete: `ba5d76e1` |
| 11B.2 | Stale tests and artifact retirement | Complete: `55363abd` |
| 11C | E2E coverage and validation metadata | Complete; validation below |
| 11D | Current-build live GUI campaign | Paused for manual usability review; see latest entry |
| 11D.2 | Resolve live pilot findings | Complete; [correction report](e2e/STAGE-11D2-CORRECTIONS.md) |
| Release | Isolated package checks and actual-host JASP compatibility | Remaining |

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

## Stage 11B.1 error isolation

Completed on 2026-11-24 from `21e57176` on `ggplot-objects`.

GLM comparison layout failures now remain in the comparison image's `plotError`
state, preserving both fits, their common sample, coefficient tables and
comparison statistics. Guide/catalog preparation and rendering use plot-local
error handling and replace stale explanatory content. Hidden comparison output
remains independent. LM's preview guard now includes label/context/catalog and
ghost-note preparation; preview and renderer errors produce terminal diagnostic
images while numerical and supplementary results continue. The next valid run
clears the diagnostic and recovers in the same analysis. Existing option/result
identifiers, saved label keys/stores and migration versions are unchanged.

The focused package run passes 684 assertions; native-handler/protobuf/restoration
checks pass 606, including 25 new saved-option assertions. Regressions cover
visible/hidden GLM recovery, explicit and Automatic families, common samples,
renderer-only errors, LM preview and comparison label preparation, label
persistence and protocol reopen. Actual PNG files, result status, cleared image
state and retained numerical tables are checked. Fitting/inference failures are
not converted into layout errors.

Four representative LM/Gaussian/Binomial/Automatic captures are identical to
Stage 11A for tables, coefficients/covariance, fitted values and rows, built plot
coordinates and ghost metadata, including interactions, quadratics and panels.
The module was built and installed using existing caches with source fingerprint
`68964d65afd6f88cbf4697301d09edc4d32412ad773ddcb1298cd965fb317550`;
archive and installed-payload hashes were independently verified. No dependency,
JASP, E2E manifest, unrelated stale test, `.DS_Store` or visual-reference changes
were made. See the audit's Stage 11B.1 section for detailed evidence.

Live jamovi 2.7.41 verified the stale GLM comparison role and a controlled LM
layout error, with valid estimates and comparison statistics retained. A test-data
edit forced native recalculation before opening the options panes; changed
coefficient values confirmed the results were not merely cached fixture output.
Correcting the roles recovered both plots and removed their diagnostics. GLM
main/comparison visibility remained independent. The user's help was needed only
to open the disposable file through the macOS picker; the review was subsequently
closed and test edits discarded. Injected label-helper/renderer exceptions and
hidden stale-layout cases are R6/native evidence, not additional live GUI claims.
The comprehensive GUI campaign and JASP compatibility remain outstanding.

The final full package suite completes from a fresh source export with **23,767
passed, 15 failed assertions, 1 error, zero testthat warnings and zero skips**
across 796 test cases. All **140 approved visual comparisons pass** and their
files are byte-identical. The repository's 26 unapproved SVG artifacts and all
`.DS_Store` files remain untouched. Compared with Stage 11A's 23,253/15/2 baseline,
the LM preview error is resolved; the extra passes come from new coverage and
assertions now reached. The existing direct-helper stale-axis assertion was
updated to require retained fits/statistics and a plot-local error. No unrelated
stale expectations were repaired.

The remaining failures are twelve GLM note/visibility assertions, two Mixed
image-path assertions, one SVG inventory assertion and one obsolete GLM
table-note error. The complete laptop native audit's 42 classified failures
remain separate from this stage's passing 606-assertion selection. Next stages
should address the verified stale tests, portable oracles and repository/E2E
hygiene before rerunning release gates and the broader GUI/JASP campaigns.

## Stage 11B.2 test contracts and repository hygiene

Completed 2026-11-24 from `ba5d76e1` on `ggplot-objects`. Production source,
calculations, option/result identifiers and the installed module's build-input
fingerprint remain unchanged (`68964d65afd6f88cbf4697301d09edc4d32412ad773ddcb1298cd965fb317550`).

GLM tests now assert concise likelihood-ratio notes, independent plot visibility
and exact numerical preservation through toggles. Reporting tests inspect actual
SE/CI, residual-df, IRT, marginal-averaging and negative-binomial cell footnotes,
including removal on hidden/empty output and recovery. Mixed lifecycle tests
require cleared paths/state and render real PNGs after invalidation across all
four visibility combinations. Native checks retain hidden fixed-effect p column
identifiers, independently test comparison likelihood p-values, and preserve
revision-2 divergent settings and label stores across repeated protobuf reopen.
The forest oracle alone uses `1e-14` for floating-point boundary probabilities;
its independent public prediction comparisons keep the existing `1e-10` tolerance.

Exactly 26 unapproved SVG artifacts were untracked and moved out of the active
snapshot directory after hash-verified copies were preserved in ignored
`.jamovi-dev/unapproved-svg-artifacts/`. Exact-path ignore rules prevent these
legacy outputs from becoming untracked release candidates. Root `.DS_Store` was
untracked but remains locally present; unrelated development Finder files are
also preserved and ignored. The audit lists every retired path. All 140 approved
reference hashes remain unchanged, and the strict inventory sees exactly 140.

The corrected focused package selection passes 1,817 assertions and the relevant
native/restoration selection passes 648, with no failures, errors, skips or
testthat warnings. No checked-in E2E inventory/scenario metadata was regenerated. Full-suite
and remaining Stage 11C results are recorded in the audit's Stage 11B.2 section.

The complete native desktop suite has **4,609 passed and 29 failed assertions**,
with no errors, skips or testthat warnings. All 29 are deferred Stage 11C E2E
checks: one inventory snapshot, 27 uncovered-option checks (5 LM / 9 GLM / 13
Mixed) and one derived coverage CSV mismatch. The other nine CSV files and all
numerical fixture/reference checks pass; the laptop's three numerical CSV
spelling failures do not recur on this host. The audit lists all uncovered
options, including Automatic's two derived fields. Keep those coverage gates
intact and update actual E2E transitions before regenerating derived metadata.

The full package suite is now **24,504 passed, 0 failures, 0 errors, 0 testthat
warnings and 0 skips** across 797 cases, versus Stage 11B.1's 23,767/15/1.
All 140 approved visual comparisons pass and their hashes are unchanged in the
working tree and clean export. Strict SVG inventory validation passes. Deletion
review confirms exactly the listed 26 unapproved SVGs plus root `.DS_Store` were
untracked; no approved reference was removed, regenerated or approved.

No new modeling defect was uncovered. An existing desktop jmvcore 2.6.3 `asDF`
limitation for empty IRT/probability tables after a family transition reproduces
on unchanged Stage 11B.1 source; native empty-state counts/visibility/notes remain
correct. It is documented as a separate development-harness compatibility
follow-up. No production workaround was added. Complete the deferred E2E work in
Stage 11C, then the comprehensive GUI, isolated built-package release checks and
actual-host JASP validation before claiming release readiness.


## Stage 11C — E2E reconciliation complete

Completed 2026-11-24 from `55363abd`. The final native suite is green:
**4,837 passed, 0 failures/errors/testthat warnings/skips**, across
92 cases. Focused E2E/native replay checks pass **882**
assertions across 32 cases. The clean full package retains the
**24,504 / 0** baseline across 797 cases, with **140/140** approved exact visual
comparisons passing. The audit contains methods, classifications and local evidence
paths; [structured counts](e2e/stage11c-validation.json) accompany it.

All 150 current options have meaningful planned actions or explicit indirect
contracts; the 29 Stage 11C failures are resolved without blanket exclusions.
The matrix is **77 scenarios** (16 Flexplot / 17 LM / 24 GLM / 20 Mixed),
**267 transitions**, **156 preserved reference checkpoints**, and 77 final host
reopen checks. New LM-17, GLM-24, MM-19 and MM-20 cover legacy provenance,
Automatic/manual families and independent Mixed controls/stores. Existing label,
visibility, CAS/ordinary ghosts and recovery definitions were reconciled with
current native controls. Removed controls and inferred fields are not submitted
as user edits. Actual protobuf decoding/migration and intended main REML-fit
invariance are independently tested. Required reviews, current UI/derived values,
actual restoration inputs and comparison-only image resources fail closed when
missing or wrong. See [every prior option gap and its checkpoints](e2e/STAGE-11C-COVERAGE.md).

Regeneration changes only source inventory, coverage CSV and scenario index.
All 13 numerical/schema/provenance files, original formulas/reference keys and
140 approved SVG bytes are preserved. Production source/result identities,
dependencies, model calculations and installed build remain unchanged at
`68964d65afd6f88cbf4697301d09edc4d32412ad773ddcb1298cd965fb317550`. No rebuild or new GUI campaign was performed; no additional
modeling defect was found. Keep the earlier jmvcore empty-table exporter caveat
separate from production correctness.

Next use [Stage 11D's prioritized plan](e2e/STAGE-11D-PLAN.md): **39 P0 / 38 P1**,
with humans for monitor/window ownership, inaccessible controls, host file pickers,
dynamic labels and controlled delayed replies. Record actual native save/close/
reopen, stale-image recovery, plot coordinates, fitted-family/event coding and
independent numerical comparisons. These 77 scenarios are **defined, not complete
current-build live GUI passes**; component-native tests and historical pilot
observations do not change that status. Release still requires the complete live
campaign, isolated built-package checks and actual-host JASP compatibility.


## Stage 11D.1 — live GUI pilot executed, acceptance incomplete

Completed the seven-scenario execution on 2026-11-24 from `d97541e8` without
production, manifest, harness, dependency, numerical-fixture or SVG changes.
See the [pilot report](e2e/STAGE-11D1-PILOT.md),
[campaign ledger](e2e/stage11d1-ledger.json) and
[evidence hashes](e2e/stage11d1-evidence.json). Large actual native captures and
host-saved OMVs remain ignored under `.jamovi-dev/e2e-runs/stage11d1-20261124/`.
Installed/running 0.27.0 source fingerprint remains
`68964d65afd6f88cbf4697301d09edc4d32412ad773ddcb1298cd965fb317550`;
post-restart descriptors and final installed receipt pass. All 156 protected
artifact/reference hashes are unchanged. The 24,504 package / 4,837 native /
140 approved visual baseline is inherited, not rerun for documentation work.

FP-03, LM-03, GLM-08, MM-10, LM-15, GLM-20 and MM-15 were attempted in order.
All 27 checkpoint states were inspected, with seven final actual host reopens
and seven subsequent native edits. LM-15's hidden comparison-group clear action is
unavailable, so its partial action is not claimed complete. Strict results are
**26 passed / 7 failed / 8 blocked** across 41 checks: transitions 18/5/4,
reopens 4/1/2, subsequent edits 4/1/2. LM-03, GLM-08 and MM-10 pass their declared
checks; all other scenario failures/blockers remain. The other 70 scenarios were
not launched. All restored typed values and factor levels/order are preserved.
Primary's unused literal backtick name is host-normalized, so FP-03's exact schema
review fails; no fixture change or waiver is made.

Two scenario contracts need review: LM-15 requires clearing an unavailable,
retained ordinary comparison-group preference; GLM-20 expects stale x/group roles
retained when native empty-term cleanup clears them. MM-15 exposes a real blank
main-error presentation symptom with correct saved error/resource clearing and
intact estimates; module/host-renderer responsibility is unassigned. Its comparison
error is visibly isolated with an explanatory companion note and correct tables,
but the verifier cannot represent HTML companion rendering-error diagnostics.
All independently checked estimates, SE/CI and comparison N/AIC/BIC/Bayes factors
match; nested LR/df checks pass where exposed. Population recovery succeeds.

Human input opened early documents on the secondary monitor. After the operator
requested autonomous opening and left, native `getApp` plus Open Recent reliably
reopened the owned document. Fresh monitor coordinates are unavailable in the
bridge; thirteen later observations explicitly leave placement unconfirmed,
blocking eight otherwise passing checks. Do not call this unattended acceptance.
Only owned analyses/documents were removed/closed, and the final review document
was saved and closed. Next resolve the main diagnostic integration, review the
two scenario contracts and companion-note verifier, establish monitor evidence
and decide the literal-name host contract; rerun affected pilot gates before
remaining P0 batches. Full campaign, isolated built-package release checks and
actual-host JASP compatibility remain outstanding.


## Stage 11D.2 — pilot corrections complete, remaining campaign ready

Completed 2026-11-25 from `9e0e39a9`. See the
[correction report](e2e/STAGE-11D2-CORRECTIONS.md),
[current-build ledger](e2e/stage11d2-ledger.json) and
[evidence index](e2e/stage11d2-evidence.json). The Mixed main plot now uses its
existing adjacent explanation for actionable rendering errors; late main and
comparison callbacks clear stale image paths. Fits, inference, geometry, result
identifiers/schemas and saved options are unchanged. LM-15 now tests intentional
retention of unsupported comparison grouping and its supported panel fallback;
GLM-20 tests clearing stale roles/results when fitted terms become empty. The
verifier recognizes only matching, current declared image/companion diagnostics,
with stale resources absent and other results intact.

The four affected scenarios FP-03, LM-15, GLM-20 and MM-15 pass all 13 transitions,
four actual native host reopens and four subsequent edits. Three current-build
smoke checks (LM-03, GLM-08, MM-10) and a supplementary main-error/valid-comparison
check also pass: **25 passed / 0 failed / 0 blocked**. Smokes are not comprehensive
scenario reruns. Numerical results agree with original independent references;
FP-03's native regression line agrees at five coordinates within 0.70 pixels.
The other 70 scenarios were not launched.

The production/installed/running fingerprint is
`54081947f58fe59e1543ca1c1e21f3828ee34464c719ee23ba84ac7808c42d63`.
Both complete suites passed on the approved desktop environment: **24,546 package
assertions / 799 cases**, **4,925 native assertions / 98 cases**, zero failures,
errors, testthat warnings or skips; **140/140 exact SVG expectations passed**.
All four UI definitions compile and package-test source boundaries pass. All
156 independent numerical keys, 16 fixture/metadata artifacts, 140 SVGs and
416 indexed historical local files retain their hashes. No visual baseline was
approved or regenerated.

Native rename/save can store a literal backtick, but jamovi 2.7.41 normalizes it
again on reopening. GUI verification permits only explicit primary-fixture
``tick`name`` → `tick_name` mapping for that host; all typed values and factor
metadata remain exact. Development OMV preparation also now preserves whole-valued
doubles with matching binary and Decimal metadata. This corrects the earlier
pilot summary's overly broad typed-storage claim for unused `nb_y`/`constant`;
original evidence/verdicts remain unchanged. Rejected preliminary metadata-width
experiments are retained and excluded from passes.

Monitor placement is unconfirmed for these new checks and is operational only;
exact owned disposable-document/analysis identity remains mandatory. One initial
post-install native-binding outage required a human-assisted file open. Native
getApp/Recent then completed the required reopens autonomously, with fresh identity
checks and no repeated Finder/picker attempts. The review document was saved and
closed; no unrelated windows/settings were changed. Historical monitor-only
reassessment is separate (34/7/0), with original build/evidence provenance, and is
not a corrected-build rerun or replacement for the original 26/7/8 ledger.

The process is ready to resume prioritized P0 batches with the new policies and
strict current-build evidence. Do not begin them automatically. Comprehensive
P0/P1 GUI acceptance, isolated built-package release checks and actual-host JASP
compatibility still block release. The earlier Stage 11D.1 next-step paragraph is
historical; its pilot correction gates are now resolved as described here.


## Stage 11D.3 — independent-output GUI batch, acceptance incomplete

Executed 2026-11-25 from `1fe5c7b2`; see the
[batch report](e2e/STAGE-11D3-INDEPENDENT-PLOTS.md),
[ledger](e2e/stage11d3-ledger.json) and [evidence index](e2e/stage11d3-evidence.json).
Only LM-11, LM-16, GLM-23 and MM-19 were launched. Strict disposition across
40 planned checks is **23 passed / 3 failed / 1 blocked / 13 unexecuted**.
LM-16 passes its five transitions, actual native close/Recent reopen and subsequent
independent main-title edit. LM-11 completes eight transitions plus reopen/edit,
but its required ordinary comparison explanation is absent at step 6. GLM-23
attempts all eight transitions; its raw comparison selects a new label context
with a blank title despite the earlier title remaining in its retained store.
Its final host reopen/edit are unexecuted after the finding. MM-19's initial
population/conditional plots and independent numerical checks pass; step 2 exposes
missing generated Plot title rows. That scenario stops immediately, leaving
steps 3–11 and final reopen/edit unexecuted. No backend title injection or contract
waiver substitutes for those actions.

GLM step 5 is separately blocked: its native screenshot has the main title, but
the copied completed saved PNG lacks it. Step 6's saved PNG has the title again.
An asynchronous save/copy race versus cached-resource serialization remains
unresolved; do not call this a confirmed comparison-edit rendering defect.
The initial optimistic result/review is retained separately and superseded by the
ledger's blocked disposition. Future evidence must establish settled disk saves,
not infer image behavior from options or hash differences alone.

Saved LM/GLM estimates and applicable comparison statistics agree with independent
fits. LM ghost coordinates identify low as the source, without a duplicate source
curve; nine GLM comparison coordinates pass within 1.663 pixels. Mixed REML
coefficients/SE/limits match within `5e-16`, ML comparison references agree, and
24 population/conditional image coordinates pass within 2.453 pixels. REML/ML
curve differences are below pixel resolution; coefficient checks provide that
fit distinction. No later Mixed CAS/ghost coverage is claimed.

Production, tests, harness, manifest and dependencies are unchanged. Installed/
running 0.27.0 retains fingerprint `54081947f58fe59e1543ca1c1e21f3828ee34464c719ee23ba84ac7808c42d63`.
The **24,546 package / 4,925 native / 140 visual** baseline is inherited, not rerun.
All 162 protected files, 156 original reference keys, and both earlier local
indexes (416 + 316 files) are unchanged. One owned document was saved and closed;
all monitor placements remain operationally unconfirmed. No human assistance or
unrelated window/settings changes were needed.

Four P0 now have full current-build acceptance (LM-15, GLM-20, MM-15, LM-16);
FP-03 is accepted P1. **35 P0 still require full acceptance**, including three
smoke-only and these three unsuccessful scenarios; 29 P0 have not been attempted
on this build. The other 66 scenarios remain unlaunched. First resolve the Mixed
title gap, decide LM explanation/GLM title-context requirements, and investigate
settled-save evidence before rerunning affected scenarios. Then resume authorized
P0/P1 batches, isolated built-package validation and actual-host JASP/.jasp checks.
This batch supplies evidence and blockers; it neither repairs them nor approves
release. Earlier statements about 70 unlaunched scenarios are historical.


## Manual usability review — ordinary Flexplot corrections

Completed 2026-11-25 from `c57f1af5` on `ggplot-objects`. The comprehensive E2E
campaign is **paused for the user's manual review**; no new campaign scenario,
LM/GLM/Mixed feature work, JASP work or release-package campaign was started.
Earlier Stage 11D ledgers retain their original build identities and verdicts.

- Removed the related-t checkbox from ordinary Flexplot. Its existing `diff`
  Bool option/default and R adapter behavior remain for saved analyses. The option
  is explicitly hidden in the analysis declaration because the jamovi compiler
  otherwise adds an omitted control back into the compiled interface. Core paired
  differences remain available in R; actual protobuf restoration is tested.
- Ordinary suppression now bypasses the reference-ghost route while retaining the
  saved `ghost` preference. Curves, categorical fitted means/uncertainty and their
  ghosts disappear together; raw observations remain. Restoring fits restores
  supported ghosts. Binary categorical probability markers also honor suppression:
  the shared magnet renderer now gates `show_fitted` with `!suppress_smooth`.
  Explicit comparison predictions retain their existing separate path, checked by
  the directly affected magnet-comparison regressions. No fit calculations changed.
- The suppression checkbox remains enabled for continuous, categorical and empty
  predictor layouts; measurement type no longer traps a retained true value.
- Bins are enabled only for a continuous numeric second predictor or continuous
  numeric conditioning variable. A numeric first predictor alone, integer-stored
  nominal/ordinal roles, or categorical panels do not qualify. Native metadata
  requests refresh on predictor/panel changes and remote data changes; request
  identity rejects obsolete replies. Eligibility changes do not reset `bins`.
  Other ordinary fit controls retain their existing enablement rules.

Citation review: commit `6a57fc36` moved GLM citations from individual tables to
analysis-level refs. Ordinary `jamovi/flexplota.r.yaml` **already uses that solution**:
root `refs: [flexplot, graphicalAnalysis]`, with no plot/item citation declarations.
They identify Fife (2022) and Fife et al. (2021) in `jamovi/00refs.yaml`. Read-only
inspection of jamovi 2.7.41's `resultsview-CmqlfJYb.js` confirms its
`jmv-reference-numbers` component maps reference IDs to numbered spans and the
result element appends that component. Actual native screenshots show [3] [4] at
the analysis footer, with their full bibliography and jamovi/R software attribution
preserved. Analysis-level placement avoids table-attached markers but does not
remove the host-generated footer numbers. That remaining placement is a host
limitation; no citation was deleted, CSS hidden, or installed renderer modified.

Focused validation passed **3,389 package assertions / 53 cases** and **32 native
R assertions / two cases**, zero failures/errors/test warnings/skips. The native
subset compiles/bundles ordinary Flexplot against the installed automatic-handler
runtime and exercises reorder/reassignment, metadata-type changes, stale replies,
unavailable columns, preference retention and citation declarations. New package
regressions cover continuous/categorical suppression/restoration, probability
markers, raw points, ghosts, callback transitions, univariate validity and real
legacy protobuf options. The older conditioning test now requests fits explicitly
when asserting that ghosts are drawn. Directly affected magnet/ghost/related-plot
regressions and the four existing ordinary adapter SVG comparisons pass. No full
package/native suite was run. All 140 approved SVGs and 16 deterministic artifact
files retain their hashes; no visual baseline was regenerated or approved.

Built and installed 0.27.0 using the existing dependency cache. Final installed/
running preflight passed with source fingerprint
`2cdaca06b435693667527848f5e60c361777f1ec15375459ac0b1c85bc88df32`, archive
`48366487b5145d0fa7d52f03a580c44b69efd081d1ed667b9fda8ed320a3f012`, payload
`86d2cd6c9e261bdf8f949a21f323f9199b8b2e4ec1db099387342e4ff9154c49`.
The latest module is left installed. No LM, GLM or Mixed definition/handler was
changed; this focused validation does not promote historical GUI passes to the
new build or claim a new complete-suite baseline.

Brief actual jamovi 2.7.41 smoke checks confirmed related-t control absence,
continuous fitted/band/ghost suppression, categorical transition with the checkbox
still enabled, restored categorical summaries/ghosts, numeric second-axis and
numeric-panel bin eligibility, retained value 5 while disabled, and native citation
placement. A settled native save is complete with categorical predictor `binary`,
panels `group,z`, bins 5 and retained ghost preference. Probability-marker and
measurement-type/reorder edge cases are additionally covered by the focused
renderer/compiled-handler tests; they are not claimed as extra live campaign
transitions. Monitor placement was unconfirmed. Automation stopped safely when
the foreground became the user's `testing` document; after the user requested
continuation, native Recent reopened the owned review file. One bounded bridge
operation timed out and was rebound; an initial incomplete transfer capture is
retained as a probe, not a categorical-first pass. The completed owned review
was saved/closed, returning to `testing`; no user document was edited or closed.
Local screenshots, AX captures, saved OMV/inspection, receipt/preflight and test
logs are under `.jamovi-dev/manual-flexplot-20261125/`.

Stop here and await the next manual-feedback batch. Comprehensive GUI acceptance,
previous Stage 11D.3 findings, isolated package checks and actual-host JASP remain
separate outstanding release work.


## Manual usability review — Linear Model corrections

Completed 2026-11-25 from `06cdc10c` on `ggplot-objects`. Keep the comprehensive
E2E campaign paused and await the user's GLM feedback. The immediate target is
jamovi manual-review/release preparation; comprehensive JASP compatibility is
**deferred**, as requested. Historical campaign ledgers remain unchanged and do
not establish acceptance of this newly built module.

1. **Conditional common-sample explanation:** LM now compares each formula's
   independent complete-case mask against the existing common estimation mask.
   The note appears only if at least one independent sample loses observations,
   and gives the additional exclusion count for each affected model. Complete data
   and identical independent complete cases have no note. The same common rows
   still go to the same two `lm()` fits; coefficients, likelihoods, comparison
   statistics, nested F-test calculation and eligibility rules are unchanged.
2. **Consistent plot names:** the main section, Output Options checkbox, image
   result heading and default plot title use `Model Visualization`; corresponding
   comparison names use `Model Comparison Visualization`. The default is applied
   before the existing custom-label renderer. Saved custom titles and all label
   keys/option identifiers are preserved. The main CI label is now simply
   `95% confidence intervals`. There is no redundant comparison CI caption to
   remove; existing independent comparison/main uncertainty behavior is unchanged.
3. **Include p-values:** LM passes its existing preference to the comparison table.
   Unchecked hides that column and its F-test/unavailability explanation; checked
   restores them. The numeric cell value and comparison statistics are retained.
   Disabling/re-enabling comparison retains the preference. Nonnested/equivalent
   cases still cannot acquire a newly invented F-test.
4. **Comparison supplier:** its `persistentItems` is now false, matching the main
   visualization supplier. The existing model-union synchronization and native
   used-item filtering remove assigned x/panel variables from available choices,
   return eligible removed roles, and update after replacement/reorder/model edits.
   No parallel supplier implementation was added. The separate magnet grouping
   supplier and temporarily unavailable stored grouping preferences are preserved.
5. **Statistical header markers:** only LM's comparison notes changed. There is no
   installed `Column` API for statistical header footnotes: native cell
   `addFootnote()` would place markers on values. The reliable supported alternative
   is literal Unicode superscripts in `Column$setTitle()` plus keyed `Table$setNote()`
   explanations. For all three notes this produces `N¹`, `BIC Bayes factor²`, `p³`;
   when N/p notes are absent the remaining markers renumber consecutively. Titles
   and notes reset on every table refresh. Comparison cells carry no duplicate
   footnotes. Column identifiers/types, row keys and numeric values remain intact.
   The installed renderer escapes header titles as text (HTML `<sup>` is not a
   formatting API), preserves Unicode superscripts, and renders the matching notes
   below the table. Native serialization/restoration and R text export preserve
   these headings. No CSS, host changes or unofficial heading-footnote mechanism.
6. **Bibliographic placement:** `6a57fc36` moved GLM references from a table to
   the analysis root, which still places a native reference component at the
   analysis footer. Jamovi supports `refs` on individual results. LM now registers
   its two article references only on an additive native HTML result,
   `methodReferences`, following the existing outputs. Its visible **Method
   References** text identifies Fife (2022) and Fife et al. (2021) and directs readers
   to the full bibliography. The native numbered markers thus belong to that
   labeled context, rather than appearing alone at the analysis footer. Full
   bibliography and automatic jamovi/R software attribution remain. The host
   `jmv-reference-numbers` component still appends markers after its owning result;
   the module cannot select arbitrary pixel coordinates or an inline sentence
   insertion point through supported result metadata. Root refs are empty, so no
   duplicate analysis marker is created. Old caches with root refs and no new
   block restore into the existing named results and recalculate correctly. This
   is LM-only; ordinary Flexplot, GLM and Mixed citation placement is unchanged.

Validation: **579 package assertions / 17 cases**, **248 native assertions / six
cases**, and **99 assertions / three cases against the actually installed module**
all passed, with zero failures/errors/test warnings/skips. Focused coverage includes
independent/common sample masks and unchanged fits/F-test, hidden/restored p-values,
conditional header numbering, cell-marker absence, default/custom titles and stable
keys, interval independence, supplier filtering with the installed native method,
model union/group retention, real protobuf options/results restoration, compiled
UI/headers and named annotation ordering. Installed renderer methods execute against
real serialized table fixtures and exercise Unicode headings/notes and citation
ownership. The new Unicode header approach also passes native text export checks.
The existing comparison-role/style, targeted statistical-note and label suites were
used for directly affected consumers; no full package/native suite, comprehensive
E2E campaign, JASP work or visual-baseline regeneration was run. All 140 SVGs and
16 deterministic artifact files remain unchanged; package-test source boundaries
pass. Initial development-test fixture/DOM-stub errors were corrected before the
final passing native run; production behavior was not loosened to satisfy them.

Built/installed 0.27.0 using the existing cache, with source fingerprint
`5b079fa85aad012281e832b0734803316f3f51d96e500c98bbe69460792f7f6d`, archive
`856670ea98c0ba1c947fb978c4620039dc03b9ddcc8685338804990f95e9710f`, payload
`a63e870308d3084934e0487e18e0217301fef9ad5be1dc664fe4c41d81006aaa`.
Installed source/archive/payload receipt verification passed. The latest module is
left installed for the user's inspection. **No live GUI testing and no opening,
closing or manipulation of jamovi review windows occurred.** Header and citation
placement are renderer-level verified; the user's live visual review remains the
next check. No claim of new live GUI or full-suite acceptance is made. Local logs,
result summaries and installation preflight are in
`.jamovi-dev/manual-lm-20261125/`.
