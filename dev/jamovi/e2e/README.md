# jamovi end-to-end framework — Stage 11C

This directory contains deterministic fixtures, authoritative scenario definitions,
strict metadata validation and a single-scenario native GUI evidence harness.
Stage 11C reconciles the Stage 10–11B implementation from `55363abd`; it does
not execute the Stage 11D campaign. All **77 scenarios / 267 transitions** remain
**defined, not executed or passed on the current installed build**. Each also
requires a final host save/close/reopen checkpoint (344 required captures total).
Component native/R6/renderer/protobuf tests are separate evidence, not full live
scenario passes. Historical pilot observations in PHASE-2A.md remain historical.

See [the option reconciliation](STAGE-11C-COVERAGE.md) and the prioritized
[Stage 11D campaign plan](STAGE-11D-PLAN.md). Source R/JS/YAML remains authoritative.

Historical campaign update (2026-11-24): [Stage 11D.1's seven-scenario pilot](STAGE-11D1-PILOT.md)
was executed after Stage 11C. Its [separate ledger](stage11d1-ledger.json) records
26 passed / 7 failed / 8 blocked checks; three scenarios pass declared checks,
with shared fixture caveats. The other 70 remain unexecuted. The Stage 11C counts
and historical Phase 2A records above/below retain their phase-specific meaning.

Current correction (2026-11-25): [Stage 11D.2](STAGE-11D2-CORRECTIONS.md) records
25 current-build checks passed: four fully rerun affected scenarios, including
actual host reopening and subsequent edits, three proportionate smoke checks and
one supplementary error-isolation check. The other 70 remain unexecuted. See the
[separate correction ledger](stage11d2-ledger.json); these results do not approve release.

## Files and reproducibility

| File | Purpose |
| --- | --- |
| `fixtures.R` | Fixed-seed DGP and independent `lm`, `glm`, `glm.nb`, `lmer` reference fitting; no flexplot fitting calls |
| `manifest.json` | 77 scenarios and ordered native-control/restoration checkpoints; curated source of truth |
| `generate.R` | Validate manifest, regenerate typed data, independent references, source inventory and option coverage |
| `artifacts/fixtures.rds` | Authoritative typed fixtures, preserving factors, ordered levels and literal names |
| `artifacts/*.csv` | Importable datasets; empty fields are missing values, not empty categories |
| `artifacts/data-schema.json` | Classes, factor order, missing counts; apply these after CSV import |
| `artifacts/references.rds` | Full-precision named coefficient/covariance/prediction reference objects |
| `artifacts/references.json` | Portable numerical references with explicit coefficient names and row identities |
| `artifacts/inventory.json` | Every option/default/domain, nested UI control/condition, result item, handler and relevant source/test path |
| `artifacts/scenario-index.csv` | Compact scenario IDs, priorities, datasets, checkpoint counts and launch paths |
| `artifacts/coverage.csv` | Every option linked to proposed GUI scenarios and existing headless evidence |
| `artifacts/provenance.json` | Seeds, package versions, dimensions and CSV fingerprints; no timestamps or absolute paths |
| `../tests/test-e2e-design.R` | Mandatory fixture, oracle, coverage, regeneration and representative real-R6 tests |

```sh
# Stage 11C: metadata only; never overwrite numerical fixtures or references.
Rscript --vanilla dev/jamovi/e2e/generate.R --metadata-only
# Alternative output directory supports a read-only comparison with tracked artifacts.
Rscript --vanilla dev/jamovi/e2e/generate.R /tmp/flexplot-e2e-artifacts
# Existing mandatory development validation includes the new tests automatically.
Rscript --vanilla dev/jamovi.R ui
Rscript --vanilla dev/jamovi.R test
```

No new dependencies: base/recommended R, MASS, lme4, yaml, jsonlite and existing
development/test tooling. No backend or app dependency is introduced to packaged
tests. These development files are already excluded by `.Rbuildignore`.

Generation restores the caller's RNG kind and state. It is deterministic on the
recorded R/package versions. Regeneration on another platform can give slightly
different optimizer estimates; compare numerical tolerances, not binary RDS
hashes. Never accept changed numerical reference files solely because generation
finished. CSV fingerprints and typed values must remain identical for an unchanged
DGP. Inventory regeneration is required after changing an option or result schema.

## Data-generating process

The primary dataset has **436 rows, 34 columns**, seed **20261120** with explicit
Mersenne-Twister/Inversion/Rejection RNG choices. There are 24 clusters (`id`),
sizes 12–27, assigned by repeating sizes 12:27 to length 24. No missing values or
pathological variables enter ordinary scenarios unless explicitly selected.

Independent standard-Normal predictors are `x` and `z`. `correlated` equals
`0.85*x + sqrt(1-0.85^2)*independent_Normal`. Categorical `binary` has levels No,
Yes, with b=1 for Yes. `group` has deliberately nonalphabetic levels middle,
low, high; their effects g are 0, -0.6, 0.9. `ordered` preserves that exact order.
Both factors are approximately balanced and randomly permuted across clusters.

Let `eta = 0.4 + 0.8*x - 0.45*z + 0.7*b + g`.

| Outcome | Population construction |
| --- | --- |
| `y` | eta + independent Normal SD 0.6 |
| `quadratic_y` | eta + 0.85*x² + independent Normal SD 0.6 |
| `interaction_y` | eta + 1.1*x*z + 1.3*x*b + independent Normal SD 0.6 |
| `binary_y` | Bernoulli(plogis(-0.35 + 0.65*x - 0.3*z + 0.45*b + 0.5*x*b)) |
| `factor_y`, `ordered_y` | Same binary outcomes, levels No=0, Yes=1, unordered/ordered respectively |
| `count_y` | Poisson(exp(0.4 + 0.3*x - 0.15*z + 0.2*b)) |
| `nb_y` | Negative Binomial, mean exp(0.5 + 0.35*x - 0.2*z + 0.3*b), size 1.5 |
| `gamma_y` | Gamma shape 6, mean exp(0.8 + 0.2*x - 0.15*z + 0.2*b) |
| `gamma_inverse_y` | Gamma shape 10, mean 1/(2 + 0.12*x + 0.1*z); diagnostic prediction domain must stay positive |
| `random_intercept_y` | eta + u[id] + independent Normal SD 0.45 |
| `mixed_y` | eta + u[id] + v[id]*x + independent Normal SD 0.45 |
| `category_y` | Three-level: P(Disagree)=0.3, P(Agree)=0.7*plogis(0.3 + 0.8*b + g), remainder Neutral; prevents binary-magnet dispatch |

u is Normal SD 0.9; v=0.2*u + independent Normal SD 0.45. These are generated
random effects, not claimed fitted BLUPs. Realized estimates will differ from
population parameters. `row_id` provides a stable diagnostic identity and is not
a model predictor. Some deliberately reduced models omit true effects; that is
part of the test, not a reference-model defect.

Missingness is fixed by row index: `x_missing` every 17th row starting at 7,
`z_missing` every 13th starting at 3, `y_missing` every 19th starting at 11,
`binary_missing` every 23rd starting at 5. `constant` is 1; `x_duplicate` is exactly
2*x. Literal-name aliases exercise spaces, plus, backtick, backslash, function-like
text and the name `1`. `stop('must not execute')` is a literal column alias of x,
never executable input. Selecting aliases together intentionally produces
collinearity; ordinary scenarios do not do so.

Purpose-built datasets:

| Dataset | Rows × columns | Purpose |
| --- | --- | --- |
| `common_sample` | 240 × 5 | Exact existing seed-925 regression fixture; first 60 z missing, reducing 16 to 12 groups |
| `separation` | 40 × 2 | Perfect binomial separation; record glm diagnostics, not an assumed rejection |
| `rank_deficient` | 60 × 4 | x, 2*x and constant; explicit alias/invalid comparison |
| `sparse` | 4 × 4 | Empty factor combinations, unused levels, singleton cells |
| `singular` | 128 × 4 | Zero between-cluster random signal with fixed slope; boundary covariance fit |
| `invalid` | 30 × 7 | Nonbinary, negative/fractional counts, nonpositive Gamma, constant/nonfinite response |
| `paired` | 60 × 2 | Existing row-order pairing convention, exact After-Before difference 0.5 |

Use repository `small`, `small_logistic`, ToothGrowth, and the existing seed-925
fixture in their established unit tests; the new primary data supports cross-analysis
GUI reuse without replacing those tests. For CSV imports, Phase 2 must apply the
schema: automatic jamovi type inference must not silently reorder levels or
convert ordered factors to continuous numbers. Verify the imported values and
levels before running any analysis.

## Actual feature inventory

`inventory.json` contains **150 declared options** across the four analyses,
including implicit data, hidden compatibility state, defaults, allowed values,
UI conditions inherited from containers, and all result-schema items.
`coverage.csv` has one row per option. Links to existing tests are analysis-level
evidence pointers, not a claim that every assertion or native GUI interaction is
already covered. Exact current controls are retained in the inventory even when
backend behavior is legacy or unusual.

### Flexplot (`flexplota`): 16 scenarios

- Outcome, ordered predictor roles (`preds`), and conditioning variables (`given`)
  directly construct a plotting formula; no model-term builder or Model 2.
- Numeric fitted methods: Loess, Regression, Logistic, Polynomial, Cubic, Time
  Series. Bands (`se`), fitted-line suppression, and descriptive plots are
  separate controls. Do not compare LOESS/time-series output with an unrelated lm.
- Categorical options: jittered-density, boxplot, violin; median/quartiles,
  mean ± one SE, mean ± SD. Grouped geometry must be tested through this adapter,
  not inferred from the underlying renderer alone.
- Added-variable (`resid`) and paired-difference (`diff`) routes; pairing uses
  existing row order, not a selected participant ID.
- Opacity and sampling percentages, explicit x/y jitter, bins 2–6, ghost toggle
  (default true). Raw sampling must not change population summaries.
- Panel state enables ghost/bin/residual controls. First predictor's declared
  data type controls enabled fit versus categorical options. Asynchronous
  column-information responses need actual GUI checks after rapid reassignment.
- Numeric conditioning is binned; both categorical/numeric panel roles and
  association zero cells need checks. The current association independence model
  includes panel variables as main-effect margins; it is not redefined as a
  separate independence fit within each panel. No theme/custom-label GUI control exists
  in this analysis; JASP theme options are outside this inventory.

### Linear Model (`glmbasic`): 17 scenarios

- Outcome/predictor pool; native independent primary/second `Terms` builders.
  NULL means additive defaults; explicit empty terms mean intercept-only in R.
  Interactions have independent enable gates; native main-effect and interaction
  destinations must agree with the generated fitted model terms.
- Model 1 and Model 2 each have an initially unchecked polynomial switch and
  numeric squared-term destination. Selecting a square adds its linear term by
  default; removing the linear term deliberately can retain square-only terms.
  Disabling the switch clears squares; re-enabling does not recreate them.
- Comparison default off, separate x/two panels, second-axis grouping only for
  supported magnet comparisons. Numeric-outcome comparisons direct users to
  panels. Model terms in either fit define eligible display variables.
- Single-model x/two panels/group; automatic x if empty. Model plots, diagnostic
  plots, estimates and SE display are distinct. Legacy line and center values
  are retained compatibility state, with no native controls. Ordinary ghost lines
  and main/comparison label editors have independent retained state.
- Full coefficient table (including interactions and aliases), finite-df t CI
  and optional coefficient p. Conditional factor means/differences hold numeric
  covariates at means and other factors at modes. Two-level t, one-way Tukey–Kramer,
  general adjusted Bonferroni intervals; labels must identify the actual method.
- R² and model-space semi-partial R²: protect interaction main effects; report
  interaction increments; curvature increments and combined polynomial rows
  differ. Current comparison table's F-test p is not controlled by the
  coefficient `includeP` option; the plan records this existing behavior.
- Comparisons use common complete rows, independent model identities, AIC/BIC,
  approximate reciprocal BIC Bayes factors, nested F only. Aliased single fits
  retain explicit coefficients; nonestimable comparisons are rejected.

### Generalized Linear Model (`glmgeneralized`): 24 scenarios

- Fresh/waiting visibility requires outcome, predictor pool, and a usable model
  term or square. Invalid/removed selections must clear stale results.
- Independent primary/second term and square destinations with interaction and
  polynomial enable gates. Adding a square adds a main effect by default;
  clearing/removing its predictor removes the square. Controls and saved options
  must agree after fresh, prepared, reopened and legacy initialization.
- Fresh `family=auto` resolves the actual outcome: eligible binary values use
  Binomial/logit; other outcomes use Gaussian/identity. Hidden `automaticFamily`
  and `automaticFamilyOutcome` are derived observations, never native edits.
  Legacy omitted family restores Gaussian; explicit named families remain manual.
  Exact manual family values: `gaussian` identity, `binomial` logit, `poisson` log,
  `gammaLog` log, `Gamma` inverse (saved compatibility), `negativebinomial` log
  using MASS::glm.nb. No generic free-link selector or extra families.
- One family-aware Estimates table based on estimates.glm; logistic IRT and
  marginal categorical probabilities; undefined statistics blank. Raw coefficient
  CIs use t for estimated Gaussian/Gamma dispersion and normal for binomial/
  Poisson/NB. NB inference conditions on fitted theta. Supplementary quadratic
  limitations must be explained, not filled with fabricated numbers.
- Coefficient p is default off; no generic fit/likelihood tables. Model comparison
  remains a separate table with its supported nested likelihood statistics.
- Default continuous-x logistic overlay shows observed bin proportions and
  supplied-model adjusted response predictions; `raw` retains binary observations.
  Observed bin proportions are not asserted to be bin-matched to marginal curves.
- Binomial style/scale controls follow the effective fitted family, including
  Automatic. Main `se`/`plotStyle`/`plotScale` are independent of comparison
  `comparisonSe`/`comparisonPlotStyle`/`comparisonPlotScale`; hidden settings persist.
- Exclusive x/panel/color roles, exactly one x automatically selected where
  possible; only actual model predictors eligible. Comparison has one x and up
  to two panels, no group selector. Response-scale curves use the original models.
- Comparisons fit one common union-variable complete sample; nested likelihood
  LR/df/asymptotic p, nonnested/equivalent LRT blank. NB estimates theta separately
  for both fits. Invalid outcomes/domain/rank/nonconvergence need useful errors.

### Mixed Models (`mixedmodel`): 20 scenarios

- One outcome/one shared cluster, fixed predictor pool. Independently gated
  interactions and numeric squares default off. Switches remove selected terms,
  not merely hide them; checking alone reveals builders. Random slopes remain
  distinct from fixed-effect selection.
- Model 2 has its own fixed predictor selector, terms, gates, squares, random
  intercept/slopes and estimation method. First comparison initializes Model 2
  from Model 1 fixed predictors. NULL legacy options preserve saved terms.
- Correlated random intercept/slopes, intercept-free slopes, REML default or ML;
  no silent simplification. One grouping factor only; no GLMM families here.
- Both model-specific coefficient and random-variance tables; residual variance/
  SD; optional symmetric correlation matrices; normal-Wald fixed CIs; coefficient
  p column retained in the schema but hidden because no denominator-df
  approximation is imposed. Estimates toggles both model-specific sections;
  normal-Wald confidence intervals remain present, independent of legacy includeCI. Singular and
  convergence diagnostics are distinct and appear before estimates.
- Comparison enabled means **one common sample for all tables and plots**, while
  each original model keeps its requested ML/REML method. Comparison statistics
  use ML copies. AIC/BIC remain available for converged singular fits; current
  policy suppresses BIC BF for singular fits. No policy change is part of testing.
- Ordinary LRT p only for eligible nested fixed-effects comparisons with the same
  random design and no singularity. Random-structure boundaries get LR/df when
  valid, not an uncalibrated chi-square p. Nonnested LRT unavailable. Hiding p
  removes the whole comparison column and conditional random-p note.
- Main and comparison images/settings/label stores are independent; each offers
  population, cluster-specific (three random displayed clusters default; 1–20),
  CAS modes. Cluster-specific layout needs the cluster in panels/color; unsupported
  layouts must show plot-local errors. Main Model 1 retains its intended requested
  fit and prediction type instead of the ML inference copy. No mixed prediction
  confidence bands. Ordinary ghosts affect supported main plots only.
- CAS removes all original fitted random contributions (`y-Zb`), not population
  prediction substitution. Two CAS panels have their own model adjustment, shared
  axes/reference conditions. Optional dashed Model 2 fixed curve appears **only
  in Model 1**, with a concise conditional note, never a reciprocal ghost.
- CAS excludes cluster from visible plotting roles/supplier without changing
  model/data; other modes restore availability. Native label rows cover applicable
  axes, legends, facet titles and levels; hidden label store persists inactive
  labels. Sample note occurs once directly beneath model specifications.

## Numerical reference contract

Each fitted checkpoint names an **explicit formula**, engine, family, and requested
estimation. These are not assembled using flexplot's formula builder. Rows are
selected independently using complete cases across the union of those formulas.
For comparisons this matches the intended common sample; for standalone fits it
uses only that formula. Original row identities, N, residual df, coef, SE, full
vcov, coefficient CI/p, logLik and parameter count, AIC/BIC are recorded. Mixed
references include VarCorr, cluster count, singularity, original REML status,
conditional predictions, and independently adjusted outcomes.

Full-reference diagnostic marginal predictions substitute x=-1,0,1 across *all*
eligible rows and average response-scale `predict()` values. They are **diagnostic
coordinates**, not assumed jamovi grid points or bin midpoints. Phase 2 must
capture the actual plotted x, panel representative values, groups and selected
cluster identities and repeat the independent calculation there. Continuous
moderators must use flexplot's actual displayed representative definitions; do
not average a different panel distribution or invert an averaged link. No fixed
seed is claimed for production cluster selection: record which clusters were
selected and calculate their predictions from the original fitted model.

For linear-model factor summaries, independently construct reference profiles
(numeric means, other factor modes), design-matrix differences, `L beta`, and
`sqrt(L vcov L')`. Use t intervals for two levels, TukeyHSD equivalence only for
classical one-way ANOVA, Bonferroni t over all estimable adjusted pairs otherwise.
For unique R², fit reduced models on identical rows; follow interaction-protected
and polynomial model-space row definitions. Existing independent unit tests
provide these supplementary oracles; Phase 2 must port their comparisons to
captured GUI values, not infer correctness from coefficient agreement.

For comparisons, original-method coefficients are separate from ML likelihood
criteria. Reference BIC ratios are mathematical diagnostics; **do not require a
reported BF where current singularity policy suppresses it**. Check nesting and
boundary eligibility before comparing p-values. For aliased models, diagnostic
marginal profiles are omitted rather than extrapolating outside the estimable
model space. Missing/NaN cells must render blank; literal categories named like
missing tokens remain text.

Numeric tolerances: lm/GLM 1e-8, mixed fits 1e-5, plotted coordinates 1e-6. For
rounded GUI text, add half one final displayed unit; compare unrounded serialized
values whenever accessible. Exact checks apply to option identities, formulas'
model spaces, row IDs, levels, visibility and group/facet membership. A GUI image
or successful `.run()` is never proof of numerical correctness.

## Coverage and known gaps

Existing coverage is substantial but mostly R6/headless:

| Feature family | Existing evidence | Additional Phase 2 requirement |
| --- | --- | --- |
| Factor summaries, mean-SE, grouped categorical and association geometry | categorical/association tests; `test-flexplot-standard-error.R`, conditioning tests | Actual controls, type import and final displayed geometry |
| Full LM coefficients, nonstandard names, adjusted contrasts and unique R² | `test-jamovi-lm-coefficients.R`, `test-estimates-factor-contrasts.R`, semi-partial tests | Live GUI edits, rounded cells, visible adjustment notes |
| LM/GLM independent comparisons and quadratics | comparison and polynomial tests in package + dev suites | Drag/drop destinations, independent Model 2, restored state |
| Family estimates and uncertainty | generalized/reporting/expansion tests against independent fits | Family-switch visibility, no duplicate/stale tables, usable labels |
| GLM restoration defect | installed-handler `test-jamovi-glm-restoration.R` + saved protocol checks | Permanent GLM-08: fresh vs prepared vs actual disk save/reopen, followed by live edits |
| Mixed common-sample defect | `test-jamovi-mixed-sample.R`, both-model tables and saved options | Permanent MM-10: actual N/slope/cluster changes across every transition and plot mode |
| Random structures, ML copies and boundary reporting | mixed/comparison tests | Real supplier controls and concise diagnostics after updates |
| CAS adjustment/ghost coordinates | independent y-Zb and shared predictions tests | Completed actual combined plot, directional ghost and conditional note |
| Saved option messages, compiled handlers and note layout | mandatory `dev/jamovi/tests` | Full `.omv` filesystem round-trip and legacy migration, not merely protocol round-trip |
| Desktop ownership/cleanup | managed-session and workflow mocks | One registered owned document; monitor recorded separately; permission/timeout failures verified safely |

No comprehensive automated native drag/drop or keyboard campaign exists yet.
A prepared `.omv` alone bypasses important fresh-control paths. Physical placement,
clipping, focus disruption, actual save/reopen recalculation and transient cached
results require GUI observation. Headless type schemas do not prove jamovi's CSV
import preserved the coding. No retained pre-fix legacy `.omv` is assumed to exist:
if unavailable, record that gap and distinguish constructed legacy-option tests.

## Phase 2 implementation plan

1. **Preflight, headless first.** Run `dev/jamovi.R ui`, `test`, fixture/manifest
   validation and the release package suite. Build/install once using the existing
   isolated release workflow; record source commit, archive checksum, installed
   build-hash, module/app/R/package versions. Failed preflight blocks GUI execution.
2. **Document assembly.** Reuse `fixture.R`, `finalize-fixture.py`, `check-fixture.R`
   and real jmvcore protocol encoding rather than inventing an `.omv` format.
   Extend a separate fixture builder to load the new typed data and scenario
   options. Keep the eight-analysis quick smoke document intact. Validate the
   new document's data/factor schema and expected option round-trip headlessly.
3. **Bounded GUI driver.** Use the established registered-document workflow and
   `gui-session.js`; one process/window, secondary monitor preferred, no RStudio activation,
   no preference edits. Reuse the existing test window when safe. Refuse ambiguous
   ownership. Never close/replace an unregistered document. Use a native fresh
   analysis for `fresh`, prepared options only for `prepared_options`, and actual
   disk close/reopen for `saved_reopen`; these paths are not interchangeable.
4. **Deterministic scenario dispatcher.** Proposed CLI `e2e run --scenario GLM-08`
   or `--priority P0` (not implemented in Phase 1). Order by priority then manifest
   position. Reset/recreate the analysis between scenarios. Use native controls;
   directly setting backend options is only a diagnostic, never proof that a GUI
   interaction works. Defaults should be observed before applying selections.
5. **Three-level checkpoints.** After each important edit, capture visible control
   selections, underlying saved options, result status, fitted formulas/rows and
   numerical tables; capture plot coordinates plus a screenshot of the completed
   image. Read underlying options through an existing supported bridge if available,
   otherwise save/inspect a temporary `.omv`. Validate output independently.
6. **Completion and failures.** Require an engine-complete status, expected result
   update/option revision, and stable populated output; mere disappearance of a
   spinner or a cached image is insufficient. Use bounded polling (proposed 30s
   ordinary, 90s mixed/CAS; tune from measured baselines). On timeout capture options,
   status, error/log text, screenshot, last completed checkpoint and timings, then
   stop that scenario. Do not loop indefinitely or launch a replacement window.
7. **Reports and reruns.** Write one JSON record per scenario/checkpoint to ignored
   `.jamovi-dev/e2e-runs/<run-id>/`; human summary lists failures and explicit
   not-run/blocked states. Fields: build identity, data fingerprint, launch path,
   initial/actual/expected options, timestamps, numerical deltas/tolerances,
   warnings, saved file/hash, artifact paths, UI/statistical/visual outcomes.
   Screenshot paths and errors must not contain unrelated user-document data.
   Individual-ID reruns preserve prior records; do not auto-update references.
8. **Cleanup.** Keep a workflow registry and cleanup in failure/finalization paths;
   release only owned disposable analyses/documents. Never issue Quit or close
   user files; closing the last owned document may naturally end the host. Manual `open` commands retain their existing leave-open contract;
   the campaign gets explicit disposable ownership. If existing controls cannot
   report monitor placement, record it as unconfirmed; this does not block
   functional acceptance. Unambiguous ownership remains required. Do not move
   windows speculatively. Restore no RStudio focus.

Implementation estimate: small dispatcher/report layer; moderate native-control
and saved-document adapter work. The numerical/reference layer is ready. Reliable
nonintrusive native drag/drop, result revision detection, and underlying option
inspection are the principal Phase 2 engineering risks. Do not introduce a new
GUI dependency until existing installed-handler and desktop tools are assessed.

## Beta acceptance gates

- All 77 planned scenarios executed or explicitly dispositioned; all P0 scenarios
  pass on fresh and required restored paths. No P0 skip counts as a pass.
- All four analyses initialize and update; visible native controls match saved
  options at every checkpoint. No persistent waiting state, loading spinner,
  stale table/plot, missing interaction/quadratic coefficient, or duplicate result.
- Numeric differences within stated tolerances against realized-data independent
  fits; factor success coding, common rows, estimators and uncertainty methods
  verified. CAS y-Zb, per-model curves, directional ghost and note pass.
- Unsupported configurations produce understandable completed errors, not silent
  model simplification, fallback smoothers, or statistically invalid p-values.
- Actual save/reopen plus live edits passes for every analysis; legacy-document
  limitations explicitly documented. Cached initialization output is acceptable
  only if it is replaced by correct recalculated output within the time budget.
- Expected images, facets, ordering, bands/intervals, captions and notes are
  readable at normal window size; no unexplained footnote markers or blank areas.
- Repeated P0 run is stable on the same build. Full package tests, mandatory dev
  checks, and clean-package release checks pass. Remaining warnings/notes are
  classified and accepted explicitly, not suppressed by the GUI runner.
- One-window ownership and desktop restrictions upheld. Any blocked GUI access
  is reported as unverified; headless/R6 success is never described as GUI success.

Before automation starts, decide how to obtain a trustworthy live option/result
snapshot without bypassing native controls, retain any real legacy `.omv` files
available, and verify the single registered disposable window identity. Prefer
the secondary monitor, recording placement separately when unconfirmed.
Stage 11C changes no statistical design or dependencies.

The pre-Stage-11C matrix had 73 scenarios: GLM-22 added reference-panel ghosts;
LM-16 and GLM-23 added independent label editors. Stage 11C adds LM-17, GLM-24,
MM-19 and MM-20 (77 total). Their definitions are not live execution evidence.
Existing numerical reference files remain unchanged. This scenario requires a
separate `panel_ghosts` native GUI review with `source: "native_gui_review"`, a
nonempty `evidence` identifier, observed `visible` and `scale`, and (when enabled)
explicit `source_preserved`, `no_source_duplicate`, `destinations_verified`, and
`source_coordinates_verified` checks. Saved options or reconstructed R6 plots
alone cannot pass it; absent native review is blocked. These are planned GUI
checkpoints, not evidence of completed GUI verification.

LM-16 and GLM-23 are planned native label-edit/restoration checks. The selected
`label_option` identifies the ordinary or comparison editor. A checkpoint with
`expected_plot_labels` requires `plot_labels` evidence from `native_gui_review`,
a nonempty evidence identifier and the actual rendered `text` values. Without
that observation, the checkpoint is blocked. Hidden stores are checked through
saved options, never edited as user controls. Existing numerical references are
unchanged.

## Stage 11C evidence and indirect coverage contract

`initialization` sends deterministic import/schema and initial public native-control
entry to the first driver request. Subsequent `changes` must agree with expected
saved/control values. Hidden options are rejected as native edits. Label actions
use actual generated row identities, commit by blur/Enter, and require rendered
text rather than fabricated label keys.

`indirect_coverage` is an explicit option → checkpoint → semantic kind → observation
mapping. Kinds are dataset import, derived metadata, actual legacy protobuf
restoration and retained label stores. There are no blanket internal exclusions
or “all scenarios cover this hidden option” rules. Removing a real action removes
its coverage. Restoration source options are decoded with installed protobuf types
in native tests; actual host opening and save/reopen remain Stage 11D requirements.
`restoration.original_options` must equal the original serialized input, accompanied
by its original file/hash and actual host-open evidence.

New/revised checkpoints carry concrete `observation_contracts` and required reviews.
For each required key, submit `reviews[key] = {source: "native_gui_review",
evidence: "path-or-record-ID", value: true}` only after verifying the stated
rendered geometry, family/coding/independent fit, stores or migration behavior.
Missing evidence is blocked, a negative observation is failed, and R6 provenance
is rejected. `expected_derived_options` checks saved read-only fields without
requiring editable controls. `expected_ui` requires separately captured `ui_state`
values from native inspection. Every visible image must have an actual resource
and displayed-image evidence, even with the main image hidden. Hidden estimates
are explicitly not counted as numerically inspected; re-enable and verify them.
GLM numerical inspection recognizes actual SE column identifiers and the existing
three-decimal Gaussian SE presentation; confidence limits retain full-precision
checks. No numerical reference or tolerance in the stored oracles changed.

The full generator may fit references **only in a disposable output directory**
for deterministic comparison. Stage 11C writes just inventory.json, coverage.csv
and scenario-index.csv through `--metadata-only`; it never writes tracked numerical
CSV/JSON/RDS, fixture schema/provenance or approved SVGs. Native design tests verify
metadata-only output isolation, exact reference keys and all four module counts.


## Stage 11D.2 verification policy

Functional GUI verdicts require actual native actions, current output and safe
owned-document targeting. Secondary-monitor placement is a recorded operational
preference, never a correctness gate. `e2e_checkpoint()` returns a separate
`operations` record; unknown placement is `unconfirmed`. Non-owned or ambiguous
windows still block execution. Preserve Stage 11D.1's ledger; the separate
`stage11d2-policy-reassessment.json` removes only its monitor gate using original
evidence hashes and does not claim new-build GUI passes.

Mixed rendering errors can use only their declared adjacent notes. The image must
own the matching error and have no stale resource, while a current visible
companion contains the observed text and other valid results remain available.
Unrelated, hidden, stale, mismatched or duplicated companions fail verification.

GUI fixture checks default to exact source names/types/values/factor metadata.
For primary on jamovi 2.7.41 only, explicit `jamovi-2.7.41-import` policy maps
``tick`name`` to `tick_name`, rejecting any collision or other rename. Native
inline renaming can write the literal name, but opening the file normalizes it
again. `e2e_write_gui_fixture()` writes source doubles as double binary values and
sets matching Decimal metadata, including whole-valued columns. The existing
writer otherwise re-infers Integer metadata for these columns; conflicting widths
corrupt native imports even when its R reader succeeds. A metadata regression
check and actual host-saved typed-data comparison cover both paths. The helper
refuses to overwrite existing evidence. The
original deterministic fixtures and R-side nonstandard-name tests are unchanged.

Use native `getApp` followed by Open → Recent Files and verify the reopened title,
path and analysis. Keep system file-picker/Finder failures bounded. Request human
assistance when native binding or safe targeting remains unavailable; process
presence alone is not proof of a usable review window. No backend setter may
substitute for a failed native action.
