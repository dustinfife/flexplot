# Stage 11D.1 — current-build live GUI pilot

The seven-scenario pilot started from `d97541e898877755266b3ceba42d02cdd17648eb`
on `ggplot-objects`, using jamovi 2.7.41 and flexplot 0.27.0 on the desktop
secondary monitor initially. Execution finished with failures and blocked
acceptance gates; this is not a release sign-off. The [campaign ledger](stage11d1-ledger.json) and
[local evidence hash index](stage11d1-evidence.json) record current-build
observations; historical Phase 2A captures are not promoted to current passes. The other 70 scenarios are outside this pilot.

## Build and evidence identity

Installed and running module preflights verify source SHA-256
`68964d65afd6f88cbf4697301d09edc4d32412ad773ddcb1298cd965fb317550`,
archive `dc64a566a7373b2989f2fc60530f840ea5ae5c086931229df02a172229bc490c`
and payload `8c35856734293d976adf70dbc19d82d5e331c63763f809c42db19c72de8aa779`.
The existing install matches current build inputs; no rebuild was necessary.
Read-only localhost module descriptors were checked after host restarts.

Large evidence remains local and ignored under
`.jamovi-dev/e2e-runs/stage11d1-20261124/`: redacted AX captures, screenshots,
actual host-saved OMVs, decoded saved responses, original-reference verification
results and supplementary independent comparison/plot-coordinate reviews.
Files prepared headlessly are explicitly inputs only. Actual native opening,
control actions, output inspection and saving are required for GUI acceptance.
One owned disposable document was used at a time; unrelated windows and settings
were preserved. The human confirmed placement through the LM-15 legacy input.
After the user requested autonomous opening and left, native `getApp` plus
Open → Recent Files successfully reopened the owned documents. The bridge does
not expose monitor coordinates; fresh secondary-monitor placement is unconfirmed
for the final LM-15 reopen and subsequent GLM-20/MM-15 observations. These gates
are recorded as unobserved, not inferred from the previous window. The final
owned document was saved and closed.

## Interpretation and numerical scope

The manifest, rather than abbreviated prompt captions, determines execution:
LM-03 tests quadratic gates and hierarchy. GLM-20 tests role reassignment,
predictor removal, empty-term waiting and recovery; it does not configure an
invalid comparison. This pilot cannot claim that additional comparison coverage.

Fixed coefficient/SE/CI comparisons use original independent references and the
existing harness tolerances. Only actually displayed coefficient tables are
claimed: LM/GLM Model 2 coefficients are not exposed as separate coefficient
tables. Supplementary saved comparison tables are checked independently for N,
AIC/BIC and reciprocal BIC Bayes factors. GLM published values use their existing
three-decimal precision; Mixed table values use the existing mixed-model numerical
tolerance. Nested LR/df checks use independent original ML log-likelihoods and
parameter counts. A hidden p column is not a visible p-value check.

FP-03's actual blue line agrees with the independent fitted regression to within
1.13 pixels at calibrated image coordinates (2-pixel visual tolerance). Its
suppressed state has no blue line; restored line/band are visibly inspected.
MM-10's actual standalone conditional red curves at x=0 for clusters 4, 6 and 13
agree with original independent REML conditional predictions within 0.67 pixels.
These visual precision checks are separate from full-precision table checks.

## Final disposition

All seven scenarios were attempted in order, with all 27 declared checkpoint states
inspected, seven actual final host reopens and seven subsequent native live edits.
LM-15's hidden-group clearing action was unavailable; its partial native step is
not promoted to a completed action. Strict verification counts include monitor
placement and the unchanged scenario/verifier contracts:

| Checks | Passed | Failed | Blocked | Total |
| --- | ---: | ---: | ---: | ---: |
| Declared transitions | 18 | 5 | 4 | 27 |
| Final host reopens | 4 | 1 | 2 | 7 |
| Post-reopen edits | 4 | 1 | 2 | 7 |
| Total | 26 | 7 | 8 | 41 |

There are no wholly unexecuted pilot checkpoints. The other **70 scenarios remain
unexecuted**. GLM-08's native preparation check is excluded from these counts; its
required initial actual host reopen is part of declared step 1.

| Scenario | Strict disposition | Functional evidence and limitation |
| --- | --- | --- |
| FP-03 | Failed | First schema review fails host literal-name normalization; later plotting, reopen and edit checks pass |
| LM-03 | Passed declared checks | All four quadratic transitions, final reopen and native square edit; independent coefficients/SE/CI pass |
| GLM-08 | Passed declared checks | Both prepared/restored formulas, five transitions, final reopen and edit; independent estimates and comparison statistics pass |
| MM-10 | Passed declared checks | 240/16 → 180/12 → 240/16 → 180/12 → 240/16; original REML versus ML comparison references, reopen and edit pass |
| LM-15 | Failed | Expected restored warning, new panels and statistics work; hidden retained comparison group cannot be cleared as required; later monitor gate unconfirmed |
| GLM-20 | Failed and blocked | Native waiting clears stale roles contrary to manifest; z-only recovery, reopen and edit numerically correct, later monitor gate unconfirmed |
| MM-15 | Failed and blocked | Main error message absent; comparison isolation visibly correct but companion-note verifier limitation; population recovery/reopen/edit numerically correct, monitor gate unconfirmed |

The three declared-scenario passes do not waive the shared primary fixture's
unused literal-name normalization. The campaign as a whole is not accepted.
All seven restored datasets preserve every typed value, factor level/order and
ordered-factor flag. Only primary literal names have the documented host rename;
common-sample names are exact. See local `restored-dataset-preservation.json`.

## Findings

- **HOST-01 — literal-name import normalization:** the typed primary fixture's
  unused ``tick`name`` column becomes `tick_name` in the actual host document.
  FP-03's strict dataset-schema review fails. Its selected x/y values are unchanged,
  later plotting checkpoints pass, and the initial schema requirement is not
  waived. This is a host/fixture-contract incompatibility, not a fitted-model
  numerical mismatch. Preserve the existing fixture until a deliberate contract
  decision; do not silently approve its renamed column.
- **AUTO-01 — native lifecycle/file opening:** closing the owned document can quit
  jamovi and return `noWindowsAvailable`, `App quit` or screenshot-stream errors.
  The optional `launch_app` API is absent; native `getApp` can relaunch the host.
  A Finder-resolution fallback stalled and was abandoned; early reopens required
  the human. Later `getApp` and native Recent Files worked,
  after the user explicitly requested autonomous file opening. These are
  automation/host lifecycle limitations, not model failures.
- **AUTO-02 — scrolled AX targeting:** some offscreen accessibility clicks leave
  the intended section unchanged. Fresh screenshots and visible coordinate targets
  recover the controls; no attempted click is accepted without observed change.
  A splitter drag had no effect and is not reported as a successful resize.
- **HARNESS-01 — temporary controller parsing:** a runtime checkbox helper initially
  counted the focused-element footer as another control. Restricting matches to
  numbered tree rows resolves it. No tracked harness or production change is needed.

- **CONTRACT-01 — LM-15 hidden preference:** step 2 deliberately restores
  `comparisonGroup="group"` while ordinary grouping is unavailable. Native panel
  assignment retains that preference by design. Step 3 requires NULL, with no
  native clearing control. Its exact option check and subsequent restored/edit
  checks fail; visual recovery and independent statistics pass. A corrected native
  action/expectation must be reviewed separately; this pilot changes neither.
- **CONTRACT-02 — GLM-20 stale role expectations:** removing every term clears
  `plotX` and `plotGroup` to NULL. The actual waiting message is visible and the
  old plot/table are cleared. Step 3 expects `z`/`group` retained, so fails.
  Restoring z gives the correct Poisson/log model and independent estimates.
- **UI-01 — main Mixed error not displayed:** native cluster predictions with
  explicit non-cluster binary panels produce the correct saved image error and
  no stale image resource. The GUI leaves a blank plot area without the expected
  message; fixed estimates/SE/CI remain correct. This is a user-facing presentation
  integration defect; module versus host-renderer responsibility is unassigned.
  Reproduce with MM-15/1 and the retained main-error screenshots before correction.
- **HARNESS-02 — companion diagnostic scope:** MM-15/2 visibly shows
  `comparisonPlotNote`: “Comparison plot unavailable: For cluster-specific
  comparisons, place the shared grouping variable in panels.” The corresponding
  `comparisonplot` owns the rendering error and has no old image resource. The
  verifier rejects the actual HTML diagnostic scope as non-image/no-own-error.
  Its raw result remains failed; do not relabel the message as image text. Native
  comparison-only isolation and both fixed tables/ML statistics are correct.
- **AUTO-03 — monitor placement after autonomous reopen:** thirteen observations
  explicitly set `secondary_monitor` to null. This blocks eight otherwise passing
  checks; five already failing checks retain their failures plus the unobserved
  desktop gate. Native document identity and actual reopen/edit work are observed.
  Capture sizes also varied between 1280×800 and 2560×1600; fresh screenshots were
  used to adjust targets, without changing display settings.

## Preservation and remaining gates

No production source, option/result identities, statistical methods, numerical
fixtures, SVG references or dependencies are changed. The inherited Stage 11C
baseline is 24,504 package assertions, 4,837 native assertions and 140/140 approved
visual comparisons, all passing. These suites are not rerun merely to collect GUI
evidence. All 156 preserved artifact/reference hashes are checked unchanged.

This pilot is not an unattended runner and does not complete the 77-scenario
campaign. Remaining P0 work needs reliable human file-opening/ownership support,
resolution of blocked/failed pilot contracts and the specific delayed-reply,
label, CAS/ghost and migration observations in the campaign plan. Isolated
built-package release checks and actual-host JASP compatibility remain separate
release requirements.

## Recommended correction sequence

1. Reproduce and resolve the main Mixed error-display integration finding;
   preserve plot-local isolation and the independently correct fitted results.
2. Review the LM-15 hidden-preference and GLM-20 empty-role contracts against
   intended native behavior. Correct companion-note verification by linking the
   visible message to its associated errored image, with focused negative tests.
3. Establish observable monitor placement for autonomous reopens and explicitly
   decide the literal-name host/fixture contract. Keep current failures intact.
4. Rerun affected pilot checkpoints and final reopen/edit gates. Then continue
   the P0 campaign in reviewed batches, with the specific label, ghost/CAS,
   migration and delayed-reply requirements in Stage 11D's plan.

No automatic remaining-campaign launch is authorized by this pilot. Existing
Stage 11B exception-injection tests remain component evidence; these natural UI
scenarios do not claim to reproduce injected exceptions.
