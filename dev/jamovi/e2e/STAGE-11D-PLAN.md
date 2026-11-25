# Stage 11D — current-build live GUI campaign

Status: planned, **not run in Stage 11C**. The matrix defines 77 scenarios and
267 transitions plus 77 final host reopen checks. All P0 must pass before release;
all P1 must be executed with recorded disposition. A blocked P0 remains a blocker.
Native/R6/renderer passes and historical pilot captures cannot replace these checks.

Stage 11D.1 update (2026-11-24): the [seven-scenario pilot](STAGE-11D1-PILOT.md)
has been executed, with failures and monitor-dependent blocks retained in its
[ledger](stage11d1-ledger.json). No remaining 70-scenario run was launched.
That historical result is unchanged. Stage 11D.2 resolves those pilot gates as
recorded in the [correction report](STAGE-11D2-CORRECTIONS.md) and
[correction ledger](stage11d2-ledger.json): four affected scenarios pass all
13 transitions, four actual host reopens and four subsequent edits; three other
pilot scenarios pass proportionate current-build smoke checks. One supplementary
main-error isolation check also passes (25 checks total). Monitor placement is
unconfirmed and recorded operationally. The process is ready to resume P0 batches;
the other 70 scenarios remain unexecuted and must not be treated as passed.

## Preflight and human ownership

Use installed jamovi 2.7.41; prefer the desktop secondary monitor. Confirm the running
module endpoint, version, build hash and archive/install receipt with the existing
preflight, not just the repository version. The Stage 11D.2 corrected production
fingerprint is `54081947f58fe59e1543ca1c1e21f3828ee34464c719ee23ba84ac7808c42d63`. Use the approved macOS ARM64 R 4.5.0 / ggplot2 4.0.3 / vdiffr
1.0.8 environment for reference comparison; never approve changed SVGs for a GUI
mismatch. Prepare deterministic GUI inputs with `e2e_write_gui_fixture()` from the original
typed RDS, or apply data-schema.json after CSV import. The existing writer needs
matching Decimal metadata and double binary storage for whole-valued doubles;
R-reader round-trip alone does not prove native import integrity. Preserve factor order,
missing values and types; never regenerate source numerical artifacts for a GUI
import difference. jamovi 2.7.41 normalizes the primary fixture's literal backtick
name to `tick_name` on opening, even after a successful native rename/save. Only
explicit `jamovi-2.7.41-import` verification may map that one source spelling;
`e2e_gui_fixture()` otherwise requires exact names, classes, values and factor
metadata and rejects collisions. Exact-name checking remains the default.

Require exact identity and unambiguous ownership of the disposable document and
active analysis. Record monitor placement separately as secondary, other or
unconfirmed. Unknown placement alone never blocks functional acceptance. Reuse
one owned window; do not move unrelated windows or change display settings. Only an explicitly registered second
owned window is permitted for restoration; close it after verifying the reopen.
Do not close user documents. Prefer native `getApp` and Open → Recent Files for actual reopens, verifying the
resulting title, document and analysis before editing. A newly prepared owned file
may be selected in the already observed in-app directory list. Avoid repeated
failed system-picker/Finder attempts. Native drag/drop destinations, dynamic
label cells or offscreen targets require fresh visible confirmation; request human
help when safe targeting cannot be established. Mark unavailable actions blocked,
never replace them with backend setters. Record binding/launch timeouts separately
from product errors; an observed running process is not document ownership.

Before each scenario enter initialization options through native controls, then
follow steps in order. Reset to fresh owned analysis between scenarios. Keep the
original IDs and 156 independent formula/reference keys. New scenarios use the
same deterministic data and independent stats::lm/stats::glm/lme4::lmer checks described in
their observation contracts; do not alter stored reference outputs.

## P0 execution order and release gates

1. Initialization/reopen and regression smoke: FP-03, LM-03, GLM-08, MM-10,
   then current-build error isolation/recovery LM-15, GLM-20, MM-15. Injected
   renderer exceptions from Stage 11B remain native tests; do not claim the GUI
   naturally reproduced them. Capture invalid image diagnostics, valid other plot,
   intact tables, spinner completion, cleared stale resources and valid recovery.
2. Independent outputs and labels: LM-11, LM-16, GLM-23, MM-19. Verify all
   main/comparison visibility combinations, immediate explanations, retained
   labels and settings, same estimates after re-enabling, and stable result order.
   Mixed main predictions must use intended Model 1 REML/ML and prediction type,
   independently of ML comparison inference. Verify conditional versus population
   curves at captured coordinates, ordinary main ghosts and directional CAS ghosts.
3. Migration and family provenance: LM-17, GLM-24, MM-20, then GLM-22 ordinary
   probability/logit ghosts and MM-11 directional CAS. Build disposable legacy
   inputs from the declared saved options using installed protobuf types; retain
   original encoded messages and host-readable OMV before opening. Never edit
   derived Automatic metadata through a native control. Verify both explicit and
   omitted legacy Gaussian, Binary Automatic and manual override.
4. Finish every other P0 below. Perform each scenario's final host save, close,
   reopen and subsequent live edit. Prefer small reviewed batches over an
   unattended whole-matrix run.

### Complete P0 inventory (39 scenarios)

- flexplota: `FP-01`, `FP-09`, `FP-10`.
- glmbasic: `LM-01`, `LM-02`, `LM-03`, `LM-07`, `LM-10`, `LM-11`, `LM-15`, `LM-16`, `LM-17`.
- glmgeneralized: `GLM-01`, `GLM-02`, `GLM-03`, `GLM-04`, `GLM-05`, `GLM-06`, `GLM-07`, `GLM-08`, `GLM-11`, `GLM-18`, `GLM-20`, `GLM-22`, `GLM-23`, `GLM-24`.
- mixedmodel: `MM-01`, `MM-02`, `MM-03`, `MM-05`, `MM-06`, `MM-07`, `MM-10`, `MM-11`, `MM-14`, `MM-15`, `MM-16`, `MM-19`, `MM-20`.

### Complete P1 inventory (38 scenarios)

- flexplota: `FP-02`, `FP-03`, `FP-04`, `FP-05`, `FP-06`, `FP-07`, `FP-08`, `FP-11`, `FP-12`, `FP-13`, `FP-14`, `FP-15`, `FP-16`.
- glmbasic: `LM-04`, `LM-05`, `LM-06`, `LM-08`, `LM-09`, `LM-12`, `LM-13`, `LM-14`.
- glmgeneralized: `GLM-09`, `GLM-10`, `GLM-12`, `GLM-13`, `GLM-14`, `GLM-15`, `GLM-16`, `GLM-17`, `GLM-19`, `GLM-21`.
- mixedmodel: `MM-04`, `MM-08`, `MM-09`, `MM-12`, `MM-13`, `MM-17`, `MM-18`.

## Save/reopen, asynchronous checks and evidence

For every scenario use native Save As to a unique owned OMV, capture the original
hash/options, close and reopen through the host, verify imported factor metadata,
options, recalculated formulas/sample/uncertainty, both plot resources and labels,
then make the next declared live edit. Legacy scenarios additionally retain the
pre-migration original file/protobuf, compare its actual options with the manifest,
inspect the current marker and explicit independent settings after two reopen
cycles, and verify no one-time copy repeats. Prepared R6 files count as inputs only.

GLM-24/2 requires rapid y → factor_y → binary_y native actions plus observable
metadata request/reply identities. A controlled native-driver delayed reply is
needed for deterministic stale-reply coverage. If the host cannot delay the old
reply without backend mutation, record that subcheck blocked and retain the passing
native stale-response test separately. A final binary fit alone does not prove
that an old response was rejected. Human observation is needed for control enabled/
visible state, event coding, legends, panel source/reference geometry and focus.

For each transition and restored checkpoint retain timestamp, scenario/step ID,
source fingerprint, jamovi/OS/monitor context, request, fresh accessibility/DOM and
screenshot, owned window/document identity, actual host-saved OMV plus SHA256 and
inspector JSON, current native completion state, displayed image/resource evidence,
control values/visibility, diagnostic phase/text/result, independent numerical
comparison and all required reviews with evidence paths. Record actual plot
coordinates, cluster IDs and source/destination ghost curves, not just a checkbox.
A rendering error may be visible in its declared adjacent companion note only
when the expected image owns that exact error, its stale resource is absent, the
companion is current/visible and other valid images/tables remain available. The
verifier accepts the known Mixed `plot`/`plotExplanation` and
`comparisonplot`/`comparisonPlotNote` relationships, not arbitrary HTML elsewhere.
The manifest's expected_ui and derived state require independent captures; missing
reviews are blocked and mismatches failed. `run.R verify` validates a single
observation; the file bridge never supplies missing human work automatically.

Maintain a separate campaign ledger with defined, native-component-tested,
live-attempted, live-executed, live-passed, failed, blocked and unexecuted columns.
Only a fully evidenced transition may pass; a scenario passes only after every
transition and host reopen. Keep failed/blocked captures and rerun provenance.
Keep Stage 11D.1's original ledger and verdicts unchanged. Record policy
reassessment separately with original evidence hashes and old source identity;
that reassessment is not a corrected-build GUI rerun. Do not promote older pilot passes. Release still requires isolated built-package
checks and actual-host JASP compatibility outside this jamovi campaign.

For new fitted states without a stored reference key, retain an independent R
calculation alongside the capture: LM-17/2 uses lm(binary_y ~ binary + group)
and lm(binary_y ~ binary) on their common rows; GLM-24 uses glm(outcome ~ x + z +
binary) with the observed outcome coding and declared effective family/link.
MM-19 uses existing MM-07/1 fit references, with independent predictions at the
captured coordinates and selected cluster IDs. MM-20 uses independent lmer fits
for its explicit restored fixed/random formulas and requested estimation methods;
comparison inference uses the existing eligible common sample and ML copies.
Do not add or overwrite stored numerical artifacts to record these GUI checks.
