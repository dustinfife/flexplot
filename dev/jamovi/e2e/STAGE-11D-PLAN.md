# Stage 11D — current-build live GUI campaign

Status: planned, **not run in Stage 11C**. The matrix defines 77 scenarios and
267 transitions plus 77 final host reopen checks. All P0 must pass before release;
all P1 must be executed with recorded disposition. A blocked P0 remains a blocker.
Native/R6/renderer passes and historical pilot captures cannot replace these checks.

Stage 11D.1 update (2026-11-24): the [seven-scenario pilot](STAGE-11D1-PILOT.md)
has been executed, with failures and monitor-dependent blocks retained in its
[ledger](stage11d1-ledger.json). No remaining 70-scenario run was launched.
Resolve the documented diagnostic, scenario/verifier contracts and placement
observability before accepting the pilot or starting unattended batches.

## Preflight and human ownership

Use installed jamovi 2.7.41 on the desktop secondary monitor. Confirm the running
module endpoint, version, build hash and archive/install receipt with the existing
preflight, not just the repository version. Production fingerprint remains the
Stage 11B.1 build. Use the approved macOS ARM64 R 4.5.0 / ggplot2 4.0.3 / vdiffr
1.0.8 environment for reference comparison; never approve changed SVGs for a GUI
mismatch. Import deterministic CSVs and apply data-schema.json, including factor
order, missing values and literal variable names.

Human operator confirms secondary-monitor identity, unique disposable document and
active analysis. Reuse one owned window. Only an explicitly registered second
owned window is permitted for restoration; close it after verifying the reopen.
Do not close user documents. Native file pickers, drag/drop destinations, dynamic
label cells and any ambiguous/offscreen target require human assistance. Mark an
unavailable target blocked, do not use backend setters to manufacture GUI success.

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
The manifest's expected_ui and derived state require independent captures; missing
reviews are blocked and mismatches failed. `run.R verify` validates a single
observation; the file bridge never supplies missing human work automatically.

Maintain a separate campaign ledger with defined, native-component-tested,
live-attempted, live-executed, live-passed, failed, blocked and unexecuted columns.
Only a fully evidenced transition may pass; a scenario passes only after every
transition and host reopen. Keep failed/blocked captures and rerun provenance.
Do not promote older pilot passes. Release still requires isolated built-package
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
