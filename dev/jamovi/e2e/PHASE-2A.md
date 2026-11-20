# Phase 2A harness and pilot status

**Status: validated partial infrastructure; GUI pilot remains incomplete.**

The stabilization task authorizes committing this infrastructure independently of
GUI pilot completion. See the final section and `phase2a-stabilization.json` for
the revised two-window policy and current verification. Earlier reports below
remain historical; their blocked/unexecuted checkpoints have not been promoted
to passes.
The current installed **and running** module is now verified as **0.27.0** against
its source fingerprint, retained archive, and installed file checksums. See
`phase2a-followup.json` for current-build evidence and remaining blockers. The
older `phase2a-pilot.json` records diagnostic observations on the previous 0.12.3
installation; those passes are not acceptance of the current build.

No full 70-scenario campaign was run. Production R, statistical calculations,
GUI definitions, source versions, and user-facing options are unchanged.

## Architecture

- `build-pilot.R` builds two **prepared** `.omv` starting documents from the Phase 1
  typed fixtures, using the existing smoke resource helper, actual R6 results,
  jmvReadWrite and `finalize-fixture.py`. It never launches jamovi. The shared
  serializer now supports Flexplot's absence of `modelTerms`; existing modeling
  assertions remain enforced. Prepared/cached results are not GUI evidence.
- `cua-driver.js` supplies native CUA actions and fresh accessibility inspection
  for an already-bound App. It verifies checkbox changes, blocks missing targets,
  and selects the semantic parent row rather than its nested label. Removal
  requires both the correct selected row and the explicitly named native Remove
  button. There is no destructive-key fallback. Duplicate destinations, wrong
  active analyses, wrong selected rows, and unobserved expansion are blocked.
  It does not launch, move, raise or close windows. Offscreen AX targeting remains
  unreliable, so this is not a validated unattended native driver.
- `inspect-omv.py` decodes the **actual saved host** AnalysisResponse protobufs,
  options, revisions, result visibility/errors, table values, notes, image resource
  presence and SHA-256 fingerprints. It reads one immutable byte snapshot of the
  file, not a live socket. Undefined numbers become JSON null. Dataset metadata
  is included. This is saved GUI state, not access to live R6 objects.
- `harness.R` consumes the Phase 1 manifest, drives action/observation callbacks,
  polls for observable completion within a deadline, validates native selections
  against saved options, and compares table coefficients/SE/CIs to the independent
  references. It records request reproduction steps, observations, reasons,
  numerical evidence, and JSON/text summaries. Unobserved required fields block a
  checkpoint; failures do not become skips. Pending checkpoints remain unexecuted.
- `run.R` provides per-scenario planning, inspection, verification and orchestration.
  The existing operation lock prevents concurrent development runs. The full
  campaign is deliberately not exposed. `check-inspector.py`, `check-cua-driver.cjs`
  and `test-e2e-harness.R` exercise isolated protocol/driver/failure fixtures.

No dependencies were added. All code remains under excluded development tooling.
The inventory snapshot changed only to include the new development test file.

## Commands

```sh
# Headless prepared starting files; does not open them.
Rscript --vanilla dev/jamovi/e2e/build-pilot.R
# Show the six selected pilot scenarios, or one exact scenario.
Rscript --vanilla dev/jamovi/e2e/run.R plan
Rscript --vanilla dev/jamovi/e2e/run.R plan GLM-08
# Decode actual host-saved options/results without opening any app.
Rscript --vanilla dev/jamovi/e2e/run.R inspect /path/to/disposable.omv
# Validate one recorded native checkpoint (nonzero on failure/block).
Rscript --vanilla dev/jamovi/e2e/run.R verify LM-03 2 /path/to/observation.json
# Request/observation orchestration, single scenario only.
# Supply the port observed in the existing GUI, not an assumed port.
Rscript --vanilla dev/jamovi/e2e/run.R run GLM-08 /tmp/owned-cua-bridge 90 http://127.0.0.1:PORT/modules/flexplot
# Mandatory headless regression checks; neither command launches a GUI.
Rscript --vanilla dev/jamovi.R ui
```

`run` requires a desktop controller to fulfill the file bridge using native CUA
controls. It is **not an unattended standalone macOS UI runner**. The controller
reads `request.json`, performs the exact requested native edit, observes completion,
saves through the native GUI and publishes `observation.json` atomically. The core
polls for observations, never treats elapsed time as successful completion, and
stops on failures, blocked actions or deadlines. It does not silently replace
native actions with backend option changes. Only a real native controller may
label an observation `native_gui`; synthetic driver tests are unit tests, not GUI
passes. A field declaring provenance is an audit trail, not cryptographic proof.

Observation contract:

```json
{
  "source": "native_gui",
  "checkpoint_id": "LM-03/2",
  "analysis_id": 4,
  "desktop": {"secondary_monitor": true, "window_count": 1, "owned_document": true},
  "native_status": "complete",
  "accessibility": "Fresh native control/result evidence, with access keys redacted",
  "visible_options": {"out": "quadratic_y", "polynomials": true, "quadratic": ["x"]},
  "control_visibility": {"plotX": false},
  "rendered_images": {"plot": true},
  "saved_file": "/path/to/owned-checkpoint-copy.omv",
  "reopened_in_gui": false
}
```

The example is abbreviated: every expected **visible** option must be supplied
from native inspection, not copied from the manifest or saved options. Closed or
hidden controls require explicit visibility evidence; their saved values are
still checked. A reopening checkpoint has an `/restored` ID and requires an actual
GUI close/reopen observation; a protocol round-trip cannot substitute for it.
Store captures and checkpoint copies in ignored `.jamovi-dev/e2e-runs/`. Do not
collect other documents or persist `access_key` query values. The GUI controller
must confirm the process/window already exists, is disposable and on the secondary
monitor before binding it. Never create or reposition a window to resolve a block.

Table checks cover every displayed coefficient, SE and coefficient CI for Model 1,
and Model 2 where the Mixed Models schema exposes its coefficient table. Logistic
raw coefficients use the existing three-decimal display tolerance; uncertainty
is compared at full precision. Model 2 LM/GLM coefficients are **not claimed checked**
when they are not displayed. This first harness does not yet extract numerical
plot coordinates or validate all supplementary/comparison statistics. Rendering
checks require both a saved resource and actual native display observation; a PNG
resource or reconstructed ggplot alone does not count. These limitations must be
closed or explicitly scoped before the full Phase 1 acceptance matrix can pass.

## Original pilot evidence (legacy installation, 2026-11-20)

Six scenarios selected: FP-03, LM-03, LM-09, GLM-08, MM-10, MM-11. Their 22 model/
plot checkpoints plus six reopening checkpoints total **28 required checkpoints**.
The machine-readable disposition is `phase2a-pilot.json`.

| Scenario | Actual installed GUI work | Disposition |
| --- | --- | --- |
| FP-03 | Confidence-band and suppression changes; restored fitted curve/band; completed scatterplots observed; host-saved options decoded | Checkpoints 2 and 3 passed; initial/reopen evidence incomplete |
| LM-03 | Enabled polynomial builder, added x using the native arrow, observed curved panel plots, then disabled polynomial terms; coefficients/SE/t CI matched independent lm references | Checkpoints 2 and 3 passed; initial/re-enable/reopen checks incomplete |
| LM-09 | Prepared comparison only; not exercised through controls | Not executed |
| GLM-08 | Native prepared controls showed x, z, x:z and x². Attempt to target interaction selected another supplier instead; intended interaction remained in the destination | Term-edit checkpoint blocked; full restoration unverified |
| MM-10 | Common-sample document generated headlessly; not opened | Not executed |
| MM-11 | Prepared CAS output present in document; no native comparison/ghost actions performed | Not executed |

Four checkpoints passed; ten are blocked/incompletely observed and fourteen were
not executed. There is no complete scenario pass or four-analysis GUI sign-off.
Actual saved checkpoints and verification logs are retained locally under
`.jamovi-dev/e2e-runs/pilot-evidence/`; screenshots were observed in the desktop
session. The tracked report does not embed local access tokens or user documents.

Only the user's existing secondary-monitor window was used; an empty document
was replaced through native Open. No extra jamovi document window was launched,
no monitor placement changed, no RStudio activation or unrelated-document actions
occurred. The disposable `primary` document remains open. Further GUI actions
stopped once reliable native term targeting could not be established. No automated
close/reopen was attempted without a reliable same-window ownership path.

The installed-version mismatch was discovered after the limited observations. No
installation/engine restart was attempted while native targeting was blocked.
Version equality alone is insufficient proof of identical source: retain a
fresh-install build receipt and stamp before accepting a future pilot.

## Findings relevant to safe automation

1. Native AX clicks on term/supplier items sometimes landed in a different list;
   a click that returns normally is not proof that its target was selected.
   Checkbox changes worked; adding a square required a screenshot-grounded native
   arrow click. The adapter now blocks unverified selections before sending keys.
2. Saving immediately after an edit captured **new options with old coefficients**.
   The independent coefficient-set check rejected it; after the native table update
   and another native save, the checkpoint passed. Do not infer engine completion
   from the `File Saved` message alone.
3. Saved child statuses can be normalized by the host and cached images can retain
   `stale=true` even after completed updates. Revisions can begin at zero and later
   advance. Neither a lone status/flag nor a fixed sleep is sufficient. Use native
   result-update evidence, current options, file/image fingerprints and independent
   values together. A failed early capture is not by itself a production defect.
4. An extra live InfoRequest socket calls `instance.set_coms()` in the installed
   server and can replace the GUI connection. That route was inspected in source
   and **not used**. Saved-state decoding avoids this interference.
5. Native import changed at least the unusual backtick column's displayed name to
   `tick_name`. Current pilot predictors use unaffected names. Validate imported
   names/levels before unusual-name scenarios; do not assume the prepared writer
   preserves every Phase 1 edge-case name. No production fix was attempted.

## Original validation and failure detection

Validation completed: **546 focused assertions** and **1,175 mandatory development
assertions**, with no test failures or skips. The installed-version preflight exits
with status 2 and a structured blocked reason. `git diff --check` passes. The
existing testthat package-built-under-newer-R startup notice remains environmental.

Focused tests regenerate Phase 1 fixtures/references, validate the harness and use
real installed protobuf types. Controlled temporary observations inject:

- Visible controls disagreeing with saved options: failed.
- Incorrect coefficient numbers: failed; CLI exits nonzero for that reason.
- Missing image/table or unchanged saved result after an edit: failed.
- Persistent running/waiting/no observation: bounded timeout, blocked, nonzero run.
- Missing actual reopening evidence: blocked; never counted as restoration success.
- Wrong supplier selected by a native click: blocked; no destructive key sent.

Positive unit controls and CLI controls pass. Negative CLI tests inspect both the
exit status and the specific failure reason, so an unrelated subprocess/path error
cannot masquerade as successful failure detection. Temporary files are removed;
production code, approved visual references, and fitted-model algorithms are never
modified. The GUI pilot itself is blocked, so automatic commit/sign-off is withheld.

Next step: resolve safe native targeting/active-analysis scoping and a verifiable
same-window save/reopen path, or provide a supported live inspection bridge that
does not take over the GUI connection. Then rerun these same six scenarios before
expanding coverage. No full campaign or production modifications are warranted yet.


## Follow-up: build identity resolved

The old installation was built at 18:03 on November 19, before commit `badcd51c`
(18:40), which synchronized `jamovi/0000.yaml` to the R package version. Its install
log reported R package 0.27.0 while writing `flexplot_0.12.3.jmo`. The compiler uses
module metadata independently; this was a stale pre-synchronization installation,
not an intentional second version convention. The old installation's complete
source contents cannot be proven from its version or timestamp.

| Stage | Authoritative version | Current verification |
| --- | --- | --- |
| R source | `DESCRIPTION` | 0.27.0 |
| Module source | `jamovi/0000.yaml` | 0.27.0 |
| Generated metadata | compiler-generated `jamovi.yaml` | 0.27.0 |
| Retained archive | `.jamovi-dev/artifacts/flexplot_0.27.0-7b72706aace3.jmo` | Module 0.27.0; R package 0.27.0 |
| Installed payload | module `jamovi.yaml` and `R/flexplot/DESCRIPTION` | Both 0.27.0; all 1,097 archived files match installed bytes |
| Running GUI server | read-only `/modules/flexplot` descriptor | 0.27.0 and matching source-hash stamp |

Analysis `.a.yaml` versions such as 1.0.0 describe analysis/saved-option schema
versions, not the module release; they were not changed.

`dev/jamovi.R install` now fails on unsynchronized source versions, fingerprints
build inputs, stamps the module, retains the archive, verifies installed bytes,
and writes `.jamovi-dev/build-receipt.json`. Source identity excludes generated
headers/translations and dev-only work, so a later testing-only commit does not
invalidate a valid build. This is a reproducible **source** fingerprint and exact
payload/archive verification, not a claim that zip/compiler artifacts are
bit-reproducible across environments. The fingerprint identifies dirty source as
well as committed source; Git HEAD is retained as provenance, not used alone.

- Source SHA-256: `836ee4ab183d8bcb85a9155140fbfd8635b4d762b17b51524cbced0a60fb3da5`
- Archive SHA-256: `2829353efc2378fabd43b36e2b9b1e22fcd21bda8fb359abde6b230291e4a615`
- Installed/archive payload SHA-256: `da50de62ade33753132465c8209fab252fd0ffd4e51597fc19fc613516d46bfe`

The runner additionally requires the existing GUI's observed local HTTP module
endpoint and compares its descriptor with the verified installation. A matching
version alone cannot pass. No live InfoRequest/WebSocket connection is opened.

## Follow-up: native targeting and pilot

The native selectable **text row** owns term selection; the nested label/container
does not reliably target that row. Selecting the parent row exposed the correct
explicit Remove button. Native GLM interaction removal and Model 2 squared-term
removal then worked. Full saved-option deltas rejected collateral changes.
Initial host context fields (`.ppi`, theme, palette, decimal symbol, language) may
be classified separately only when absent from the before snapshot and explicitly
permitted; existing user options are never exempted.

However, some AX section-button clicks and AX scrolling reported success while
leaving the intended control unchanged or focusing Hide options. A screenshot-
grounded section click/scroll enabled limited further CAS testing; no guessed
coordinates, destructive keys, or backend option setters were used. The adapter
now blocks an expansion that was not observed. Safe unattended targeting is
therefore **not fully resolved**.

| Scenario | Current-build evidence | Full checkpoint disposition |
| --- | --- | --- |
| FP-03 | Not rerun after verified installation | Not executed |
| LM-03 | Not rerun after verified installation | Not executed |
| LM-09 | Not exercised | Not executed |
| GLM-08 | Removed Model 1 interaction; removed Model 2 square; comparison off/on; native controls, saved options and all displayed coefficient/SE/CI values checked | Four checkpoints partially observed; blocked on complete native/render evidence; initial and reopen incomplete |
| MM-10 | Separate common-sample document not opened | Not executed; safe single-window dataset switch unresolved |
| MM-11 | CAS baseline rendered; ghost on displayed a dashed horizontal Model 2 prediction in Model 1; ghost off; only casGhost changed; both models' displayed coefficient/SE/CI values checked | Three checkpoints partially observed; complete Model 2 panel inspection and reopening blocked |

The transient Mixed Models waiting message after a live change resolved to a
completed model and CAS output; it was not treated as a persistent production bug.
Seven saved host states matched independent numerical references. Six native edits
had exactly their intended model-option changes. Detailed local evidence is in
`.jamovi-dev/e2e-runs/current-pilot/`; the tracked follow-up report preserves the
blocked status rather than fabricating missing visible-control captures.

All six required reopening checkpoints remain blocked. Inspection of the installed
frontend showed that Open on a nonblank document uses `host.openWindow()`; only
blank/unedited documents use same-window navigation. The native menus expose no
verified same-window document-close/reopen operation. We did not invoke Open on
the nonempty document, close/relaunch the owned window without a reliable monitor-
placement guarantee, or relabel a page refresh as save/reopen verification.

There are **zero fully passed current-build checkpoints**, 13 blocked (seven
partial observations plus six required reopening checks), and 15 not executed.
This is deliberately conservative: numerical/control subchecks do not constitute
complete GUI scenario passes. No full campaign or local commit was created.

Only the original secondary-monitor window was used. No new windows were opened
or moved, no RStudio activation occurred, and no unrelated document was changed.
The disposable primary document remains open with the CAS ghost disabled.

## Follow-up validation

- Focused Phase 1/reference/harness tests: 559 assertions; zero failures/warnings/skips.
- Mandatory UI/development validation and installed-handler tests: 1,190 assertions;
  zero failures/warnings/skips.
- `dev/jamovi.R test`: 3,312 package jamovi assertions; zero failures/skips,
  one ggplot2 `element_line(size=...)` deprecation warning attributed to jmvcore
  in `test-jamovi-mixed-cas.R:36`. jmvcore also printed oversized plot-state
  diagnostics (up to about 17 MB). No production changes were made for these.
- Mocked running/nonrunning install/open/cleanup workflow: passed; no GUI launched.
- Controlled option, numerical, stale/missing result, waiting/timeout and restoration
  failures remain detected. New tests cover wrong parent selection, ambiguous
  destinations, adjacent-row false positives, unobserved expansion, source edits,
  unsynchronized versions, mismatched installed bytes and stale running metadata.
- No runtime dependencies were added. Existing compiler JSZip is reused.
- The environment still prints testthat's built-under-R-4.5.2 startup notice.

Next step: establish a supported same-window document lifecycle and reliable
onscreen section/supplier targeting, then rerun these six complete scenarios.
Do not accept this partial pilot as GUI release sign-off.


## Stabilization and revised window policy

The infrastructure is being retained as a **partial, controller-assisted harness**,
not a completed GUI test campaign. Its tests cover build identity, exact option
and numerical comparisons, explicit failure/blocked statuses, and native targeting
failure detection. No production R, GUI definitions, distributions, or statistical
calculations were changed. The original uncommitted Phase 2A files are preserved.

A second disposable document is now permitted **only for save/reopen verification**.
Native Open may open that second window; do not force same-window navigation.
The native controller must:

1. Keep the original secondary-monitor document open. Identify a unique disposable
   copy path for reopening; do not reuse an unrelated/user document's name.
2. Record exact document paths, observed native instance/window identities and
   titles. Confirm secondary-monitor placement independently; an archive, filename,
   successful Open command or screenshot without monitor information cannot prove
   monitor placement.
3. Register observed windows with `e2e_register_window()`. A second window requires
   purpose `save_reopen`; a third, ambiguous identity or unconfirmed monitor is
   blocked. Include `window_registry` and `active_window_id` in the checkpoint's
   desktop observation. The runner stores that observation in its per-action report.
4. Inspect the reopened native controls/results and make live edits there. Read
   the host-saved state only as complementary evidence. Do not claim archive-only
   decoding as a GUI restoration pass.
5. Before cleanup, call `e2e_authorize_window_close()` against a fresh native
   observation. Close only the exact newly created test window. The original
   reused window, unmatched titles/paths/identities and moved/unconfirmed windows
   are rejected. Confirm closure natively and retain the closed record as an audit
   trail. Never use Close All, quit the application or delete a user document.

These helpers authorize/validate evidence; they do **not** themselves operate the
OS or establish monitor placement. A desktop controller is still required. Do not
invoke the general development `open` command as a substitute for this restoration
sequence: its existing managed-document replacement policy serves a different
workflow. No existing general install/open/cleanup behavior was broadened.

`desktop.window_count` counts registered disposable test windows, not unrelated
user windows. The original one-window observation contract is retained. Two-window
checkpoints require a complete registry and matching active identity. Scenario
summaries now distinguish `attempted_checkpoints` from `passed_checkpoints`; a
blocked action does not count as a completed success.

Prepared pilot creation also refuses to overwrite existing `.omv` evidence unless
`--force` is explicitly requested. Use that option only for intentional regeneration
of closed disposable documents; ordinary testing must preserve an open fixture.

### Targeted restoration disposition

The user confirmed the new test window was on the secondary monitor. Both analyses
were then inspected in that **actual reopened jamovi document**, not reconstructed
R6 objects. `phase2a-restoration.json` records the detailed evidence and limits.

- **GLM — targeted checks passed:** Model 1 restored x/z, x:z, and square x;
  Model 2 restored independent z and square z. Native parent-row/Remove actions
  then removed Model 1's interaction and Model 2's square independently. Updated
  coefficient tables and formula notes were observed and host saves decoded.
  Coefficients, SEs/CIs, and both models' AIC/BIC matched independent references at
  the existing reporting precision. GLM plot-display checks were not performed
  during this restoration session; its full pilot checkpoints remain blocked.
- **Mixed Models — targeted checks passed:** fixed effects, independent Model 2
  predictors, random intercept/slopes, REML, and CAS x/panel/group selections
  restored correctly. Empty interaction/polynomial selections remained empty.
  Both models' coefficients/SEs/CIs, variances/SDs and ML comparison AIC/BIC matched
  independent references. Nonempty mixed interaction/quadratic restoration was
  not exercised.
- **CAS rendering passed for this case:** after reopening, ghost on/off changed
  Model 1's dashed Model 2 curve; Model 2 remained its own solid horizontal curve
  without a reciprocal ghost. The actual panels were inspected sequentially with
  the results scrollbar. No separate reopening with the ghost already enabled
  was performed. Numerical curve-coordinate extraction is still outside this
  partial harness's acceptance coverage.
- Only the intended options changed (`modelTerms`, `comparisonQuadratic`,
  `casGhost`). On the first GLM edit, initially absent host context fields were
  explicitly classified as initialization. No other model options changed.
  The saved binary dataset checksum remained identical.
- One disposable copy, `phase2a-restore-28cc94f8` (native instance
  `4daa1ece-9b97-4126-90ac-59094c0ba1e0`), was opened. Its exact title/path/instance
  were recorded. After verification, the unique native close button was used only
  after an ownership check; the original `primary` instance was then observed.
  No unrelated window was closed, no window was moved by automation and RStudio
  was not activated. At most two disposable documents were open.

A click can return before the accessibility tree updates. Some checkbox clicks
only established focus. The adapter now provides an explicit `focusedCheckbox()`
Space action, guarded by the exact native focus identity; it is **not** an automatic
second toggle after a pending mouse click. Wrong or unobserved focus blocks keys.
Horizontal wheel scrolling did not reveal Model 2; the screenshot-identified
results scrollbar did. These remain controller-assisted interactions, not proof
of a fully unattended runner. Earlier reports and unexecuted scenarios retain
explicit statuses. No full 70-scenario run occurred.

The next step toward the 30 high-risk scenarios is to finish the remaining native
plot and term-targeting gaps in the six-scenario pilot, including the separate
common-sample fixture. Then proceed through small manifest-prioritized batches. Keep accessibility failures blocked until
reliable targeting is available; do not expand a nominal pass count with archive-
only evidence or back-end option setters.

Current automated results and limitations are in `phase2a-stabilization.json`.
The infrastructure can be committed while the full pilot remains incomplete;
that commit is not release or full GUI coverage sign-off.


Post-restoration focused checks passed again: 578 assertions, no failures/warnings/
skips. The final mandatory UI/development check result is recorded in the
stabilization report. This work does not claim complete GUI scenario coverage.
