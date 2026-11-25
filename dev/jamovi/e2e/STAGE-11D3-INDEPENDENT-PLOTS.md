# Stage 11D.3 — independent outputs and labels

Executed on the desktop on 2026-11-25 from `1fe5c7b2`, branch
`ggplot-objects`, in actual jamovi 2.7.41. This evidence-only batch changed no
production code, tests, harness, manifest, dependencies or approved artifacts.
Strict acceptance is incomplete. The [ledger](stage11d3-ledger.json) records
**23 passed / 3 failed / 1 blocked / 13 unexecuted** of 40 planned checks
(32 transitions, four final reopens and four subsequent edits). Initial in-flight
captures and investigation probes are retained separately and excluded from totals.

| Scenario | Declared transitions | Final actual host reopen/edit | Disposition |
| --- | --- | --- | --- |
| LM-11 | 7 passed, 1 failed | Both passed | Required comparison explanation absent at step 6 |
| LM-16 | All 5 passed | Both passed | Full scenario passed |
| GLM-23 | 6 passed, 1 failed, 1 blocked | Unexecuted after findings | Title-context contract failure and unresolved saved-image discrepancy |
| MM-19 | Step 1 passed, step 2 failed; 3–11 unexecuted | Unexecuted after finding | Required generated Plot title row absent; scenario stopped |

## Preflight and operations

Installed and running 0.27.0 matched the receipt and production source fingerprint
`54081947f58fe59e1543ca1c1e21f3828ee34464c719ee23ba84ac7808c42d63`, archive
`2a4ec202a5fe4cbedd407e8f2add73edd0b4655a21f5db4b906ecfbccec3a88e` and payload
`942478e722d2baf0b8bc93412fbfee6ab98bd038b01a3d93f977b6919d0e5df6`.
The observed running server descriptor passed read-only preflight. No rebuild was
needed. Native controls performed all attempted transitions; no backend option
setter substituted for an unavailable control.

One explicitly owned disposable document, `/private/tmp/flexplot-stage11d3-review.omv`,
was reused with fresh native analyses. Native getApp/Recent completed both LM final
host round trips and subsequent edits autonomously. No human assistance was needed.
Every capture records exact owned window/analysis identity. Monitor placement is
**unconfirmed**, independently of functional acceptance. No unrelated windows or
global settings were changed. The owned document was saved and closed at the end.
Actual host-saved typed-data checks passed, including the explicit, narrow
jamovi-2.7.41 backtick-name mapping. Original fixture data were not changed.

## Findings and stopping points

**CONTRACT-D3-01 — LM-11/6, explanation contract mismatch.** Native main output
was hidden, comparison image visible and comparison statistics hidden, with
estimates retained. The required observation also demands a comparison explanation.
The ordinary LM comparison has no explanation in this valid ungrouped state.
Its existing explanation path publishes actionable unsupported-layout warnings;
there is no generic note here. The strict checkpoint fails. This was recorded as
a contract/presentation requirement question rather than a fitting defect, and
steps 7–8 and the actual host round trip were completed without waiving step 6.
Decide the intended explanatory requirement before a separately authorized
contract or product correction; this batch did neither.

**UI-D3-01 — MM-19/2, missing generated title control.** Both editors are enabled,
but the comparison catalog contains only x/y axes, model legend entries and id
facets. There is no Plot title row, so the required native edit to
`Independent mixed comparison` cannot be performed. The actual saved catalog
confirms the absence in both main and comparison editors. Read-only source
inspection of `R/mixedmodel-labels.R` confirms that Mixed retains the legacy
unscoped catalog; that catalog does not generate title/subtitle rows. This is an
implementation capability gap against the declared title contract, not an
unreliable click. No title was injected through saved options. The affected
scenario stopped at step 2; its remaining CAS/ghost/configuration transitions,
final restoration and live edit are explicitly unexecuted. This also prevents
claiming the later required main title or comparison-title persistence checks.

**CONTRACT-D3-02 — GLM-23/8, comparison title context mismatch.** Probability/raw
comparison points, curves and intervals render, but its generated title row is
blank and the comparison image lacks `Comparison custom title`. The previously
committed title remains in the independent comparison store under the earlier
probability/logistic-overlay context. GLM deliberately keys labels by output,
outcome, family/link, scale and overlay meaning (`jamovi_glm_label_info()`); changing
to logit and then raw selects different rows. This is not lost storage or a
cross-copy into the main editor. It nevertheless fails the exact step-8 requirement
that both images retain their own titles. Product title policy versus the manifest
needs an explicit decision, with scoped-label compatibility retained. No expectation
was rewritten. No final GLM host reopen/edit pass is claimed after this finding.

**EVIDENCE-D3-01 — GLM-23/5, unresolved capture/resource discrepancy.** The native
`05.png` shows the main custom title and the native editors retain their respective
titles; saved rows/stores have the same main key and value. However, the main PNG
extracted from `05.omv` lacks that title despite `ANALYSIS_COMPLETE`. Step 6's
actual saved PNG has the title again. Initial image-hash comparison alone led to
an incorrect suspicion that comparison-only edits removed the title; direct image
inspection shows the reverse chronology. Do not treat that suspicion as a product
defect. Native Save is asynchronous, and the private capture helper copied the file
before taking its final screenshot. The evidence does not distinguish a save/copy
race from retained-resource serialization behavior. Step 5 is conservatively
**blocked** pending a settled-save recapture and actual host restoration investigation.
Its optimistic original review/result is retained as `05-initial-review-*`;
`05-result.json` and the campaign ledger supersede its acceptance status. The
step-6 plot/title observation itself is correct. A byte hash difference alone
never proves changed plot geometry or labels.

LM-16 and GLM-23 also produced initial `ANALYSIS_INITED` saves while native label
updates settled. Those `*-running*` probes are retained, not accepted. LM-16's
settled checkpoint and final actual reopen/edit succeeded. This operational timing
finding does not authorize replacing live acceptance with R6 restoration.

## Numerical and image evidence

Host-saved LM and GLM coefficients, SE/CI and applicable comparison N/AIC/BIC/Bayes
factors match independent realized-data references at their declared tolerances.
No detected numerical disagreement is hidden by the presentation failures.
Additional independent fits used the original typed fixture; 32 local reference
keys supplement this batch without changing the original 156 reference keys.

- LM-11 ordinary ghosts use the **group: low** source panel. Actual native PNG
  coordinate checks find copied gray curves in middle/high panels (33/30 new pixels
  at the tested source coordinates) and none duplicated in low. The adjacent note
  names low, the source red curve remains, and disabling ghosts removes the copies
  and explanation. Visibility, confidence intervals, diagnostics and estimate
  controls were exercised. Actual host reopening and a subsequent interval edit pass.
- LM-16 commits `Primary custom title` and `Comparison custom title` independently,
  retains both with editors hidden and restores both editors. After actual native
  close/Recent reopen, a main-title edit to `Primary custom title after reopen`
  updates only the main output. Saved label stores and native resources are retained.
- GLM-23's probability comparison agrees with an independent binomial/logit fit
  at nine group/x coordinates within **1.663 pixels**, tolerance 3. Native logit
  comparison without intervals and raw/probability comparison with intervals are
  captured. Hiding comparison output leaves the likelihood-ratio table available;
  comparison statistics match the same independent fitted pair. The title and
  capture findings above prevent full acceptance.
- MM-19 step 1 uses `mixed_y ~ x + z + binary + (1 + x | id)` and
  `mixed_y ~ x + binary + (1 + x | id)`, requested REML, 436 rows/24 groups.
  Both fixed tables' coefficients, SE and limits match independent references
  within `5e-16` (tolerance `1e-5`). Independently refitted ML comparison N/AIC/BIC/
  Bayes factors match. Main population curves in binary panels and comparison
  conditional curves in C08/C15/C22 pass 24 image-coordinate checks within
  **2.453 pixels**, tolerance 3; dashed cyan lines are searched within two horizontal
  pixels to account for gaps. REML/ML main coefficient differences exceed the table
  tolerance, supporting the intended REML fit. Their curve differences are below
  pixel resolution: image checks alone cannot distinguish REML from ML. No later
  Mixed conditional/CAS or ghost behavior is claimed from this initial checkpoint.

## Preserved baseline and evidence

The inherited baseline remains **24,546 package assertions, 4,925 native assertions,
140/140 approved visuals**. Complete suites were not rerun for evidence-only work.
Focused work comprised the existing saved-document/typed-data/checkpoint verifier,
independent numerical fits and native PNG coordinate checks; read-only renderer
source inspection investigated the missing title row/context semantics. No new
R6 regression test or SVG approval was needed or performed.

All 162 preflight-protected files are unchanged: 16 fixture/metadata artifacts,
140 SVGs, the manifest and five earlier ledger/index/policy files. Both historical
local evidence indexes also verify unchanged: 416 Stage 11D.1 files and 316 Stage
11D.2 files. Original numerical-reference keys remain 156. Private scripts,
rejected timing/calibration probes, actual OMVs, reopened hashes, screenshots,
accessibility captures, inspector JSON and independent comparisons remain under
`.jamovi-dev/e2e-runs/stage11d3-20261125/`, indexed by
[stage11d3-evidence.json](stage11d3-evidence.json). Large artifacts are not committed.
Raw verifier passes and capture presence cannot override the ledger's manual
quality dispositions, especially GLM step 5.

## Remaining coverage and correction sequence

Of 39 P0 scenarios, **four** have full current-build acceptance: LM-15, GLM-20,
MM-15 and now LM-16. FP-03 is a fully accepted P1. LM-03, GLM-08 and MM-10 have
current-build smoke evidence only; their full matrices remain required. LM-11,
GLM-23 and MM-19 remain unsuccessful. Thus **35 P0 require full acceptance**,
including 29 not yet attempted on this build. The 66 scenarios outside both the
seven-scenario pilot/correction set and this four-scenario batch remain unlaunched.
Defined contracts/native-component coverage are separate from live execution;
only LM-11 and LM-16 completed all declared transitions and final host round trips
in this batch. No new scenarios were started outside the authorized four.

Next, resolve the Mixed title capability gap, explicitly decide the two title/note
contracts, and investigate GLM's settled-save capture/serialization discrepancy.
Then rerun all affected contracts through actual host reopen/edit before promoting
acceptance. Continue the existing prioritized remaining P0/P1 campaign only in a
separately requested batch. Isolated built-package checks and actual-host JASP
compatibility/.jasp restoration remain release gates; neither was begun here.
