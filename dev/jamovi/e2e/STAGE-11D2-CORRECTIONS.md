# Stage 11D.2 — live pilot corrections

Completed on the desktop on 2026-11-25, starting at `9e0e39a9` on
`ggplot-objects`. The corrected process is ready for the remaining P0 campaign.
This is not release approval: the other 70 scenarios were not launched,
comprehensive GUI acceptance, isolated built-package checks and actual-host JASP
compatibility remain outstanding.

## Corrections and contracts

**UI-01, genuine presentation defect:** the Mixed main image records its error,
but jamovi 2.7.41's native image output leaves a blank image area. Native
reproduction, saved protobuf inspection and installed renderer checks separate
this from fitting failure. The existing adjacent `plotExplanation` now displays
`Model plot unavailable:` followed by the actionable layout diagnostic. Both early
plot construction errors and late renderer/label exceptions use that same result.
Late main/comparison callbacks explicitly clear the image path as well as its
state; jmvcore's `setState(NULL)` alone leaves the previous file path available to
serialization. Recovery replaces the diagnostic with the ordinary guide; hiding
output hides/clears obsolete error explanations. Existing result objects,
identifiers, schemas, saved options and plot geometry are preserved. No host code,
CSS, dependencies, fitting calculations or inference rules changed.

**CONTRACT-01, stale LM-15 expectation:** retained unsupported ordinary comparison
grouping is intentional. The third checkpoint now expects stored
`comparisonGroup="group"`, the existing “not applied” explanation and fresh
comparison facets. No redundant grouping control was added. Installed handler
tests verify retention after panel edits. A real R6 counterfactual with that
preference cleared produces identical built comparison geometry and numerical
results. Actual native plots use neutral observations and model line encodings;
the main plot retains its own group encoding.

**CONTRACT-02, stale GLM-20 expectation:** no fitted terms means no eligible
predictor roles. The third checkpoint now requires `plotX=NULL`,
`plotGroup=NULL`, empty panels, absent stale images/tables and the actionable
waiting instruction. Restoring `z` automatically restores x to `z`, leaves color
empty and returns independently verified estimates. Existing retained display
preferences and label tests remain; they are distinct from stale predictor roles.

**HARNESS-02, verifier defect:** only the declared Mixed pairs
`plot`/`plotExplanation` and `comparisonplot`/`comparisonPlotNote` qualify as
companion rendering errors. A unique visible image must own the nonempty error
and have no resource/path; its unique current, completed, visible HTML companion
must contain the observed matching diagnostic. Unrelated, stale, hidden,
duplicated or mismatched notes, surviving resources, unexpected errors in the
other image and unavailable declared tables do not pass. Actual unaffected images
and tables remain required.

## Native import and operations

**HOST-01:** a supported native inline rename can save ``tick`name`` literally.
Actual close/relaunch/Recent reopening changes it back to `tick_name`. Read-only
inspection of jamovi 2.7.41's `formatio/__init__.py`, `fix_column_names`, confirms
the import replacement. Only explicit `jamovi-2.7.41-import` policy for the
primary fixture accepts this one mapping. Exact checking is the default;
unexpected renames/order, collisions, dimensions, classes, values, missingness or
factor levels/order fail. Source fixtures and R-side literal-name tests remain
unchanged. This does not claim native preservation of backticks across reopening.

The stricter check also exposed a **GUI fixture serialization issue**. The old
writer stores whole-valued source doubles (`nb_y`, `constant`) as integers.
Forcing its binary type alone still lets it infer conflicting Integer metadata,
which shifts subsequent columns on native import despite a successful R-reader
round-trip. The development input writer now writes doubles with matching Decimal
metadata. The mismatch regression test, actual native import and all 25 host-saved
dataset comparisons pass. Rejected preliminary OMVs/observation records are retained and
are not passes. This clarifies the historical pilot's typed-data claim: its
values/factor checks passed, but those two unused columns' double storage was not
preserved. No original pilot evidence or verdict was rewritten.

Monitor placement is an operational preference. All new checks record it as
**unconfirmed**, with functional acceptance and exact disposable-document
ownership separate. Ownership, active document/analysis identity and safe closure
remain mandatory; ambiguous or unrelated windows cannot be automated. No display
settings or unrelated windows were changed. One disposable review document was
reused and saved/closed at the end.

Native `getApp` and Open → Recent Files completed all affected final reopens and
the LM-15 saved-input open. Native startup can briefly expose an intermediate
window; re-observe the final title/analysis before editing. Following installation,
two bounded bridge-binding timeouts required one human-assisted open. The user
opened the requested disposable document; subsequent native opens were autonomous.
No repeated Finder/system-picker fallback or backend option setter was used.

## Current-build GUI results

| Scenario | Declared transitions | Actual final reopen | Subsequent live edit | Result |
| --- | ---: | ---: | ---: | --- |
| FP-03 | 3 | 1 | 1 | All passed |
| LM-15 | 3, including original sparse saved-option input | 1 | 1 | All passed |
| GLM-20 | 4, including empty-term waiting and recovery | 1 | 1 | All passed |
| MM-15 | 3, including both error layouts and recovery | 1 | 1 | All passed |

These are **21 declared checks**. A supplementary main-error/valid-comparison
isolation check and one proportionate current-build smoke each for LM-03,
GLM-08 and MM-10 also pass: **25 passed / 0 failed / 0 blocked** total. The three
smokes are not full scenario reruns. Their prepared files were inputs only;
actual native edits forced recalculation before saving the evidence.

Actual host-saved estimates, SE/CI, sample/formulas and comparison statistics
agree with the original independent references. LM-15 comparison N/AIC/BIC/Bayes
factors remain unchanged with the retained preference. Mixed MM-15 fixed effects
for both models match within `1e-5` (observed differences below `5e-16`), and its
ML comparison remains intact in both error cases. MM-10 retains the intended
REML fits on the common 180 observations/12 groups from 240 source rows, with
independently checked ML comparison statistics. FP-03's actual native PNG regression
line matches independent `lm(y ~ x)` at five x coordinates within 0.70 pixels
(2-pixel tolerance), with suppression/bands/recovery inspected in the host.
No unavailable Model 2 coefficient table is claimed as directly checked.

## Validation and provenance

The installed and running module is 0.27.0 with verified production fingerprint
`54081947f58fe59e1543ca1c1e21f3828ee34464c719ee23ba84ac7808c42d63`, archive
`2a4ec202a5fe4cbedd407e8f2add73edd0b4655a21f5db4b906ecfbccec3a88e` and payload
`942478e722d2baf0b8bc93412fbfee6ab98bd038b01a3d93f977b6919d0e5df6`.
The observed native server's read-only module descriptor matches the installation
receipt. No InfoRequest WebSocket replaced the GUI channel.

| Validation | Passing assertions / cases | Failures / errors / warnings / skips |
| --- | ---: | --- |
| Focused package Mixed plot R6 regression tests | 151 | 0 / 0 / 0 / 0 |
| Focused native design, verifier, renderer, handlers and restoration | 1,200 | 0 / 0 / 0 / 0 |
| Final fixture/verifier focused checks after native-import correction | 200 / 24 | 0 / 0 / 0 / 0 |
| Full package, clean source export | 24,546 / 799 | 0 / 0 / 0 / 0 |
| Full native, final fixture correction included | 4,925 / 98 | 0 / 0 / 0 / 0 |
| Approved SVG expectations in full package run | 140 / 140 | None changed, skipped or approved |
| Four compiled UI definitions and package-test source boundaries | Passed | No violations |

The approved ARM64 macOS / R 4.5.0 / ggplot2 4.0.3 / vdiffr 1.0.8 environment
was retained. Full package elapsed 1446.8 seconds; final native 422.6 seconds.
The full package and production/testthat inputs are unchanged by the later
development-only fixture writer correction; the complete native suite was rerun
after that correction. The testthat build-version startup advisory is outside
testthat warning counts. Earlier superseded/partial runs remain retained and are
not the authoritative totals above.

All 156 independent reference keys, all 16 fixture/metadata artifacts, 140 SVGs,
the two historical ledger/index files and all 416 indexed historical local files
retain their starting hashes. The current numerical/visual baseline is additive
test coverage over Stage 11C's 24,504 / 4,837 baseline, without fitting changes.
The separate [policy-only reassessment](stage11d2-policy-reassessment.json) yields
34 passed / 7 failed / 0 blocked from the original 26/7/8 when only monitor gating
is removed. It retains the old build, contracts and verifier findings; it is not
current-build evidence or a retroactive correction of the historical ledger.

The [correction ledger](stage11d2-ledger.json) records authoritative dispositions;
the [evidence index](stage11d2-evidence.json) hashes local detailed captures,
OMVs, inspector data, numerical comparisons and test logs under ignored
`.jamovi-dev/e2e-runs/stage11d2-20261125/`. Large artifacts are not committed.
Capture presence alone never establishes a pass.

## Next sequence

The pilot's product, contract, verifier and fixture blockers are resolved for this
build. Resume the existing prioritized P0 batches with current-build preflight,
strict ownership, the narrow host-name policy and real save/reopen/edit evidence.
Do not promote the three smoke checks into comprehensive scenario coverage or
start the remaining 70 automatically. Obtain human assistance if native targeting
fails; unknown monitor placement alone is not a functional block. Complete and
disposition all P0/P1 GUI scenarios, then isolated built-package and intended
actual-host JASP dependency, menu, option-transfer, output and .jasp restoration
checks before release approval.
