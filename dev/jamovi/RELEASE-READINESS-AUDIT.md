# Stage 11A release-readiness audit

Audited Stage 10 (`96a2152bdd2be40dbd36e74dd0d2cbe3a4579445`) on `ggplot-objects`, 2026-11-24, with desktop completion against the laptop handoff commit `9e14c0326d258085635cdd7b4dd6c2344187b68e`. The [development handoff](DEVELOPMENT-HANDOFF.md) records the historical test and live-GUI evidence.

**Stage 11A complete.** The desktop completed the interrupted full-package investigation from a clean source export, verified the existing development environment and installed payload, and classified the remaining failures. The full suite was not green and this was not release approval. The two confirmed implementation defects were assigned to Stage 11B; see the Stage 11B.1 continuation below for their corrections. At audit completion, stale validation gates, repository artifacts, comprehensive GUI coverage and JASP compatibility remained open. Stage 11A changed no production source, tests or visual baselines. Stage 11B.2 records artifact retirement; the latest green package/native baseline and reconciled E2E definitions are recorded in Stage 11C below. Live host-level release gates remain open.

## Laptop environment and test method

The audit host is Intel macOS with R 4.6.2, Node 24.22.3, Python 3.9.6, jamovi 2.7.41 and JASP 0.18. All flexplot Imports were loadable. Development dependencies were isolated under the ignored `.jamovi-dev/` directory. The official jamovi source repository supplied jmvtools 28.17. Installation needed the system R 4.6 libraries ahead of jamovi's bundled library. Putting the host library first selected an older `fs` and an `rlang` binary with an unresolved `_SETLENGTH` symbol. CRAN macOS binaries installed vdiffr 1.0.9, randomForest 4.7-1.2 and pscl 1.5.9. The vdiffr source build hit a missing C++ standard header on the compiler's default include path. Neither workaround upgraded system tools. `extrafont` 0.20 and `papaja` 0.1.5 also installed from CRAN source by the end of setup. The initial full-test process started before those optional packages finished installing. The jamovi app library supplied jmvcore 2.7.35, RProtoBuf and jmvReadWrite. [Official jmvtools installation instructions](https://dev.jamovi.org/tutorial/tuts0101-getting-started).

Package and native tests use a `git archive` export of Stage 10 at `.jamovi-dev/audit-source`. Use the audit R library paths before the ordinary R installation, then append the jamovi app library for protocol tests. The ignored local logs are `audit-module.log`, `audit-native.log`, `audit-native-all.log` and `audit-full.log` under `.jamovi-dev/`. The completed native run and focused package run wrote structured counts next to their logs. The interrupted full run has only a partial log. The approved graphics reference environment was ARM64 macOS with R 4.5.0, vdiffr 1.0.8 and ggplot2 4.0.3. This Intel host uses ggplot2 4.0.4. Reproduce changed plots on a matched reference environment before assigning a renderer defect or approving any new SVG.

The repository's `install --deps` command built and installed flexplot 0.27.0, populated `.jamovi-dev/cache` for jamovi's R 4.5 Intel runtime, and recorded archive and installed-payload verification in `.jamovi-dev/build-receipt.json`. The installed metadata and independently computed source fingerprint both match Stage 10's `c2c163c61c318af7371cf4d95d048524c96b44661e8312b00df1fec35af05f5f`. The development UI command compiled all four definitions, validated citations, and checked generated headers and bundled scripts. The command stopped during test execution at testthat's default failure limit. A separate native run used the summary reporter to collect the entire suite.

From the repository root, use these paths for subsequent checks on this machine. The audit used the exported checkout for the suite runs to keep the local SVGs outside test discovery.

```sh
export R_LIBS="$PWD/.jamovi-dev/audit-binlib:$PWD/.jamovi-dev/audit-lib"
Rscript --vanilla dev/jamovi.R ui
```

For package tests in the clean export, append `/Applications/jamovi.app/Contents/Resources/modules/base/R` to the end of `.libPaths()` inside R and run `devtools::test(reporter="summary", stop_on_failure=FALSE)`. For complete native triage, initialize `jmvcore:::initProtoBuf()`, load the exported package with `devtools::load_all(".", quiet=TRUE, helpers=FALSE)`, then use `testthat::test_dir("dev/jamovi/tests", package="flexplot", reporter="summary", stop_on_failure=FALSE)`. Count failures and errors from the returned result. The normal repository UI command remains the pass/fail gate.

## Unapproved SVG artifacts

All 26 are valid SVGs outside the active reference inventory. Eighteen are in `testcompare_fits/`, five in `testpartial_residual_plots/`, two are association plots in `testflexplot_unconventional/`, and one is `testvisualize_mixed/mixed-old-error.svg`. Twenty-two have same-name approved counterparts under `fits/` or `partial/`. None of the 26 matches an approved SVG byte for byte. The four without same-name references are the commented-out categorical rpart case, the two association plots, and the legacy Mixed error plot. Historical test titles, directory conventions and the explicit reference migration in `ca43ae71` point to generated output from older visual tests. The exact generation run is unknown.

These were local, unapproved artifacts during the laptop investigation, but commit `9e14c0326d258085635cdd7b4dd6c2344187b68e` tracked and pushed all 26. The desktop therefore tests them as part of the exact clean checkout: 166 SVG files against 140 active references. They are not replacements for active references. Preserve them during this documentation-only audit; remove or relocate them in a separately reviewed repository-hygiene change before release. No baseline approval or regeneration is justified by their presence. The laptop commit also contains a root `.DS_Store` change; the desktop's two untracked development `.DS_Store` files remain untouched.

## Laptop reproduced baseline

| Check | Outcome |
| --- | --- |
| UI compilation | All four analyses compiled. Generated headers, UI scripts and citations passed validation. |
| Installed module | flexplot 0.27.0 installed. Verified source fingerprint, source commit and archive-to-installed payload. |
| Full package suite | Interrupted during `jamovi-mixed-sample`. Historical Stage 10 baseline was 23,244 passing assertions and 16 unsuccessful checks. The focused run reproduced all 16, and a separate targeted rerun confirmed the additional forest rounding assertion seen in the partial full run. The desktop continuation below supplies the completed package and visual baseline. |
| Native integration | 4,494 passed and 42 failed, with zero errors, skips or testthat warnings. Failures comprise 28 inventory/coverage checks, four CSV checks, two stale Mixed p-column checks and eight stale Mixed revision checks. Mixed state serialization emitted its known size advisory outside testthat. |

## Desktop continuation (2026-11-24)

The desktop fast-forwarded `ggplot-objects` from `96a2152b` to the expected remote commit `9e14c0326d258085635cdd7b4dd6c2344187b68e`. Two untracked development `.DS_Store` files were preserved, with their hashes unchanged. The incoming changes contain the audit documents, 26 SVG artifacts and the tracked root `.DS_Store`; production code and test definitions are unchanged from Stage 10.

The desktop is ARM64 macOS (Darwin 25.6.0), R 4.5.0 (`aarch64-apple-darwin20`), Node 26.13.1 and Python 3.14.7. All package Imports load without installing dependencies. Existing packages include devtools 2.4.5, testthat 3.3.2, jmvtools 28.17, randomForest 4.7-1.2, party 1.3-23, pscl 1.5.9, extrafont 0.19 and papaja 0.1.5. Ordinary system R libraries precede the appended jamovi app library: jmvcore 2.6.3 comes from system R, while RProtoBuf 0.4.24 and jmvReadWrite 0.4.12 come from the app. The laptop used jmvcore 2.7.35; the desktop checks retain its existing working dependencies.

The desktop matches the approved primary visual environment: ARM64 macOS, R 4.5.0, vdiffr 1.0.8 and ggplot2 4.0.3. Supporting graphics packages are svglite 2.2.2, systemfonts 1.3.1 and textshaping 1.0.1. Some installed packages report that they were built under R 4.5.2; their namespaces load under this R 4.5.0 installation. Matching versions alone is not evidence that every SVG matches; the full run enables the actual byte-exact comparisons.

Existing development dependencies and the supported `build/R4.5.0-arm64-macos` and `build/js` caches remain usable; `.jamovi-dev/cache` is absent on this host. All four UI definitions compile with the installed compiler, including citation, generated-header and bundle validation. The current production-input fingerprint, installed jamovi metadata and existing build receipt agree on Stage 10 fingerprint `c2c163c61c318af7371cf4d95d048524c96b44661e8312b00df1fec35af05f5f`. An independent payload check confirms both archive and installed-tree hashes. The retained archive is `.jamovi-dev/artifacts/flexplot_0.27.0-46f56f809ab1.jmo`. Its receipt's commit label is `9481f993`, the pre-commit build provenance, and must not be represented as the audit commit; source identity is established by the matching fingerprint. No rebuild or reinstall was needed.

A focused real-protobuf/native desktop smoke passed **533 assertions, zero failures, errors, testthat warnings or skips**, covering `jamovi-glm-automatic-family`, `jamovi-mixed-independent` and `jamovi-mixed`. Mixed emitted the existing large-state advisories for the two plots. This supplements the laptop's complete native run; it does not replace or erase its 42 classified failures.

The full package run uses an exact `git archive` export of `9e14c032` at `.jamovi-dev/audit-desktop-source`, ordinary system libraries followed by the app library, `NOT_CRAN=true`, a null PDF device, `testthat::set_max_fails(Inf)` and `devtools::test(reporter="summary", stop_on_failure=FALSE)`. Snapshot candidates are disabled. Logs and structured results are retained locally as `.jamovi-dev/audit-desktop-full*`; native smoke results use `audit-desktop-native*` and compilation uses `audit-desktop-ui.log`. The export includes the 26 tracked artifacts, so it measures the current remote tree rather than silently cleaning its inventory. No production source, test expectations or approved references were edited.

| Desktop check | Completed outcome |
| --- | --- |
| Full package suite | **23,253 passed, 15 failed assertions, 2 errors, 0 testthat warnings, 0 skips**, across 792 test cases in about 14 minutes 22 seconds. The errors are the known LM preview exception and obsolete GLM table-note assertion. |
| Comparison with Stage 10 | All 16 historical unsuccessful checks recur at their original sites (14 failed assertions and 2 errors). The only additional failure is the strict SVG inventory check caused by the 26 artifacts added in `9e14c032`. The final Stage 10 Automatic fixture has 207 assertions, ten more than the earlier full run's 197; ten extra passes minus the inventory assertion's change from pass to fail accounts for the net nine-pass difference from 23,244. |
| Exact visual regression | **140 of 140 active references have recorded successful comparisons** in the full result object. No SVG comparison failed or skipped. All approved references are unchanged from Stage 10 and remain byte-identical after testing. The working repository retains all 166 SVGs unchanged. The inventory failure concerns extra files, not a changed plot. |
| Forest oracle portability | The desktop's boundary-probability oracle and all three flexplot prediction comparisons pass. The laptop-only rounding failure remains a classified environment-dependent test expectation, not a new desktop failure or a flexplot prediction regression. |
| UI / native / installed payload | All four UIs compile; focused desktop native smoke passes 533 assertions; existing source fingerprint, archive and installed payload agree. The complete laptop native baseline remains 4,494 passed and 42 classified failures. |

The desktop adds no newly observed model-calculation defect or unexplained failure. The GLM layout defect remains established by the laptop's targeted R6 probe even though it is not a failing assertion in the full package suite. The two Stage 11B defects therefore cannot be counted solely from the suite total. Large-state advisories and ordinary plotting messages are not testthat warnings.

An additional harness finding is that testthat 3.3.2's end-of-run `snapshot_cleanup()` deletes the 26 unused SVG artifacts from the disposable export after the inventory failure. The full log records “Deleting unused snapshots”; a post-run comparison confirms exactly those 26 files are absent, with no missing or changed approved reference. The working repository is untouched. Re-export the exact commit before reproducing this baseline: a second run in the already-cleaned export would omit the inventory failure and misrepresent the committed tree. This is snapshot-cleanup behavior plus repository-hygiene debt, not a model or rendering defect, and does not authorize approving or deleting references. The summary reporter displays only ten failure details, but the complete returned result object records all 17 unsuccessful checks and all 792 cases; totals above come from that object.

No new GUI campaign or JASP host verification was performed on the desktop. Existing Stage 9/10 live evidence remains historical and the GUI and JASP requirements below remain release work.

## Outstanding failures and release implications

Focused runs reproduced all 16 historical package failures at their original assertion sites. The other assertions in those files passed. The comparison terms, coefficient and likelihood checks pass despite the two stale expectations per GLM family.

This table records Stage 11A triage. Stage 11B.1 below resolves the LM preview error and the separately probed GLM layout defect; Stage 11B.2 corrects the stale package/native expectations and retires the unapproved repository artifacts. E2E coverage and host-validation requirements remain open.

| Location | Expected and actual | Root cause and disposition | Correction and risk |
| --- | --- | --- | --- |
| `tests/testthat/test-jamovi-generalized-comparison.R:68,70` | Expects a named likelihood-ratio table note and a hidden comparison image when the main image is disabled. The current table-level note says “Likelihood-ratio test; asymptotic chi-square reference.” The comparison image has its own default-on preference. | Stages 3 and 7 changed the contract. The 12 historical failures are six families times two stale expectations. | Assert the current table-level method note and independently toggle `comparisonGraphic`. Low implementation risk. Check saved legacy visibility independently so the revised test also protects migration. |
| `tests/testthat/test-jamovi-lm-layout-recovery.R:40–45` | A deliberately failing label helper should exercise terminal image errors. The exception escapes during `analysis$run()`, before the test can call `.createImage()`. | Stage 8 also calls the helper in the run-time ghost-note preview. The mock now covers both paths. The fixture must scope the injected render error, and the uncovered preview path is a real error-isolation gap. | Test terminal callback handling and the preview failure separately. Guard the preview so a label/display error leaves coefficient and comparison tables intact and produces a plot-local diagnostic. Moderate risk to note timing and stale state. |
| `tests/testthat/test-jamovi-result-lifecycle.R:76` | Expects `previous-plot.png` after Mixed reset for both images. Reset clears both state and image path. | Stale expectations. The reset deliberately prevents stale images when a later run is invalid or hidden. | Require cleared paths and preserve invalid-to-valid renderer checks. Low risk. |
| `tests/testthat/test-jamovi-results-presentation.R:92` | Expects “Residual df:” in the `intervals` table note. It receives NULL. The SE and CI cells carry “t reference with … residual df.” and the interval headers identify the method. | Stage 7 moved method details to native cell footnotes. The assertion error stops the test before related legacy note checks at lines 103–107. | Assert native CI/SE cell footnotes and update the downstream IRT, averaging and NB checks to inspect their actual cells. Preserve numerical interval checks. Low risk if exact scope, content and hidden-table behavior remain covered. |
| `tests/testthat/test-helper_compare_fits.R:37` | The independent forest fixture expects exactly 0 and 1. With party 1.3.23 the Intel laptop produces 0 and 0.99999999999999978. All three flexplot prediction comparisons against that independent output passed. The ARM64 desktop passes the exact oracle too. | Environment-dependent numerical portability issue in the test oracle. The failing assertion runs before flexplot's predictions are evaluated. | Use a tight tolerance for the independent boundary probabilities and keep the per-model numerical and probability-range checks. Low risk. |
| `dev/jamovi/tests/test-jamovi-mixed-tables.R:18` | Requires `p=YES` for both fixed-effect tables. Stage 6 keeps the column identity and explicitly serializes `p=NO`. | Two stale assertions. The comparison table's actual likelihood-test p-value is separate. | Assert the hidden fixed-effect columns with legacy CI off and retain the comparison p toggle coverage. Low risk. |
| `dev/jamovi/tests/test-jamovi-output-visibility.R:27,31` | Eight checks expect revision 1 on Mixed options before and after protobuf reopen. The adapter publishes revision 2. The asserted visibility preferences still pass. | Newly cataloged stale assertions across four Mixed fixtures. Stage 9 introduced Mixed revision 2 to migrate shared plot settings. | Expect the module's applicable revision and keep an explicit revision-2 reopen case that checks plot settings, labels and visibility survive without recopying. Low risk. |
| `dev/jamovi/tests/test-e2e-design.R:61–62,66` | Generated schema, controls, result metadata, test paths and scenario mapping are expected to match the checked-in JSON and have scenario coverage. The regenerated inventory adds 8 LM, 11 GLM and 15 Mixed options, with multiple uncovered transitions. | Stale artifact plus GUI coverage debt. Two automatic-family fields are derived and need an indirect coverage mapping. | Add manifest transitions and derived-state observations first, then regenerate the inventory and coverage index in a reviewable diff. Medium risk of hiding coverage gaps with a snapshot-only refresh. |

The initial native run found one inventory mismatch, 27 uncovered-option assertions, and four byte-exact CSV mismatches. `coverage.csv` grew from 116 to 150 option rows, an expected consequence of stale coverage metadata. The other three CSVs contain numerical spelling differences on 2 common-sample, 1 invalid-fixture and 61 primary cells. The maximum absolute delta is about 1.03e-14 and the stored fixture RDS and numerical reference tolerance checks passed. Treat those CSV assertions as a reproducibility issue in the development harness. Pin the export environment or compare decoded data with explicit precision and exact nonnumeric fields. Keep the shipped CSVs stable for GUI fixtures until any deliberate regeneration is reviewed.

### Confirmed GLM recovery defect (Stage 11A evidence; corrected in 11B.1)

A Stage 10 R6 probe used valid Gaussian fits and a selected variable outside both fitted formulas as `comparisonX`. With valid x it produced three coefficient rows and two comparison rows. With the stale x it produced zero rows for both and reported “Model comparison is unavailable,” even with `comparisonGraphic=FALSE`. A valid x produced the expected rows again. The same valid → stale → valid sequence reproduced in one analysis with the comparison image shown and hidden. The Stage 2 layout call in `R/jamovi_generalized_comparison.R:20` throws inside the fit-level guard in `R/glmgeneralized.b.R:148–161`. LM already records a plot layout error separately. An invalid comparison plot should leave valid model estimates and comparison statistics available. Isolate the GLM layout error in its comparison image and adjacent explanation while retaining both fits. This is a user-visible recovery defect for restored or transient stale roles. The native trigger frequency remains unmeasured. The correction carries moderate risk for label catalog updates, diagnostic placement and rendering with a missing layout. Cover the stale-role case with the comparison plot shown and hidden, then restore a valid role in the same analysis.

## Shared infrastructure

| Area | Finding and recommendation |
| --- | --- |
| Model terms and plot roles | `jamovi/js/model-terms.js`, `R/jamovi_model_terms.R` and `R/jamovi_model_layout.R` already centralize most translation and validation. Add parity cases for saved predictor roles and unsupported choices across native controls and the R validators. Keep LM magnets, Mixed CAS/cluster requirements and GLM fitted-family eligibility explicit. |
| Visibility and saved migration | `R/jamovi_output_options.R` and native `output-options.js` own restoration. Keep explicit revision boundaries and add parity fixtures for omitted, legacy, and current options. Stage 10's explicit Automatic selector and derived fields have separate semantics. A single generic migration engine would enlarge the saved-file risk. |
| Plot labels and ghosts | The shared label catalog/store and `jamovi_model_ghosts.R` are established reuse points. Main/comparison contexts and Mixed cached cluster streams need to remain isolated. Limit follow-up to shared tests for publication, retained keys and note/curve agreement. Keep GLM probability/logit and directional Mixed CAS geometry separate. |
| Table notes | `jamovi_table_notes.R` already owns cell annotations and comparison reason formatting. Keep family-specific interpretation in GLM and Mixed inference qualifications in their adapters. Test note removal on hidden and invalid outputs, including native serialization. |
| Comparison sample | LM, GLM and Mixed comparison code each filters complete cases across the union of fitted variables and tracks omissions. A small prepared-sample helper could standardize row identity and omission provenance once exact equivalence tests exist. Keep GLM Automatic resolution on the original outcome, and Mixed requested-method prediction versus ML comparison statistics, at their current boundaries. |
| Clearing and recovery | LM, GLM and Mixed reset loops separately combine generic result clearing, image path/state lifecycle and presentation defaults. Consolidate the image clearing contract after tests verify restore, hide, invalidation and successful rerender. Leave waiting text, table schema visibility and diagnostic routing with each adapter. |
| Asynchronous metadata | `glm-plot-controls.js` and `model-ghosts.js` already reject stale replies. Exercise delayed replies across outcome changes, independent output edits and workspace restoration. A shared request guard is worthwhile only if those cases expose divergent invalidation behavior. |

## GUI evidence and campaign

The deterministic framework now has 73 scenarios (16 Flexplot, 16 LM, 23 GLM and 18 Mixed), with 156 independent fitted checkpoints. The Phase 1 README's opening counts still say 70, although its later addendum records 73. The framework supplies seeded datasets, independent model references, source inventory and a single-scenario request/observation harness. The inspector reads an actual host-saved archive and the harness distinguishes failed, blocked and unexecuted checkpoints. Headless fixtures, native handlers, R6 fits and renderer tests cannot establish visible placement, focus, engine completion or a file reopening.

The [Phase 2A record](e2e/PHASE-2A.md) shows four passed historical checkpoints across FP-03 and LM-03, partial interaction/term edits for GLM-08 and CAS observations for MM-11. The later native reopen verified targeted GLM and Mixed controls and live edits, including directional CAS, with gaps in GLM image observation and nonempty Mixed term restoration. Stage 9 separately verified independent Mixed labels, plots and modes through a real save/relaunch/reopen. Stage 10 separately verified legacy Gaussian alongside Automatic, manual override, eligible categorical/0/1 outcomes and continuation to a continuous response. Earlier stage reviews observed representative output toggles, plot editors, ghosts and notes. No full scenario matrix has passed on the audited build.

1. Update action targets and expected results after Stages 1–10. Keep LM-16 and GLM-23 as the independent editor starting points, GLM-22 for ordinary GLM ghosts, GLM-08 for saved term synchronization and MM-10/MM-11 for common-sample and directional CAS behavior. Update LM-11/15, GLM-07/09/20 and MM-04/05/12 for the current visibility, family, role and plot contracts. Retain the exploratory Flexplot cases, numerical reference fits, supported term builders, common-sample checks, invalid datasets and model-specific plot geometries. Update comparisons that expect shared visibility or grouping, GLM's historical default, and Mixed's former single active plot. Add explicit cases for both plot-label editors, ordinary CAS ghosts and derived Automatic eligibility.
2. Start with identity verification and the eight-analysis smoke file. Confirm terms survive editing an open options pane after module refresh, given the existing term-loss observation. Exercise ordinary Flexplot early.
3. Run the 31 P0 scenarios first, then the 42 P1 scenarios, in batches organized by behavior. Cover all named GLM families, Automatic/manual/legacy Gaussian and outcome changes, common-sample level loss and rapid reassignment. Check independently styled and labeled comparison output, grouped LM magnets, Mixed population/cluster/CAS modes, ordinary versus directional ghosts, custom reference labels and all independent visibility toggles.
4. Run valid → invalid → valid cases for formula terms, unsupported axes/panels, missing reference curves, ineligible binary samples, separation and singular/random-boundary fits. Verify cleared stale images and notes, appropriate diagnostics, and recovery without rebuilding the analysis.
5. Save with divergent plot options, labels and hidden preferences, reopen through the host, verify visible controls/results and edit again. Include prior revisions and explicit/omitted historical family options. Wait for rendered completion before saving, capture an exact window and build receipt, and record evidence per checkpoint. Use a human operator for ambiguous supplier selection, native dialogs or window placement. Resume an unattended path only where targeting and completion can be observed.

## JASP compatibility

The Intel JASP installation includes its own R 4.3.2, jaspBase and jaspGraphs 0.18.3, and a historical flexplot 0.20.3 in the jaspVisualModeling module. Both host-specific library namespaces loaded under JASP's embedded R 4.3.2. The host module library has ggplot2 3.4.4 and lme4 1.1-35.1 and lacks the required `reformulas` and `tidyr` packages. Use 0.18 as a legacy target after resolving those dependencies and checking Stage 10 against the older plotting and modeling packages. Compatibility with Stage 10 and newer hosts remains unverified. The packages are absent from the ordinary R 4.6 library. An embedded-R probe initially selected a user-library `purrr` built against an absent standalone R 4.3 framework. Excluding that user library let the bundled flexplot 0.20.3 load. Use the host-matched R and an isolated development module library for Stage 10. For source reproduction, use the official GitHub packages in a host-matched renv library. Pin jaspBase to `8b03c9b6239649f1c05a1420977cf35cabdc0454` and jaspGraphs to `73e2e77b0acb80dba6edccdf105b3d216eba8345` for this installed host. Verify native linkage and dependency versions before loading Stage 10. [JASP module workflow](https://github.com/jasp-stats/jasp-desktop/blob/development/Docs/development/jasp-modules-tutorial.md), [jaspBase source](https://github.com/jasp-stats/jaspBase), [jaspGraphs installation](https://github.com/jasp-stats/jaspGraphs).

`inst/DESCRIPTION.qml` registers Flexplot, LM, GLM and Mixed QML interfaces. Their adapters are `flexplot_jasp2.R`, `linmod_jasp.R`, `glinmod_jasp.R`, `mixedmod_jasp.R` and `jasp_common.R`. The R-side checks in `test-jasp-themes.R`, `test-jasp-glm-families.R`, `testjasp_modelcomp_table.R` and `test-jasp-errorbar-linewidth.R` cover QML values, logistic and negative-binomial fitting, predicted values, comparison statistics and the plotted interval data. The old private fixture script and commented-out jaspTools commands give no active host coverage. Jamovi's Automatic option, protobuf migrations and native handlers are host-specific. Shared model predictions, plot geometry, ghost metadata, comparisons and ggplot rendering still need JASP regression checks. Register this exact build in developer mode, then test all four menu entries, option transfer, plots and tables, invalid-input recovery and a real `.jasp` save/reopen/edit. Confirm the intended supported JASP host versions before making a compatibility claim. No JASP installation or GUI settings were changed in this audit.

## Documentation and implementation sequence

- Correct the Phase 1 README’s scenario counts and distinguish historical pilot evidence from the current build. Update the development guide's Modeling result specifications, Comparison visualization roles and Output visibility sections to describe Stage 9's independent Mixed outputs, labels and comparison grouping and the revision 2 migration. Link the development guide to the existing GLM Automatic and legacy-restoration section. Revise the GLM guide’s Visualization compatibility section, which says the UI resets incompatible Logit settings. The saved choice survives and R uses the effective Probability fallback. Update `docs/jamovi-model-ghosts.md` to describe Stage 8 reference notes and Stage 9 ordinary CAS support. Its deferral paragraph predates those implementations. Clarify which commands need the compiler, the dependency cache and the app libraries, and distinguish installed fingerprints from GUI acceptance. Thirteen native test files refer directly to the default macOS app path. Route those through the existing host resolution before promising that `JAMOVI_HOME` alone supports a nondefault install for the entire suite.
- Use the completed desktop baseline in this audit when validating corrections. Retain the graphics inventory and review any SVG differences on the reference renderer.
- Stage 11B.1 isolates the confirmed GLM comparison-layout error and LM label-preview exception from valid model results. Next repair verified stale assertions with positive checks on current reporting, isolate environment-dependent failures and rerun both suites.
- Update the E2E manifest before regenerating derived artifacts. Run an isolated built-package check and the native jamovi suite. Retain exact SVG review and classify differences against a matched renderer environment.
- Review shared sample provenance and image lifecycle as separate changes if tests show a maintainability or recovery benefit. Run the revised GUI campaign and validate JASP in the actual host before publishing any claim of cross-host support.
- The missing comprehensive live campaign and failing validation gates block release readiness. Secondary-monitor placement, native annotation spacing and the historical cache-size advisory are limitations to document. The 26 tracked, unapproved SVG artifacts do not belong in the release inventory.

## Stage 11B.1 — GLM and LM error isolation

Completed 2026-11-24, starting from Stage 11A commit `21e57176` on `ggplot-objects`. Both confirmed defects are corrected; the remaining release gates below are not approved by this stage.

The GLM comparison helper now catches only visualization-layout failures and returns a `plotError` alongside the two successful fits, common sample and unchanged comparison statistics. This follows the existing LM comparison-state convention. The comparison plot consumes that diagnostic; its guide and label-catalog preparation are guarded separately from table publication. Visible failures replace the adjacent comparison explanation with an escaped diagnostic. Hidden comparison plots retain their independent visibility preference without suppressing coefficients or likelihood statistics. Renderer errors clear the failed image state and replace obsolete explanatory content. Fitting and inference exceptions retain their existing error paths rather than being converted into plot errors.

LM's preview guard now includes display construction, label context/catalog preparation, label application and ghost-note preparation. A failure is stored in the affected image state and shown as a plot-local diagnostic while coefficient, supplementary and comparison statistics continue. The renderer handles that saved preview failure through the existing terminal diagnostic-image path; renderer-only exceptions are also contained. Failed main and comparison explanations are replaced, and the next valid run clears the diagnostic and regenerates the image. Existing label keys, catalogs, retained stores and option/result identifiers are unchanged.

Focused package tests passed **684 assertions**, including the original LM recovery test. New regressions exercise repeated valid → stale → valid GLM layouts with comparison visible/hidden, independent main visibility, Gaussian/explicit Binomial/Automatic families, common-sample omissions, exact preservation of numerical tables and predictions, actual diagnostic PNGs and state clearing. LM tests separately inject preview, renderer-only and comparison label-preparation errors, assert retained coefficients/comparisons/labels, and recover in the same analysis. Separate probes confirm genuine fitting and inference exceptions are not swallowed by the layout guard.

Native-handler/protobuf/restoration tests passed **606 assertions**, including 25 new assertions for a real stale-layout saved-options message and LM saved-label persistence through preview failure and reopen. Existing family, independent-display, label-editor and restored-controls checks pass. Tests use the current saved-option protocol and existing migration versions; no new migration is introduced.

Four independent representative captures (LM, Gaussian GLM, explicit Binomial and Automatic) are `identical()` to Stage 11A for coefficient/comparison tables, fitted coefficients and covariance matrices, fitted values and row identities, both plots' built coordinate data, and reference-panel ghost metadata. Fixtures include interactions, quadratics, panels and common-sample omissions. No prediction, inference, event-encoding or valid-plot geometry code changed, and no visual baseline was approved or regenerated.

The module was built and installed from the existing desktop caches without refreshing dependencies. The current source fingerprint, build receipt and installed metadata match `68964d65afd6f88cbf4697301d09edc4d32412ad773ddcb1298cd965fb317550`; independent checks confirm archive and installed-payload hashes. Local evidence uses `.jamovi-dev/stage11b-focused-final*`, `stage11b-native*`, `stage11b-invariance-*`, `stage11b-baseline.rds`, `stage11b-current.rds` and `stage11b-install.log`.

Live jamovi **2.7.41** used one disposable 120-row review document. The user opened it after automation failed to enter a path reliably in the macOS file picker. Its saved GLM comparison x (`group`) was outside both fitted predictor sets; its LM main plot duplicated `x` as the x and grouping roles. To distinguish cached fixture output from native execution, the review was reopened from the original settings and the first x value was changed from -2 to -1.95 before opening either options pane. Both analyses recalculated: the displayed intercepts changed from 0.486 to 0.485 (GLM) and 0.152 to 0.151 (LM), while the respective plot diagnostics and valid comparison tables remained visible. Opening the GLM controls normalized the stale comparison axis to `x`; its comparison curves and normal guide returned. Removing the LM grouping conflict restored the main panelled plot and removed its diagnostic, with the updated coefficients and comparison results retained. Both recovered plots were visually inspected. GLM comparison visibility was toggled independently, including comparison-only output with its main image hidden; the estimates and likelihood statistics stayed available. The disposable edits were discarded and the review window closed; unrelated windows were untouched.

The live LM case is a controlled display-layout failure. Deliberately injected label-preview and renderer exceptions, stale-layout recovery with comparison hidden, and protocol label persistence are R6/native test evidence, not claimed live GUI observations. No installed error-injection hook was added. The comprehensive GUI campaign and actual-host JASP compatibility requirements remain outstanding as documented above.

### Final package and visual baseline

The final full run used a fresh source export with the task changes over `21e57176`, including all 166 committed SVG files at startup. Its log and structured results are `.jamovi-dev/stage11b-final*`, with source at `stage11b-final-source`. An earlier full run was superseded when the existing assertion requiring the comparison helper to throw for a stale axis was updated for the corrected contract: retain fits/statistics and raise the layout error from the plot helper. Only that related assertion was revised; the unrelated historical stale assertions were left intact.

| Check | Final result |
| --- | --- |
| Full package suite | **23,767 passed, 15 failed assertions, 1 error, 0 testthat warnings, 0 skips**, across 796 test cases in about 14 minutes 44 seconds. |
| Stage 11A comparison | Baseline: 23,253 passed, 15 failed assertions and 2 errors. The LM preview error is resolved and its test now completes. The 514 additional passes comprise 505 new package assertions, five assertions reached after the LM correction, and four net additional assertions in the revised GLM layout-contract test. The GLM defect was originally established by an R6 probe, rather than a failing baseline assertion. |
| Remaining failed assertions | Twelve stale GLM method-note/independent-visibility assertions at `test-jamovi-generalized-comparison.R:68,70`; two stale Mixed image-path assertions at `test-jamovi-result-lifecycle.R:76`; one strict inventory assertion for the 26 tracked SVG artifacts. |
| Remaining error | The stale GLM table-note assertion at `test-jamovi-results-presentation.R:92` still expects a removed table-level note instead of the native cell footnotes. No additional failures or errors were found. |
| Visual regression | **140/140 approved comparisons pass**, with no skipped or failed SVG comparison. All 140 approved files remain byte-identical in the export. All 166 repository SVG files are unchanged. As in Stage 11A, testthat removed exactly the 26 unused artifacts from the disposable export during final cleanup; the repository copies remain preserved. |

The original LM recovery file now passes all 179 assertions; the four new package cases pass 505. The final full run also passes the revised GLM layout-contract case and the existing family-specific predictions, intervals, common-sample and numerical checks; its unrelated reporting assertions still fail as listed. `git diff --check` passes. The complete laptop native audit's 42 classified failures remain historical release work; this stage ran the relevant 606-assertion desktop selection rather than repeating that full audit or repairing its unrelated expectations. Next address verified stale reporting/lifecycle and portability expectations, repository SVG hygiene and E2E transitions/derived coverage in their own stages, then rerun the release gates and complete the comprehensive GUI and JASP campaigns.


## Stage 11B.2 — Test contracts, portability and repository hygiene

Completed 2026-11-24, starting from `ba5d76e1` on `ggplot-objects`. Production R/JS/YAML, model calculations, saved option/result identifiers and the approved visual references are unchanged.

- GLM comparison tests now require the concise likelihood-ratio table note, including the negative-binomial size qualification, and verify independent main/comparison image visibility. Explicit comparison hiding leaves the main output and statistics available; numerical tables remain exactly equal through toggles. Existing independent likelihood, coefficient, common-sample and prediction checks remain. Native legacy saved-visibility tests continue to exercise omitted revisions and explicit current preferences.
- Both Mixed image reset paths now require cleared state and paths. A new real-PNG lifecycle case renders both valid plots, invalidates the outcome, verifies that invalid/hidden outputs cannot create stale images, and recovers all four independent visibility combinations while preserving fixed-effect and comparison tables.
- GLM reporting tests inspect native SE/lower/upper cell footnotes for raw-coefficient uncertainty, t-reference residual df and negative-binomial estimated-size qualifications. They also check IRT predictor conditioning, slope interpretation, threshold units and marginal-averaging cells. Previously unreachable generic-note assertions were replaced with these positive cell checks. Existing full-precision confidence-limit tests remain, and SE checks retain the established Gaussian three-decimal presentation. Hidden and empty tables must clear notes/footnotes; populated numerical tables survive hide/show exactly. The desktop jmvcore dataframe exporter cannot export some empty tables, so empty-state assertions use native row counts and notes rather than requesting an empty `asDF`.
- Native Mixed fixed-effect p columns retain identifier `p` but serialize as hidden. A separate nested-model fixture independently calculates the ordinary ML likelihood-ratio p-value and verifies the comparison p column's independent toggle. Current Mixed output revision is 2. A revision-1 migration fixture copies legacy shared settings once, then preserves divergent axes/panels, prediction types, cluster counts, label arrays/stores and retained preferences through two further real-protobuf reopen/migration cycles. Numeric revision value is checked across protobuf integer/double storage.
- Only the independent forest boundary oracle receives a floating-point tolerance: sorted numeric probabilities must equal 0 and 1 within `1e-14`, covering the laptop's `1 - 2.22e-16` result. The tapply array is compared by its numeric values, retaining the former set assertion's intent. Oracle probabilities also have a tight range check. All three public prediction comparisons keep their existing independent predictions and `1e-10` tolerance; no unrelated tolerance or production calculation changes.

### Exact artifact retirement

The 26 tracked SVG paths listed below were verified to be outside the 140-reference inventory. Their original bytes were copied and hash-verified under ignored `.jamovi-dev/unapproved-svg-artifacts/`, preserving the relative paths below. `git rm --cached` untracked them; the copies in the active `_snaps` directory were then removed so strict inventory checks see only approved references. Exact-path ignore rules follow the existing approved-SVG exception, preventing these legacy outputs from returning as untracked release candidates without broadly ignoring legitimate reference SVGs. The root `.DS_Store` was also untracked, retained in place and ignored. Both unrelated development `.DS_Store` files retain their original hashes.

```text
tests/testthat/_snaps/testcompare_fits/compare-fits-where-listwise-deletion-causes-change-in-levels.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-glm.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-labels-breaks.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-loglink.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-many-vars-and-polynomial-v2.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-many-vars-and-polynomial.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-mixed-models-and-re-t.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-mixed-models.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-one-model.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-polr.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-rf.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-rlm.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-rpart-categorical.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-rpart-numeric.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-strong-interaction.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-tibbles.svg
tests/testthat/_snaps/testcompare_fits/compare-fits-with-two-glms.svg
tests/testthat/_snaps/testcompare_fits/compare-interaction-vs-me.svg
tests/testthat/_snaps/testflexplot_unconventional/association-plot.svg
tests/testthat/_snaps/testflexplot_unconventional/paneled-association-plot.svg
tests/testthat/_snaps/testpartial_residual_plots/partial-residual-plot-with-formula-term.svg
tests/testthat/_snaps/testpartial_residual_plots/partial-residual-plot-with-one-variable.svg
tests/testthat/_snaps/testpartial_residual_plots/partial-residual-plot-with-two-variables.svg
tests/testthat/_snaps/testpartial_residual_plots/partial-residual-with-interaction-specified-backward.svg
tests/testthat/_snaps/testpartial_residual_plots/prp-with-no-added-term-argument-specified.svg
tests/testthat/_snaps/testvisualize_mixed/mixed-old-error.svg
.DS_Store
```

The preservation manifest and approved-reference hashes are recorded locally in `.jamovi-dev/stage11b2-artifacts.json`. Ignore-rule validation confirms that precisely the 26 retired SVG paths and Finder metadata are ignored, while all 140 approved SVG paths remain eligible for tracking. This is repository hygiene; no reference SVG was regenerated or approved, and no E2E inventory or scenario manifest was updated.

### Native validation and remaining Stage 11C gates

The corrected focused package selection passes **1,817 assertions**, and the relevant native/protobuf/restoration selection passes **648**, with zero failures, errors, skips or testthat warnings. The complete desktop native suite finishes at **4,609 passed, 29 failed assertions, 0 errors, 0 testthat warnings and 0 skips**. Every failure is in `dev/jamovi/tests/test-e2e-design.R`; none is in the corrected p-column, revision, restoration or visibility tests. The existing large Mixed image-state advisories remain outside testthat warning counts.

| Deferred E2E gate | Failures | Disposition |
| --- | ---: | --- |
| Checked-in source inventory differs from current definitions/results/test metadata | 1 | Stage 11C: update the scenario transitions and derived-state mapping before reviewing regenerated inventory. |
| Options lack scenario coverage | 27 | Stage 11C: 5 LM, 9 GLM and 13 Mixed options listed below; preserve the existing coverage requirement. |
| Derived `coverage.csv` differs | 1 | Stage 11C: stale option/scenario metadata, not a model calculation change. Do not replace it without completing the coverage mapping. |

Uncovered option identifiers are:

- LM: `comparisonGraphic`, `comparisonStatistics`, `modifyPlotLabels`, `modifyComparisonPlotLabels`, `ghostLines`.
- GLM: `automaticFamily`, `automaticFamilyOutcome`, `comparisonGraphic`, `comparisonStatistics`, `comparisonSe`, `comparisonPlotStyle`, `comparisonPlotScale`, `modifyPlotLabels`, `modifyComparisonPlotLabels`. The two Automatic fields require observations of derived state rather than direct user edits.
- Mixed: `comparisonX`, `comparisonPanels`, `comparisonGroup`, `comparisonPredictionType`, `comparisonClusters`, `comparisonPlotLabels`, `comparisonPlotLabelStore`, `estimates`, `comparisonGraphic`, `comparisonStatistics`, `modifyPlotLabels`, `modifyComparisonPlotLabels`, `ghostLines`.

All nine other fixture CSVs match byte for byte on the desktop, including `primary.csv`, `common_sample.csv` and `invalid.csv`, which had numerical spelling differences on the laptop. Fixture RDS and numerical reference checks also pass. Thus the laptop's three numerical CSV reproducibility failures do not recur here; no CSV, fixture, tolerance or E2E source was changed to obtain that result. The remaining 29 assertions are the already classified E2E inventory/coverage debt, not newly discovered implementation defects. No checked-in inventory or scenario matrix was regenerated or weakened. Local native evidence is `.jamovi-dev/stage11b2-native-focused-final*` and `stage11b2-native-all*`.

Additional investigation reproduced an existing desktop development-harness limitation against the unchanged Stage 11B.1 source: after Binomial → Poisson, `jmvcore` 2.6.3's `asDF` export of now-empty IRT/probability tables raises `invalid 'row.names' length`. Both native tables have zero rows, are hidden and have no table notes. A fresh empty table exports successfully; the issue concerns exporting a table after its populated state is cleared. This is why the corrected tests inspect native empty-state counts/notes and reserve dataframe equality for populated tables. No model, inference or displayed-result defect was found, and no dependency or production workaround was introduced. The baseline probe is retained locally as `.jamovi-dev/stage11b2-empty-export-probe-final.log`; any dataframe-export follow-up belongs with the development harness/dependency compatibility work.

### Final package baseline and scope review

The final clean source export (`.jamovi-dev/stage11b2-final-source`) contains exactly the 140 approved SVG references and excludes the untracked Finder metadata. The full package suite finishes with **24,504 passed, 0 failed assertions, 0 errors, 0 testthat warnings and 0 skips**, across 797 test cases in about 16 minutes 14 seconds. Stage 11B.1 was 23,767 passed / 15 failed assertions / 1 error. All 16 unsuccessful checks are now resolved; the 737 additional passes include their corrected assertions and new or previously unreachable checks. The focused corrected files account for the entire count difference; unchanged package tests retain their prior results. Logs and structured results are `.jamovi-dev/stage11b2-focused-final*` and `stage11b2-full*`.

All **140/140 approved visual comparisons have recorded successful expectations**, with no skipped or failed comparison. Strict inventory validation passes. Post-run hashes confirm all approved files are byte-identical in both the working tree and clean export; the 26 retired local copies and all three local Finder files also retain their original hashes. Review of the Git index confirms exactly 27 intended removals (the listed 26 SVGs plus root `.DS_Store`) and all 140 approved references still tracked. `git diff --check` passes.

No production source, numerical calculations, dependency versions, checked-in E2E artifacts or scenario files were changed. The build-input fingerprint remains Stage 11B.1's `68964d65afd6f88cbf4697301d09edc4d32412ad773ddcb1298cd965fb317550`; no module rebuild or new GUI campaign was needed for this test/hygiene stage. No new modeling implementation defect was found. The empty-after-populated dataframe-export limitation above is recorded separately, not hidden by a numerical tolerance change. Stage 11C must address the 29 E2E gates by updating actual transitions and derived-state observations before regenerating inventory/coverage. The comprehensive live GUI campaign, isolated built-package release checks and actual-host JASP compatibility still remain release requirements.


## Stage 11C — Reconciled E2E definitions and validation metadata

Completed 2026-11-24 from `55363abd` on `ggplot-objects`. The 29 deferred native
assertions are resolved by real scenario contracts before metadata regeneration,
not exclusions. See [all 27 option mappings](e2e/STAGE-11C-COVERAGE.md),
[the Stage 11D plan](e2e/STAGE-11D-PLAN.md) and
[structured validation results](e2e/stage11c-validation.json).

The final inventory represents all four current definitions and all **150 options**
(17 Flexplot / 40 LM / 45 GLM / 48 Mixed). Every option retains a planned action
or explicit indirect contract. Blanket coverage of data, migration markers,
comparison capability and stores was replaced with named import, derived-state,
actual saved-protobuf restoration or committed-row/store checkpoints. Hidden
options cannot be submitted as native edits. Public changes must match expected
option state, and result names must exist in the current schemas. Removing the
real transition removes its coverage; negative tests enforce this requirement.

The matrix grows from **73 scenarios / 236 transitions to 77 / 267**, with
**156 unchanged independent fitted-reference keys**. Additions are LM-17 legacy
saved state and fitted magnet capability, GLM-24 Automatic/manual/rapid metadata
and legacy Gaussian, MM-19 independent main/comparison settings/modes/labels and
MM-20 saved shared-to-independent migration. Existing IDs, datasets and independent
formula specifications are preserved. Native interaction/polynomial gates are
explicit rather than relying on inaccessible term destinations. LM-11, LM-16,
GLM-23 and MM-19 cover independent outputs/editors and retained text. MM-11
explicitly selects comparison CAS and requires directional ghost evidence.
MM-15 retains plot-local failure/recovery with intact fits and other output.
LM-15 restores a retained ordinary grouping preference instead of pretending its
unavailable control is editable. MM-04 legacy includeCI transitions restore
protobuf input, without reintroducing a removed checkbox or fixed-effect p output.

Automatic's inferred family/outcome are saved read-only observations tied to real
outcome selection, Automatic control state, fitted family/link/event coding and
independent base-R fits. Delayed stale-reply rejection requires request identities
and controlled live reply evidence; without them it remains blocked. Legacy
source messages are decoded using installed actual protobuf types; native tests
verify one-time output/label migration, retained unavailable label rows,
integer/double numeric representations and repeated serialization. Independent
Mixed transitions compare the intended REML Model 1 coefficients/covariance with
unchanged MM-07/1 references through mode, cluster, ghost and visibility changes.

The harness requires explicit native UI state and evidence-backed observations.
Missing reviews are blocked; incorrect observations fail; reconstructed R6
provenance cannot establish GUI coverage. Restoration needs the original saved
input options, file/hash and actual host open. Comparison-only visible images
are now inspected even with the main graphic hidden. Hidden estimates are
explicitly recorded as not numerically inspected, then re-enabled for independent
checks. GLM live-table validation recognizes existing SE identifiers/rounding;
full-precision confidence-limit checks and stored numerical tolerances remain.
These correct validation semantics without changing production output.

Only `inventory.json`, `coverage.csv` and `scenario-index.csv` were regenerated in
place, using the deterministic generator's metadata-only route. Tests regenerate
numerical references exclusively in temporary directories for comparison. All
**13 protected non-metadata fixture/reference/schema/provenance files and 140
approved SVGs remain byte-identical** to the starting commit. Existing result
identities/schemas and build inputs are unchanged; fingerprint remains
`68964d65afd6f88cbf4697301d09edc4d32412ad773ddcb1298cd965fb317550`. No module rebuild, dependency change, production/statistical
change, JASP modification or snapshot approval occurred.

### Final desktop validation

| Check | Passed | Failures / errors / testthat warnings / skips |
| --- | ---: | --- |
| Focused E2E design, numerical references, evidence validator and native scenario replay | 882 (32 cases) | 0 / 0 / 0 / 0 |
| Full native integration, frozen clean source export | 4,837 (92 cases) | 0 / 0 / 0 / 0 |
| Full package, clean source export | 24,504 (797 cases) | 0 / 0 / 0 / 0 |
| Approved exact SVG expectations in the full package run | 140 / 140 | None missing, failed, skipped or regenerated |
| Strict metadata/coverage and original reference-key checks | 150 options / 77 scenarios / 156 reference keys | No unmapped current option |

Full native elapsed 224.2 seconds; full package elapsed
1038.0 seconds. Runs use the existing desktop libraries,
NOT_CRAN=true, no snapshot candidates, null PDF device and unlimited test-failure
collection, with jamovi app libraries appended after system R. The approved
visual environment remains ARM64 macOS / R 4.5.0 / ggplot2 4.0.3 / vdiffr 1.0.8.
The full package assertion total is unchanged from Stage 11B.2. Native changes
resolve all 29 earlier failures and add positive/negative evidence-contract,
restoration and independent-fit checks. Actual successful expectations are matched
to each of the 140 approved references, with post-run hashes checked in the
working tree and clean source. `git diff --check` passes. Local detailed logs,
RDS results, summaries and protected hashes are ignored `.jamovi-dev/stage11c-*`.
The installed-testthat R-build startup advisory and pre-existing large Mixed-state
messages are separate from testthat warning counts.

### Findings and remaining release gates

- **Genuine implementation defects:** no additional modeling defect found. The
  GLM layout isolation and LM label-preview exceptions remain corrected by
  Stage 11B.1; no production repair was added here.
- **Stale validation:** the prior 1 inventory / 27 option / 1 coverage-CSV failures
  are resolved. Current planned-option coverage has zero gaps; no former scenario
  or numerical reference was removed to achieve this.
- **Environment-dependent limitations:** the desktop matches the approved renderer.
  Historical laptop spelling/rendering results are not new failures here. Keep
  the previously documented empty-after-populated jmvcore asDF exporter limitation
  separate; it was neither changed nor reclassified as a modeling defect.
- **Outstanding GUI:** zero new live GUI executions or passes. The 77 defined
  scenarios require 267 transitions and 77 final host reopen captures. Native
  component/replay tests do not establish full live scenario execution. The plan
  has **39 P0 / 38 P1** scenarios; historical pilot results remain historical.
  Secondary-monitor/focus targeting, actual files/reopens, restored independent
  labels and plots, source/destination ghost coordinates and controlled stale
  replies need fresh evidence. A blocked P0 remains a release blocker.
- **Outstanding release/JASP:** run isolated built-package checks and test this
  exact build in the intended JASP host(s), including dependencies/native linkage,
  four menu entries, option transfer, tables/plots, invalid-input recovery and
  real .jasp save/reopen/edit. Existing source-level JASP tests are not host proof.

Recommended sequence: run the Stage 11D P0 preflight/recovery and independent-output
batches, then migrations/family/ghosts and remaining P0; execute/disposition P1.
File any new product defect separately with captured reproduction before repair.
Complete isolated built-package and actual-host JASP gates before release approval.
Stage 11C completes validation-framework reconciliation, not release sign-off.


## Stage 11D.2 — desktop pilot corrections and current baseline

Completed on 2026-11-25 from `9e0e39a9`; full detail and local evidence provenance
are in the [correction report](e2e/STAGE-11D2-CORRECTIONS.md),
[ledger](e2e/stage11d2-ledger.json) and
[evidence index](e2e/stage11d2-evidence.json).

- **Genuine implementation defect corrected:** Mixed Models' main rendering error
  now appears in the existing adjacent explanation in actual jamovi 2.7.41.
  Late main/comparison exceptions explicitly clear retained resource paths.
  Both fit tables, valid other visualizations and ML comparison statistics survive;
  population/layout recovery and saved-analysis reopening pass. No fitting or
  inference calculations, schemas, identifiers, host CSS/code or dependencies changed.
- **Stale expectations corrected:** LM-15 retains its intentionally unsupported
  comparison group while verifying the warning, supported panels, unaffected
  comparison geometry and numerics. GLM-20 clears stale x/color roles and old
  output on empty fitted terms, then automatically recovers x when z returns.
  Companion-note verification now requires the matching errored image, no stale
  resource, current visible note and other intact results; negative tests remain strict.
- **Host/environment distinctions:** native rename saves a literal backtick but
  real reopening normalizes it to underscore. Only explicit primary-fixture/
  jamovi-2.7.41 mapping is allowed; typed data/factor checks remain exact.
  The development fixture writer now uses matching double binary and Decimal
  metadata for whole-valued doubles; malformed preliminary inputs were rejected,
  not waived. Original deterministic artifacts are untouched. The approved ARM64
  R 4.5.0 / ggplot2 4.0.3 / vdiffr 1.0.8 environment still matches. Monitor placement
  is separately unconfirmed, with safe disposable ownership mandatory. An initial
  bridge outage needed one human open; subsequent native getApp/Recent reopens worked.
- **Current GUI evidence:** four affected scenarios pass 13 declared transitions,
  four actual host reopens and four subsequent edits; three proportionate smoke
  checks and one supplementary main-error isolation pass bring the total to
  **25 passed / 0 failed / 0 blocked**. Actual saved numerical output agrees with
  independent references; the FP regression line agrees at five native image
  coordinates within 0.70 pixels. Smoke checks do not complete the other three
  scenarios' entire current-build matrices. All remaining 70 scenarios are unexecuted.
- **Outstanding release/JASP:** comprehensive P0/P1 GUI acceptance, isolated
  built-package checks and intended actual-host JASP dependency/linkage, menu,
  option-transfer, tables/plots, invalid-input recovery and .jasp save/reopen/edit
  checks remain release blockers. No JASP-host acceptance is claimed.

The final installed and running 0.27.0 build is
`54081947f58fe59e1543ca1c1e21f3828ee34464c719ee23ba84ac7808c42d63`, verified
against the archive/payload receipt and the observed native server descriptor.
The complete package run passed **24,546 assertions / 799 cases**; the complete
native run with final fixture correction passed **4,925 / 98**. Both have zero
failures, errors, testthat warnings or skips. All **140 exact approved SVG checks**
passed; all four UI definitions and source boundaries pass. The package increase
of 42 assertions/two cases and native increase of 88/six cases are regression
coverage over Stage 11C, without changed statistical baselines. The full native
suite was repeated after the development-only fixture correction; package source
and packaged tests were unchanged by that last correction.

All 156 reference keys, all 16 fixture/metadata files, 140 SVGs and 416 indexed
historical local evidence files are preserved. Stage 11D.1's original ledger is
unchanged at 26/7/8; a separate provenance-linked monitor-only reassessment is
34/7/0 and does not substitute for current-build validation.

Recommended sequence: resume the established P0 batches with strict ownership,
verified build, narrow import policy and real reopen/edit evidence; execute and
disposition the rest of P0/P1, then isolated package checks and actual-host JASP.
Stage 11D.2 resolves the pilot's correction gates and makes that process ready;
it does not authorize starting the remaining 70 here or approve release.
