# Mixed Model analysis: inspected design and implementation gates

Status: proposal only. No Mixed Model analysis is registered or implemented in
this change. The GLM/LM presentation polish is independent of this proposal.

## Existing JASP behavior and reusable components

| Feature | Current implementation | Proposed jamovi use |
|---|---|---|
| Fixed effects and interactions | `inst/qml/mixedmod.qml` term builder; `R/mixedmod_jasp.R::.mixedmod_compute()` | Reuse jamovi's tested literal-name term controls and formula construction, not the JASP formula-text builder. |
| Fitting | `lme4::lmer()`, default REML | Same engine; explicit REML/ML selector, REML default. Do not simplify failed random structures. |
| Random effects | One grouping variable; selected fixed terms become correlated slopes, with the usual implicit intercept; otherwise random intercept | Separate grouping and random-term controls. Independent specifications for Model 2. More than one grouping variable extends JASP's interface. |
| Reporting | Original-model coefficients, SEs, t statistics; VarCorr SDs | Reuse `fixef()`, summary coefficient SE/t values and `VarCorr()` as JASP does. Distinguish covariance/correlation rows from variance/SD rows. |
| Estimates method | `estimates.lmerMod()` also fits a random-intercept baseline for variance reduction/ICC | Do not apply this whole workflow blindly to crossed/multiple groups: its documented extractor does not robustly support them. No generic fit table is needed. |
| P-values | JASP fixed-effect table has no p-values; ordinary lme4 summaries have no denominator-df p-values | Default Include p-values off; unavailable p-values blank with explanation. Do not introduce a new approximation or dependency. |
| Visualization | `visualize()` from original fitted model; default three displayed clusters; fixed and cluster-specific relationships | Reuse the original fitted models and existing cluster-selection/reference-population conventions. Explicit population/cluster-specific selector; never silently replace requested cluster predictions. |
| Comparison | No comparison controls in the supplied mixed-model QML/adapter | New jamovi controls; reuse supported comparison/prediction paths with statistical guards. |

## Proposed interface

- Outcome and fixed-predictor selectors; fixed model terms/interactions and numeric
  quadratic selections, using the working GLM synchronization handlers.
- Separate grouping variables and random-effects section: random intercept
  enabled by default, selected random slopes, and clear specification preview.
  Multiple grouping factors require an explicit per-group specification design.
- REML/ML estimation; Include p-values unchecked by default.
- Fixed-effect estimates/uncertainty and separate random-effects summaries.
- Visualization: x, up to two panels, color/symbol, population versus
  cluster-specific predictions, and number of displayed clusters (default three).
  Only fitted predictors/grouping factors can be selected.
- Optional collapsed comparison section with independent Model 2 fixed,
  quadratic, grouping and random terms. Fit a common complete-case population.
  Both plots and tables use Model 1 / Model 2 labels and solid/dashed curves.
- Save all specifications as native options; use the existing term/supplier
  synchronization, avoiding character formula parsing.

## Gates before implementing the complete requested analysis

1. **Mixed-model prediction uncertainty is unavailable.**
   `R/compare.fits_methods.R::compare_fits_marginal()` explicitly rejects
   `report.se=TRUE` for lmerMod. The older `generate_predictions.lmerMod()`
   returns fixed/conditional predictions without uncertainty. Neither the
   JASP adapter nor mixed-model geometry helpers supply the missing bands.
   Define the target first: fixed-mean confidence uncertainty, uncertainty
   conditional on estimated random effects, or prediction intervals are different.
   Do not manufacture bands by refitting a smoother.
2. **Multiple grouping factors are unsupported by the shared plotting path.**
   `R/compare_fits.R` requires exactly one shared grouping variable, even for a
   single supplied mixed model. Extending cluster selection, crossed reference
   profiles and prediction streams requires shared work. lmer itself can fit
   multiple groups, so fitting alone must not be mistaken for visualization
   support.
3. **Comparison guards must distinguish fixed and random structures.**
   `model.comparison()` refits REML models with ML for its table, uses formula-term
   nesting checks and delegates to ordinary anova likelihood-ratio tests. Those
   are insufficient guards for arbitrary random-structure comparisons.
   Different fixed effects require ML; REML likelihoods are comparable only with
   the same fixed-effect design. Nonnested models have no nested LRT. Changes to
   variance components can be boundary tests: leave their ordinary chi-square
   p-values blank, with a note, unless an explicitly supported procedure is
   agreed. AIC/BIC/directional BIC Bayes factors must use comparable likelihoods
   on the same observations. Do not forward an unqualified shared-helper p-value.

These findings trigger the requested stop before broad shared statistical or
prediction changes. A narrower first milestone (one grouping factor, original
model predictions, no plot bands with an explicit limitation) is possible, but
would not satisfy the complete visualization requirements as written.

## Suggested sequence after agreement

1. Establish the mixed-model uncertainty target and add independent numerical
   tests in the shared prediction layer.
2. Agree on multiple-group display/cluster-selection semantics and implement
   those separately, or explicitly limit the initial analysis to one group.
3. Add the R6 analysis, native controls, fixed/random reporting and saved-option
   tests; compare against independently fitted lmer models.
4. Add guarded common-data comparisons, testing fixed/random differences,
   ML/REML rules, nonnesting and boundary-test blanks.
5. Extend the generated smoke document with intercept, slope, interaction and
   supported comparison cases, then verify them in the actual jamovi GUI.
