# Prediction design for `compare_fits()`

This document records the agreed statistical and visualization design, the
current implementation, and the remaining differences. It does not announce
implementation of the proposed changes. `compare.fits()` remains an exact alias
of `compare_fits()`.

The design uses **RE** to describe whether estimated random effects contribute
to predictions. The current R argument is lowercase **`re`**; R argument names
are case-sensitive. Examples below use the existing API, not a new `RE` argument.

## Prediction methods

### Marginal: a common full reference population

**Agreed design.** `prediction_method = "marginal"` is the default. Establish one
population of all common eligible estimation observations for the supplied
models. Use that same population for every model and every plotted coordinate.
Eligibility includes complete required variables and verifiable estimation-row
identities; it does not mean every row of an arbitrary supplied dataset.

For each grid coordinate:

1. Copy the common reference population.
2. Set all displayed predictors to their grid values, including second-axis
   grouping variables and conditioning variables.
3. Leave undisplayed covariates at their observed values.
4. Predict using each original fitted model and average the predictions over
   all reference observations.

For model `m`, reference observations `r_i`, and displayed coordinate `g`, this is
`marginal_m(g) = mean_i predict_m(profile(r_i, g))`.

This is equal-observation marginal standardization, not equal-cluster weighting.
Fitting weights remain part of the fitted model; they do not automatically
become reference-population averaging weights. Transformations, interactions,
offsets, factor contrasts, and prediction scales must retain the supplied
model's prediction semantics. This quantity is not automatically a causal effect.

**Current implementation.** `compare_fits_marginal()` uses every row it receives;
the previous random cap of 300 rows has been removed. It uses one shared
reference frame for both models and all coordinates. The averaging stage is
deterministic for fixed models, coordinates, and reference rows, and does not
draw random numbers.

**Remaining conflict.** For mixed models, upstream preparation currently filters
the common eligible data to selected clusters *before* marginalization. Thus the
current reference population contains all eligible rows in those selected
clusters, rather than the full common eligible population. The agreed full-
population design requires separating the population used for averaging from
the clusters selected for display. Display selection should limit plotted
clusters, not silently redefine the marginal reference population. This
separation is proposed work, not current behavior.

### Observed: predictions at actual covariate combinations

**Agreed design and current behavior.** `prediction_method = "observed"` predicts
at actual eligible observed covariate combinations, then summarizes those
predictions within the displayed categories, groups, or bins. It does not replace
displayed predictors throughout a common population as marginal standardization
does. It is particularly useful for evaluating model fit against observations.

The current numeric-x summary convention uses quantile-bin midpoints when x has
at least ten distinct values. These summaries are not a promise to return an
unsummarized fitted value for every original row.

### Reference: retain the existing profile method

**Agreed design and current behavior.** Preserve
`prediction_method = "reference"`. It predicts on reference profiles, holding
undisplayed numeric covariates at their means and categorical covariates at their
modes using the existing helpers. It is distinct from averaging predictions
over the reference population. No removal or reinterpretation is proposed.

## Random effects and displayed clusters

The prediction method, random-effect contribution, and plotting layout are
separate choices.

| Setting | Agreed behavior | Current implementation |
| --- | --- | --- |
| RE = TRUE (`re = TRUE`) | Include estimated random effects from the original fitted model for three randomly selected displayed clusters by default. Retain estimated random intercepts and slopes. | Uses `predict(..., re.form = NULL)`. The current default is `clusters = 3`, with a configurable cluster count and an existing stratified/random selection policy. |
| RE = FALSE (`re = FALSE`) | Exclude random effects while preserving the requested groups and facets, even when fitted curves repeat across clusters. | Uses `predict(..., re.form = NA)` and retains displayed cluster variables in the prediction grid. Cluster selection still occurs upstream, regardless of `re`. |

Excluding random effects does not exclude fixed-effect moderators or their
interactions. Curves need not be identical when their displayed fixed-effect
profiles differ. When those profiles are the same, identical curves across
cluster panels are valid and are not a reason to collapse the requested layout.

**Never refit a supplied mixed model on the selected clusters.** Cluster selection
changes what is displayed, not the estimated fixed effects, random effects,
variance components, or fitting weights. The current `compare_fits()` prediction
path uses the supplied fitted model; no cluster-subset refitting was found in
that path.

Current stream conventions also need to remain explicit: with one mixed model,
`re = TRUE` returns both fixed-effect and conditional random-effect prediction
streams. With two mixed models, it retains their conditional streams. The agreed
requirement to include random effects does not by itself decide whether to
remove the single-model fixed-effect comparison stream.

Random display-cluster selection can consume random numbers and change the
displayed panels under different seeds. This is separate from RNG-free marginal
averaging. Observation sampling and jitter are also separate plotting controls.
Deterministic averaging does not make the whole stochastic visualization call
RNG-free.

Current mixed-model comparisons support one shared grouping factor. Also,
`visualize.lmerMod()` infers its random-effect display choice from the plotting
formula through `random_plot`; an explicit, consistently separated RE policy
across wrappers is not yet established.

## Cluster-adjusted scatterplots are an explicit visualization

**Agreed direction.** CAS should be requested explicitly and independently of
RE. Setting RE = FALSE means excluding random effects from predictions; it
does not mean adjusting the observed outcomes. CAS can help identify systematic
residual cluster patterns by examining adjusted observations alongside model
predictions and, when requested, descriptive smooths.

**Current implementation.** `cluster_adjusted_scatter()` already provides a
separate explicit entry point, and no automatic CAS call was found in the
`compare_fits()` path. Its adjusted outcomes and model-based curves are distinct
from an ordinary model comparison. With a supplied model, CAS uses that model;
its convenience interface can fit a model when none is supplied. That is not
permission to refit a supplied model on displayed clusters.

The proposed work is to preserve this separation and make it clear across the
visualization APIs, not to claim that `compare_fits()` currently couples RE to
automatic CAS. CAS descriptive smoothing is likewise a separate request, not a
replacement for supplied-model prediction curves.

## Future optimization: exact linear-model marginalization

**Proposed; not implemented.** Start with `lm`, then extend to `lmer`. Replace
repeated full-data prediction averaging only where exact numerical equivalence
can be established. Keep full-data prediction averaging as the correctness
oracle. Do not reintroduce reference-population sampling.

For a linear predictor `X_i(g) beta + o_i(g)`, average the correctly constructed
model-matrix rows and offsets:

`mean_i(X_i(g)) beta + mean_i(o_i(g))`.

For a displayed mixed-model cluster `j` with RE = TRUE, include
`mean_i(Z_i(g, j)) b_j`, using its original estimated random effects. With
RE = FALSE, omit that contribution. Random slopes require the appropriate
random-effects design rows, not merely an averaged random intercept.

Numerical equivalence tests must cover:

- Interactions between displayed and undisplayed predictors and moderators.
- Transformed predictors, including polynomial terms, recomputed from each
  modified profile rather than reused as stale evaluated columns.
- Categorical and ordered-factor levels and the fitted contrasts.
- Offsets evaluated using the original model's prediction semantics.
- Weighted fits, preserving their coefficients and the agreed equal-observation
  reference averaging rather than introducing new population weights.
- Random intercepts and slopes, both RE settings, and shared reference rows
  across models and plotted coordinates.
- Returned estimates and any supported uncertainty calculations, including the
  covariance of the averaged design/gradient rather than averaged individual SEs.

No timing estimates, performance warnings, or optimization implementation are
part of this document.

## Nonlinear response-scale marginalization

For nonlinear models, response-scale marginalization generally requires
`mean_i inverse_link(eta_i(g))`, not
`inverse_link(mean_i eta_i(g))`. An exact linear-model optimization must not be
silently generalized to nonlinear response predictions.

The current GLM path averages predictions on the requested response or link
scale. The mixed-model adapter currently forces response-scale predictions,
including for GLMMs; general link-scale GLMM support is not implied here.
Excluding estimated random effects in a GLMM is also distinct from integrating
over a hypothetical random-effects distribution.

## Implementation map and open decisions

| Area | Source | Status |
| --- | --- | --- |
| Eligibility, method dispatch, model labels, alias | `R/compare_fits.R`; `R/compare_fits_predictions.R` | Existing; preserve shared eligibility and outcome-scale validation. |
| Full prepared-population marginal averaging | `compare_fits_marginal()` in `R/compare.fits_methods.R` | Implemented without a row cap. Full pre-display-selection mixed-model population remains a target. |
| Observed summaries and reference profiles | `R/compare_fits_predictions.R`; `get_plotted_line()` | Existing; preserve their distinct meanings. |
| Cluster selection and data filtering | `prepare_data_for_compare_fits()`; `subset_random_model()` and `stratified_sample_re()` in `R/mixed_mod_functions.R` | Existing; separate display selection from marginal reference data in future work. |
| Mixed-model prediction streams | `generate_predictions.lmerMod()` and its GLMM alias; `compare_fits_prediction_frame()` | Original fitted model is retained; current one-model/two-model stream conventions differ. |
| Layout-driven random-effect display | `R/visualize_lmer.R` | Existing wrapper policy; explicit RE semantics across wrappers require review. |
| Explicit CAS | `R/plot_cas.R` | Existing separate entry point; broader API separation is an agreed direction. |
| Exact model-matrix averaging | No optimized implementation yet | Proposed, first for `lm`, then `lmer`, subject to equivalence tests. |

No uppercase `RE` argument, new sampling option, new refitting path, or change to
prediction-method semantics is implemented by this design document.
