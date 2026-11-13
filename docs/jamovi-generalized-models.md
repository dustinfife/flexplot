# jamovi linear and generalized linear models

The **Linear Model** analysis keeps its internal name `glmbasic`, existing
option names, and exported `glmbasic()` R function. Older saved analyses remain
compatible. **Generalized Linear Model** is a separate analysis, with internal
name `glmgeneralized`.

## Model terms and visualization variables

Choose an outcome and predictors. Model terms start with the selected main
effects; use the selected-predictor supplier to add interactions. Removing a
predictor removes terms involving it. An explicitly empty term list specifies
an intercept-only model. Terms use literal column names and interactions, not
executable expressions.

Visualization suppliers contain only predictors used in the model terms. A
variable occurring only in an interaction is still a model predictor. Removing
all terms involving a variable clears its display selections (unless it retains a quadratic term). The R adapters
also reject selections outside the fitted formulas, including variables still
present in the original variable-selection pool.

Both analyses allow one x variable, one grouping variable, and up to two panel
variables, using different predictors for each role. Numeric groups and panels
use flexplot's existing display bins. Leaving x empty selects a numeric model
predictor if available. For the GLM, native nonpersistent suppliers hide assigned variables from the available list. Each predictor can occupy only one role; removing it from a target makes it available again. X initializes automatically to an available model predictor, and an intercept-only model reports a clear visualization error.

Linear-model comparisons use the union of the two fitted models' predictors and
their shared complete-case dataset. The original `comparisonX` and
`comparisonPanels` options remain supported. Two-model second-axis grouping is
supported only for magnet plots; use panels for numeric-outcome comparisons.
Generalized-model comparison is not implemented.

## Families and coefficient tables

The supported families match the active JASP GLM adapter; Gamma additionally offers a log link. JASP's
custom-link control is commented out; custom links are not exposed here.

- **Gaussian (identity):** finite numeric outcomes that vary. Coefficients are
  in outcome units. Standard errors, tests and 95% Wald intervals use the fitted
  Gaussian GLM's estimated dispersion and residual-df t distribution.
- **Binomial / Logistic (logit):** numeric 0/1, logical, or categorical outcomes
  with exactly two observed levels among eligible observations. Both levels
  must occur. For factors, the first observed factor level is failure and the
  second is success, preserving their original ordering. The success event is
  identified below the table. Coefficients are log-odds coefficients, not
  probabilities or odds ratios; intervals and tests use the normal distribution.
- **Poisson (log):** finite nonnegative integer counts with at least one positive
  count. Coefficients are on the log-mean scale. Dispersion is fixed at one;
  normal-Wald intervals and z tests are used. Robust/overdispersion corrections
  are not provided.
- **Gamma (log):** the preferred Gamma choice for new analyses. Strictly positive finite outcomes, with multiplicative mean coefficients and estimated-dispersion t inference.
- **Gamma (inverse):** strictly positive finite numeric outcomes. Coefficients
  are on the inverse-mean scale. Dispersion is estimated; coefficient tests and
  Wald intervals use the residual-df t distribution returned by `summary.glm()`.
- **Negative Binomial (log):** finite nonnegative integer counts with at least
  one positive count, fitted with `MASS::glm.nb()`. Coefficients are on the
  log-mean scale. Normal-Wald intervals and z tests use the fitted coefficient
  covariance conditional on estimated size (theta). Theta and its standard
  error are reported in the fit table. This is not quasipoisson regression.

Fitting uses the supplied terms and ordinary fitted-model contrasts. Interaction
coefficients retain their conditional interpretation. The coefficient table
reports one row per design-matrix column: estimate, standard error, test
statistic, p-value and interval. No model is refitted for visualization.

The fit table reports N, residual degrees of freedom, deviance, null deviance,
log likelihood, AIC, BIC and dispersion, plus theta statistics for Negative
Binomial models. AIC and BIC use the fitted model's own parameter counts, including
theta. Gaussian deviance is residual sum of squares; other families use deviance
relative to a saturated model. These are not pseudo-R-squared measures.

Complete cases use the outcome and variables in the fitted terms. Unused
selected columns do not exclude observations. Invalid outcomes, rank deficiency
and failed convergence produce analysis errors. Other estimation warnings are
retained in fit-table notes. No bias correction or substitute estimator is used.

## GLM reporting and quadratic terms

The raw coefficient table retains fitted-model estimates, standard errors, Wald
confidence intervals and tests. Supplementary tables reuse `estimates.glm()`:
logistic odds ratios, conditional IRT slopes/thresholds, standardized odds ratios,
and observation-weighted marginal categorical probabilities. IRT rows include
only continuous predictors; nonunique or undefined thresholds remain missing.
Other families display their existing family/link-specific estimates rather than
borrowing the logistic interpretation. The existing non-binomial `estimates.glm()`
helpers cannot summarize some transformed formulas, including quadratic terms;
in these cases an explicit note replaces only the supplementary summaries.
Coefficient inference, fit measures and model predictions remain available.

Fit statistics are transposed into two compact tables: model/deviance/dispersion
and likelihood/AIC/BIC. Longer explanations are separate HTML blocks so they
are not constrained by the old two-column table width. Research references are
attached to the reporting table, not the result group surrounding the plot.

The GLM **Add squared terms** control accepts selected numeric predictors.
It adds literal language calls `I(x^2)` to the supplied model terms. Selecting a
quadratic initially adds its linear term; it can subsequently be removed from
the main term list. This mirrors JASP's additive-polynomial approach without
parsing user text. The independent quadratic selection is saved/restored and
cleared when its predictor is removed. Factor quadratics are rejected in R.

`gammaLog` identifies Gamma/log for new selections. The saved `Gamma` option
continues to mean Gamma/inverse, so existing analyses retain their fitted link.

## Predictions, overlays and uncertainty

All model curves are response-scale predictions from the actual fitted model:
outcome units, success probabilities or expected counts/positive outcomes.
Undisplayed covariates are averaged over eligible observations using the existing
`compare_fits()` marginal prediction method, preserving their interactions.

For binomial models with at least ten distinct numeric x values, **Automatic**
uses the existing supplied-model logistic visualization: continuous model
probabilities and observed bin proportions. This does not call the public
`logistic_overlay()` wrapper, which fits a model. The observed dots are descriptive
proportions, not bin-matched adjusted predictions; the plot caption explains this
distinction. **Raw observations and model predictions** retains the jittered
binary data instead. Categorical and low-cardinality x variables keep native
categorical/magnet plots, and non-binomial families keep their native plots. The explanatory guide is a separate results block rather than a long plot caption. Averaging text appears only when fitted-model predictors are actually undisplayed.

Prediction confidence intervals can be disabled. With all model predictors
displayed, the intervals agree with link-scale `predict.glm(..., se.fit=TRUE)`
intervals transformed to the response scale. For marginal means, the existing
average-gradient covariance calculation supplies the response-scale standard
error; a link-scale delta interval is then back-transformed. Gaussian and Gamma
use t critical values; fixed-dispersion and Negative Binomial models use normal
critical values. Negative Binomial intervals condition on estimated theta.

Gamma inverse-link intervals are decreasing transformations: endpoints are
ordered correctly, and an interval reaching zero on the link scale has an
unbounded upper response limit. Prediction profiles outside the positive Gamma
mean domain are rejected rather than plotted as invalid negative means. Such
extrapolation can occur when crossing predictors in unobserved combinations.

## References, notes and installation

Analysis references are registered in `jamovi/00refs.yaml`, attached through
result definitions and compiled to `R/00jmv.R`. Host jamovi/R software references
are unchanged. The research references are Fife's published 2022 flexplot paper
(DOI: 10.1037/met0000424) and the 2021 graphical-analysis interface paper
(DOI: 10.3758/s13428-020-01520-2).

Linear-model comparison specifications use two paragraphs in a supported table
note. The existing p-value presentation and Bayes-factor note are preserved.

Rebuild and reinstall from the package root, then restart jamovi:

```r
jmvtools::install(pkg = ".", home = "/Applications/jamovi.app")
```

`devtools::load_all()` alone cannot refresh the bundled GUI. Automated tests
exercise generated R6 classes, actual plotting callbacks, and compiled/bundled UI
handlers when the optional tools are available. Actual GUI layout and interaction
still require manual verification in jamovi.
