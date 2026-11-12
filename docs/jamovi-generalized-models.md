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
all terms involving a variable clears its display selections. The R adapters
also reject selections outside the fitted formulas, including variables still
present in the original variable-selection pool.

Both analyses allow one x variable, one grouping variable, and up to two panel
variables, using different predictors for each role. Numeric groups and panels
use flexplot's existing display bins. Leaving x empty selects a numeric model
predictor if available. An intercept-only model has no predictor available for
these roles.

Linear-model comparisons use the union of the two fitted models' predictors and
their shared complete-case dataset. The original `comparisonX` and
`comparisonPanels` options remain supported. Two-model second-axis grouping is
supported only for magnet plots; use panels for numeric-outcome comparisons.
Generalized-model comparison is not implemented.

## Families and coefficient tables

The five family/link combinations match the active JASP GLM adapter. JASP's
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
categorical/magnet plots, and non-binomial families keep their native plots.

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
