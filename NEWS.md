# flexplot 0.27.0

## Model comparisons

* `compare_fits()` is now the canonical API. `compare.fits()` remains an exported,
  supported alias with the same signature and behavior.
* The default `prediction_method="marginal"` uses observation-weighted marginal
  standardization. Displayed variables are fixed at each plotted coordinate;
  predictions are averaged over all eligible observations in the shared reference
  population, without random subsampling.
* Explicit `prediction_method="observed"` averages predictions for actual,
  common eligible estimation observations within displayed categories or bins,
  retaining correlations between displayed and omitted predictors.
* Use `prediction_method="reference"` to request historical reference profiles:
  omitted numeric predictors are held at their mean and omitted categorical
  predictors at their mode, for either model.
* Observation identities and estimation samples are checked before prediction;
  incompatible or unidentifiable common samples now produce an informative error
  instead of recycling fitted values or attaching them to different observations.
* Native plots and `return.preds=TRUE` consume the same statistical results.
  Existing magnet grouping and model-column/conditioning-row facets are retained.
* `report.se=TRUE` provides coefficient-covariance/delta-method standard errors
  for lm/glm predictions, including averages. Other model classes report that
  this uncertainty calculation is unsupported. Numeric-label ordinal scores and
  binary tree/forest probabilities are supported; multicategory probability
  matrices and neural-network adapters are not implemented.
* None of the prediction methods automatically estimates a causal effect.
  Observed conditional averaging differs from marginal standardization, partial
  dependence, and accumulated local effects (ALE).
