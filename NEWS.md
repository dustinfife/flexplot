# flexplot (development version)

## Model comparisons

* `compare_fits()` is now the canonical API. `compare.fits()` remains an exported,
  supported alias with the same signature and behavior.
* The default `prediction_method="observed"` averages predictions for actual,
  common eligible estimation observations within displayed categories or bins.
  This is an intentional change in the statistical quantity plotted. Correlations
  between displayed and omitted predictors are retained.
* Use `prediction_method="reference"` to request historical reference profiles:
  omitted numeric predictors are held at their mean and omitted categorical
  predictors at their mode, for either model.
* Observation identities and estimation samples are checked before prediction;
  incompatible or unidentifiable common samples now produce an informative error
  instead of recycling fitted values or attaching them to different observations.
* Both rendering paths and `return.preds=TRUE` consume the same statistical results.
  Existing magnet grouping and model-column/conditioning-row facets are retained.
* `report.se=TRUE` provides coefficient-covariance/delta-method standard errors
  for lm/glm predictions, including averages. Other model classes report that
  this uncertainty calculation is unsupported. Numeric-label ordinal scores and
  binary tree/forest probabilities are supported; multicategory probability
  matrices and neural-network adapters are not implemented.
* Neither observed conditional averages nor reference profiles are automatically
  causal effects. Observed averaging is not marginal standardization, partial
  dependence, or accumulated local effects (ALE).
