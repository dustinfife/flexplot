# Preparing flexplot coordinates and rendering external draws

`prepare_flexplot()` is the public preparation interface. It does not fit a
model, generate predictions, or create a ggplot. It reuses the existing automatic
formula selector, variable typing, binning, display preprocessing, and reference
profile generator.

```r
prepared = prepare_flexplot(
  data=small,
  model=y ~ x + b + a,
  formula=y ~ x + b | a
)
```

`model` can be an ordinary formula, so no fitted or artificial model object is
required. If it is a fitted object, its available factor levels and contrast
specifications are returned. Omit `formula` to request the same automatic layout
heuristic used internally by flexplot. Alternatively, specify `outcome` and
`predictors` without a `model`. Column names remain literal identifiers, including
nonstandard names.

## Preparation contract (version 1)

The returned list has class `flexplot_preparation` and these documented fields:

| Field | Meaning |
| --- | --- |
| `formula` | Selected **visualization** formula; use it when calling `flexplot()`. |
| `model_formula` | Supplied/recovered full model specification, where available. |
| `data`, `rows` | Eligible original observations and their input-row indices. |
| `plot_data` | Observations with native display conversion, ordering, and bin columns. |
| `grid` | Unique prediction profiles at original predictor coordinates, with display-bin identifiers. |
| `reference` | One-row settings for predictors omitted from the visualization. |
| `variables` | Outcome, full predictor set, and numeric/categorical classifications. |
| `roles`, `display_roles` | `x`, optional `group`, and `panels`, before/after binning. |
| `binning` | `bins`, resolved `breaks`, `labels`, and binned-variable names. |
| `factor_levels`, `contrasts` | Factor encodings and available fitted-model contrast specifications. |

Eligibility uses complete observations across all required outcome/model/plot
variables. Omitted numeric predictors use their eligible-data means; categorical
predictors use the existing modal-level convention, not necessarily the fitted
reference category. The grid is a **reference profile**, not marginal
standardization. Its primary x values come from observed coordinates. Displayed
numeric groups/panels use existing within-bin means rounded to three decimals.
Both the ordering and these representative-value conventions match the current
helpers rather than introducing a new grid algorithm.

Classifications refer to original coordinates; low-cardinality numeric predictors
can become ordered factors in `plot_data`. The outcome must exist and have
complete eligible observations. Fitted binomial models are checked for compatible
outcome coding, using the same validation as `compare_fits()`.

The `grid` does not contain the observed response. Downstream prediction code
should use the full model specification and supplied contrast metadata, evaluating
transformations itself. It should calculate predictions on the scale to be
plotted. Preparation does not infer coefficients or a probability distribution.

## Coherent external draws without Bayesian dependencies

This example draws coefficient vectors with base R. Each vector defines one
relationship across every x coordinate, category, and panel.

```r
prepared = prepare_flexplot(small, model=y ~ x + b + a,
                           formula=y ~ x + b | a)
terms = stats::delete.response(stats::terms(prepared$model_formula))
design = stats::model.matrix(terms, prepared$grid,
                            contrasts.arg=prepared$contrasts,
                            xlev=prepared$factor_levels)
set.seed(71)
coefficients = matrix(rnorm(ncol(design) * 30), nrow=ncol(design))
predictions = do.call(rbind, lapply(seq_len(30), function(draw) {
  frame = prepared$grid
  frame$draw = draw
  frame$prediction = drop(design %*% coefficients[, draw])
  frame
}))

p = flexplot(
  prepared$formula, prepared$data,
  prediction=predictions,
  prediction.draw="draw",
  prediction.alpha=.12,
  bins=prepared$binning$bins,
  breaks=prepared$binning$breaks,
  labels=prepared$binning$labels
)
p
```

The same interface can be called from flexbayes or another package: use its own
prediction engine on `prepared$grid`, then supply its externally computed values
to `flexplot()`. No flexbayes code or dependency is needed by flexplot.

## Prediction data contract

- Supply a data frame with every visualization predictor and a finite numeric
  `prediction` column. Use **original** numeric coordinates for variables that
  flexplot bins. Pass through the preparation's binning settings.
- Add a nonmissing scalar draw ID per row and name that column with the
  **named** `prediction.draw` argument. Reuse a draw ID across all coordinates,
  groups, and panels belonging to the same coefficient/parameter draw.
- `model` is optional in draw mode. If provided it must identify one model;
  it must never contain individual draw IDs. Numeric, categorical, and binary
  regression relationships can have any number of draws from that model.
- Group and panel values must be observed display levels so native slots and
  scales remain defined. New combinations of known levels can be supplied.
- Supply at most one value per displayed x/group/panel coordinate within each
  draw. Ambiguous duplicates are rejected, never averaged. Numeric x paths are
  sorted by x. Categorical x receives points and connecting lines using the
  native category ordering and shared group offsets. Singleton predictions
  remain points.
- Draw, model, group, and panel identities form independent path keys. Draw IDs
  are never mapped to color, linetype, shape, or a legend. Categories retain the
  ordinary color/linetype conventions; ungrouped draws share one color and solid
  linetype. `prediction.alpha` controls supplied-layer transparency only.
- Draw mode suppresses internally fitted trends. It uses identity geoms and
  supplied values directly. In binary magnets it retains observations/counts
  without fitting observed logistic curves; supplied values must be probabilities
  for the second observed factor level. No prediction uncertainty is inferred.
- No prediction-data formula attribute is required: pass `prepared$formula`
  explicitly. Internal layer/path metadata is not part of the public contract.

## Scope and compatibility

Existing `prediction=` calls and fitted-model comparisons retain their existing
behavior when `prediction.draw` is NULL (the default). This opt-in avoids treating
many draws as many compared models and avoids the two-model linetype limitation.

Preparation does not select mixed-model display clusters or decide whether
random effects should enter predictions; that is the downstream engine's policy.

Draw mode currently handles one model per plot. Multiple-model *draw ensembles*,
draw ghosts, paired-difference overlays, and multicategory association predictions
are not supported. Distribution plots do not have an x path for draw rendering.
Use literal visualization formulas; model transformations belong in the model
specification/prediction engine. The existing grid helper rounds numeric bin
representatives; preparation errors rather than silently omitting bins when
rounding cannot represent them. Factor transformations inside model formulas
continue to follow the underlying helpers' reference-profile conventions;
preconvert categorical columns when a raw numeric mean would not be a valid level.

Returned plots remain native ggplots and can be modified with normal ggplot2
layers, labels, themes, and scales.
