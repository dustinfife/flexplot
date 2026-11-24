# jamovi linear and generalized linear models

The **Linear Model** analysis keeps its internal name `glmbasic`, existing
option names, and exported `glmbasic()` R function. Older saved analyses remain
compatible. **Generalized Linear Model** is a separate analysis, with internal
name `glmgeneralized`.

## Model terms and visualization variables

Choose an outcome and predictors. Model terms start with the selected main
effects; use the selected-predictor supplier to add interactions. Removing a
predictor removes terms involving it. An explicitly empty term list specifies
an intercept-only linear model. The GLM report waits until at least one model
term (including an eligible quadratic term) is present. Terms use literal column names and interactions, not
executable expressions.

Visualization suppliers contain only predictors used in the model terms. A
variable occurring only in an interaction is still a model predictor. Removing
all terms involving a variable clears its display selections (unless it retains a quadratic term). The R adapters
also reject selections outside the fitted formulas, including variables still
present in the original variable-selection pool.

Both analyses allow one x variable, one grouping variable, and up to two panel
variables, using different predictors for each role. Numeric groups and panels
use flexplot's existing display bins. Leaving x empty selects a numeric model
predictor if available. For the GLM, native nonpersistent suppliers hide assigned variables from the available list. Each predictor can occupy only one role; removing it from a target makes it available again. X initializes automatically to an available model predictor, and a GLM without model terms returns to the waiting instruction.

Linear-model comparisons use the union of the two fitted models' predictors and
their shared complete-case dataset. The original `comparisonX` and
`comparisonPanels` options remain supported. Two-model second-axis grouping is
supported only for magnet plots; use panels for numeric-outcome comparisons.
Generalized-model comparisons are described below.

## Families and coefficient tables

New analyses start with **Distribution and link: Automatic**. Automatic uses
**Binomial / Logistic (logit)** for exactly two observed, nonmissing categorical
or ordered values, logical TRUE/FALSE, or numeric 0/1. Other outcomes use Gaussian;
for example numeric 3/7 is not treated as a binomial outcome. Declared but unused
factor levels do not count as observations. No outcome is recoded for detection.

Selecting any named distribution, including Gaussian, establishes a manual choice
that survives outcome changes and save/reopen. Select **Automatic** again to resume
detection. Older saved named-family choices remain manual; they are never upgraded
from Gaussian to Automatic just because their outcome is binary. A legacy request
omitting the selector retains the historical Gaussian default.

Detection uses the full observed outcome before predictor/common-sample omissions.
The existing fitted-sample validation still applies: a binary full outcome whose
complete-case sample loses a level produces the usual Binomial error. The model
specification reports the actual fitted family, link and success event. Detection
does not reverse event levels, alter comparison methods, or reset plot settings.

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
  error remain available in the retained fitted-model state. This is not quasipoisson regression.

Fitting uses the supplied terms and ordinary fitted-model contrasts. Interaction
coefficients retain their conditional interpretation. The family-aware estimates table
reports one row per design-matrix column, using the column names and substantive
estimates returned by `estimates.glm()`, plus coefficient uncertainty. No model is refitted for visualization.

Generic Model fit and Likelihood fit tables are no longer displayed. N, residual
df, deviance, null deviance, log likelihood, AIC, BIC, dispersion and applicable
theta statistics remain in `results$plot$state$fitStats` and the original model.
No fit or likelihood calculations were removed.

Complete cases use the outcome and variables in the fitted terms. Unused
selected columns do not exclude observations. Invalid outcomes, rank deficiency
and failed convergence produce analysis errors. Other estimation warnings are
retained in estimates-table notes. No bias correction or substitute estimator is used.

## GLM reporting and quadratic terms

Before an outcome, predictors and at least one valid model term are supplied,
only a short selection instruction is shown. All tables and plotting elements
start hidden. Removing required selections hides and clears the existing result
objects and prediction state; it does not delete result objects. A NULL model-term
selection preserves older saved analyses' selected-main-effects convention; an
explicit empty list does not silently add main effects.

There is one coefficient-level **Estimates** table. It reuses the column names
and values of `estimates.glm()` (including its existing display rounding),
augmented with raw-coefficient standard errors and full-precision 95% Wald bounds.
Where the method already supplies a standard-error column, that value and name
are retained rather than duplicated. Log-link families retain `raw.coefficients`,
`multiplicative.coef`, `std.mult.coef` and `Prediction Difference (+/- 1 SD)`.
Inverse-link reciprocal summaries keep their existing interpretation. Logistic
ORs, inverse ORs and standardized ORs share the same coefficient table.

Logistic IRT slopes and thresholds and categorical marginal probabilities have
predictor-level rows, so they retain separate, nonduplicate tables, shown only
when applicable. IRT rows include continuous predictors only. Undefined numeric
and missing cells are left blank; legitimate category labels such as "NA" are
preserved. Supplementary statistics unavailable for quadratic/non-binomial
formulas are identified explicitly; raw coefficients, uncertainty and model
predictions remain available without inventing supplementary values.

**Output Options** has three default-checked sections: **Estimates**,
**IRT Estimates**, and **Marginal Predicted Probabilities**. Each checkbox controls
only its corresponding table. IRT and probability controls appear for Binomial
models; their tables also require applicable continuous and categorical summaries,
respectively. Empty tables remain hidden. Output preferences persist across family
and comparison changes and in saved analyses; older analyses default to showing
all applicable sections. **Include p-values** remains off by default and controls
p-value columns only. P-values supplied by `estimates.glm()` retain that method's
precision; otherwise they come from the fitted model summary.

**Distribution and link** follows the initial outcome and predictor selectors,
before interaction, polynomial and comparison controls. **Model Comparison
Visualization**, inside Model Comparison, selects the comparison x and panel
variables separately from ordinary Model visualization.

Output selections do not change the model specification or any estimates or
predictions. The adapter currently recomputes on presentation changes; avoiding
that would require a separate fit-cache and invalidation design.

Long explanations remain separate HTML blocks. Research references are attached
to the coefficient table rather than the plot. The stable `coefficients` result
ID is retained; obsolete supplementary and generic-fit result IDs are ignored
when old result records are restored. Rebuilding/reinstalling the module is
required to apply the new results schema to the GUI.

The GLM **Add squared terms** control accepts selected numeric predictors.
It adds literal language calls `I(x^2)` to the supplied model terms. Selecting a
quadratic initially adds its linear term; it can subsequently be removed from
the main term list. This mirrors JASP's additive-polynomial approach without
parsing user text. The independent quadratic selection is saved/restored and
cleared when its predictor is removed. Factor quadratics are rejected in R.

`gammaLog` identifies Gamma/log for new selections. The saved `Gamma` option
continues to mean Gamma/inverse, so existing analyses retain their fitted link.

## Predictions, overlays and uncertainty

By default, model curves are response-scale predictions from the actual fitted model:
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

Confidence intervals can be disabled. With all model predictors
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

### Binomial visualization scale

The binomial-only **Visualization scale** selector defaults to **Probability**.
**Logit** displays `qlogis(mean(predict(model, reference, type="response")))`
at each plotting coordinate, using the same eligible reference observations,
moderators, and fitted model as Probability. It does not average linear predictors.
Confidence-interval endpoints receive the same logit transformation. The
**Confidence intervals** checkbox retains its existing saved option and calculations.
Both comparison curves use the selected scale; fitting and comparison statistics
are unchanged. The selection is retained when switching to another family.

Logit display requires Automatic visualization and a continuous numeric x predictor
with at least ten distinct values. Raw 0/1 observations have infinite logits;
raw-observation and categorical-x logit combinations therefore show an informative
plot error rather than a misleading substitute. Observed bins retain the existing
half-observation convention: proportions are bounded by `0.5/n` and `1-0.5/n`
only for display before taking logits. Model probabilities are never clipped.
Exact model probabilities or interval limits of zero/one have infinite logits and
are drawn at plot edges. A grid without finite logits, or undefined saturated
confidence limits, reports an error; disabling intervals can display a curve whose
intervals are undefined. Empty bins are not drawn.

## References, notes and installation

Analysis references are registered in `jamovi/00refs.yaml`, attached through
result definitions and compiled to `R/00jmv.R`. Host jamovi/R software references
are unchanged. The research references are Fife's published 2022 flexplot paper
(DOI: 10.1037/met0000424) and the 2021 graphical-analysis interface paper
(DOI: 10.3758/s13428-020-01520-2).

Model-comparison specifications use the existing specification renderer.
Comparison columns are headed **p**, with method-specific conditional explanations
and a concise BIC Bayes-factor description.

Rebuild and reinstall from the package root, then restart jamovi:

```r
jmvtools::install(pkg = ".", home = "/Applications/jamovi.app")
```

`devtools::load_all()` alone cannot refresh the bundled GUI. Automated tests
exercise generated R6 classes, actual plotting callbacks, and compiled/bundled UI
handlers when the optional tools are available. Actual GUI layout and interaction
still require manual verification in jamovi.

## Optional GLM model comparison

**Model Comparison** is collapsed and disabled by default. Enabling it adds an
independent second set of model terms and squared numeric predictors. Both
models use the selected outcome, distribution and link. They are fitted to the
same complete observations across the union of their required variables;
missing values in unused selected predictors do not remove observations.
Turning comparison off restores the original single-model analysis.

The comparison table reuses `model.comparison()` for AIC, BIC and BIC-based
approximate Bayes factors. Each row's Bayes factor favors that row's model over
the other model. Smaller AIC/BIC is better. No ordinary linear-model F-test or
R-squared is reported. A nested-model asymptotic likelihood-ratio test uses the
fitted model likelihoods and their parameter-count difference; nonnested and
equivalent models have blank test cells. Negative Binomial theta is estimated
separately for each model and included in the likelihood parameter count.
These tests rely on regularity and adequate sample size; sparse or separated
logistic data and unstable dispersion estimates require caution. Fitting
warnings remain visible in the independent diagnostic result near the specification.

Comparison plots use `compare_fits()` response-scale marginal predictions from
the original fitted models and their common eligible population. Choose one x
predictor and up to two panel predictors from either model. There is no comparison
color/symbol selector: use panels instead of unsupported second-axis grouping.
For a continuous binomial x, Automatic retains descriptive binned observed
proportions beside both model curves; these proportions are not bin-matched
adjusted predictions. The Raw option retains jittered binary observations.
Single-model estimates, optional p-values and visualization controls are
unchanged when comparison is disabled.

## Visualization compatibility and compact help

Raw-observation plots and categorical (including low-cardinality numeric) x axes
use Probability scale. The interface disables irrelevant binomial controls and
resets an incompatible Logit selection to Probability. Returning to a continuous
x permits selecting Logit again. When comparing models, the shared scale must
be supported by both displayed x predictors. The R adapter also resolves old
saved combinations safely, without refitting or changing model specifications.
Plots that cannot be drawn show a completed diagnostic rather than an unfilled
image. Observed-bin and marginal-standardization explanations follow their
respective plot and are hidden when irrelevant.

The primary predictor selector initializes main effects. As in Mixed Models,
the optional **Effects and interactions** builder edits the complete term
list, including deliberate interaction-only models. The squared-term builder
adds the linear term by default; it can subsequently be removed in the complete
term builder for a deliberate square-only specification. Model 2 remains
independent. Existing saved term lists remain authoritative; the old Model 1
main-effects option is retained internally for compatibility.

Jamovi 2.7.41 does not expose per-column tooltip/help metadata or heading
footnotes in its result-column API. Stage 7 uses the supported
`Table$addFootnote(col=..., rowNo=..., note=...)` API on relevant existing cells.
No new result-column identifiers or unsupported heading markers are introduced.

GLM comparison plots use 1.2-width fitted curves and lightly shaded confidence
bands (alpha 0.13). Model 1 remains solid and Model 2 dashed, with the existing
model colors and prediction scales. Model 2 specifications appear under
**Model 2 effects**.

## Reading the compact estimates tables

Column labels use **Coefficient**, **1/OR**, **Std. OR**, **1/Std. OR**,
**Multiplier** and **Reciprocal** while retaining the saved result-column IDs.
Confidence-interval headings identify the distribution and level; t-based
intervals retain their residual degrees of freedom in a targeted SE/CI footnote.
SE and confidence limits remain on the raw coefficient scale, including when ratios are shown alongside them.

Notes identify the family/link and binary success event once, on the first visible
summary table, before specialized footnotes. Standardization, prediction-difference
and fitted-contrast explanations attach to relevant effect cells; basic OR and
reciprocal definitions already clear from standard column labels are omitted.
IRT headings identify p = .5; targeted footnotes retain
predictor units, conditioning and the distinction from average marginal effects.
Marginal probabilities retain their averaging interpretation. Hidden and empty
tables have no note rows. Fitting warnings appear in the independent diagnostic
element, even when all optional estimates sections are hidden.

## Reference-panel ghost lines

**Show ghost lines** is off by default and belongs to ordinary **Model
Visualization**, independently of model comparison. With a continuous numeric x
and at least two prepared panels, it copies the core-selected reference curve to
the other panels without adding a duplicate in its source panel. One or two
categorical/numeric panel variables are supported; numeric panels use existing
bins. The checkbox's preference survives temporarily unavailable layouts and
saved analyses. Older analyses default to off.

Color/symbol grouping and categorical x layouts are not supported by this control;
ordinary plots, including magnet plots, remain available. UI metadata determines
potential eligibility; the fitted prediction grid is checked in R. An unavailable
reference or nonfinite curve leaves the ordinary plot and a short explanation
below it. No new reference-selection algorithm or GUI editor is introduced; core
R ghost-reference overrides remain supported.

Probability plots copy reference probabilities. Logit overlays transform those
same marginal probabilities, including the ghost coordinates. Neither the
reference nor destination panel causes a model refit during prediction rendering.
Rendering an enabled request prepares the ordinary plot first for validation,
then uses the existing core ghost path with the same fitted model. It does not
change comparison overlays or Mixed Models CAS ghosts.

GLM panel ghosts use neutral dark gray (`#707070`), a dashed line
(width 1.2, opacity 0.85), and are drawn above confidence bands. This styling is
shared by the ordinary modeling adapters; ordinary fitted curves and exploratory
Flexplot ghosts retain their existing appearance.

## Editing plot labels

Each GLM visualization has its own **Plot Labels** editor: one under ordinary
Model visualization and one under Model Comparison. Edit titles, subtitles,
axis titles, and applicable legend/facet titles and categories. Blank cells keep
the automatic text, including interpretation subtitles. Categorical axis tick
labels are not edited by this control.

Edits affect display text only. They do not rename data, change predictions,
confidence limits, panel membership, reference ghosts, colors or line patterns.
Ordinary/comparison outputs and probability/logit displays retain separate edits.
Temporarily unavailable elements keep their edits in a hidden saved store, ready
when that configuration returns. Older saved analyses retain automatic labels.

Stage 5B shares reference-panel ghost styling and eligibility with LM and eligible
Mixed Models visualizations, including automatic continuous x selection. See
[Modeling reference-panel ghost lines](jamovi-model-ghosts.md).

## Stage 7 statistical footnotes

SE and confidence-limit markers identify raw-coefficient uncertainty. Standardized
ratio and prediction-difference markers attach to effect rows rather than the
intercept's reference-profile prediction. Interaction-dependent coefficients get
a shared conditional-interpretation marker. Gamma inverse-link reciprocals remain
explicitly distinguished from response ratios; negative-binomial uncertainty still
conditions on estimated theta. IRT footnotes retain conditioning, probability
change per predictor unit rather than average marginal effects, predictor-unit
thresholds, and undefined/nonunique blank thresholds. Marginal probability cells
identify averaging over other observed predictors.

Writers clear existing rows and notes before repopulation. Hidden GLM estimates
sections have their cell footnotes cleared. Family/link/success context moves to
the first visible summary table, without repetition beneath every table. Restored
analyses reuse existing result objects and regenerate current markers and headings.

Available comparison tests have concise method labels, rather than a generic
nesting restriction. Unavailable LM/GLM tests distinguish equivalent designs,
nonnested designs, and invalid comparisons under the existing test. Negative
binomial comparisons retain the separately estimated size qualification. Mixed
Models retains all ML, random-structure, boundary, singularity and convergence
qualifications. All three modules use:
"Approximate BIC Bayes factor favoring the row model over the other model."
The values and their directions are unchanged, including Mixed Models' ML basis.
