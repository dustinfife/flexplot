# Mixed Model — milestone 1

The jamovi **Mixed Model** analysis fits Gaussian linear mixed models with
`lme4::lmer()`. It uses one grouping variable, REML by default or optional ML,
and the same fixed-term and quadratic controls as the other model analyses.
Selected predictors initially enter as main effects. Interactions and explicit
term removals are preserved when restoring an analysis. Numeric squared terms
add their linear main effect by default; it can subsequently be removed.

## Specification and fitting

Fixed and random formulas are previewed separately. The random-intercept checkbox
and the random-slope selection are independent. Selected random slopes are literal
predictor columns, numeric or categorical; factor slopes use lme4's standard
contrasts. Random effects are correlated when fitted together:

- Intercept only: `y ~ x + (1 | id)`.
- Intercept and slope: `y ~ x + (1 + x | id)`.
- Intercept-free slope: `y ~ x + (0 + x | id)`.

At least one random component is required. Fixed interactions and numeric
quadratics are supported; random interaction/polynomial terms and multiple
grouping factors are outside this milestone. A predictor can be selected as a
random slope independently of its fixed term. Removing a predictor clears its
squared/random-slope selections and invalid visualization roles.

The **Include Interaction Terms** and **Include Polynomial Terms** checkboxes control the model,
not just the interface. Turning one off clears its selected terms, hides its
builder, and refits while preserving selected main effects. Turning it back on
only reveals the builder; removed terms are not restored automatically. Both
options default off for new analyses. Explicit saved settings are restored unchanged.
Restoration defers normalization until all saved options have loaded. R also
excludes disabled terms if a stale saved selection reaches the adapter.

Both fixed and random variables determine complete-case eligibility. Unused
selected columns do not remove observations. The outcome must be finite numeric
and vary, and there must be at least two observed groups. Singular fits and
convergence diagnostics are displayed alongside the estimates; the requested
structure is never silently reduced. Redundant fixed terms that lmer would drop
are reported as an invalid specification rather than presented as a simplified
fit. There are no alternative optimizer or simplified-model retries.

## Reporting

Fixed coefficients and standard errors follow the original model's `fixef()` and
summary, as in the JASP adapter. They have their usual conditional coefficient
interpretations, fitted contrasts, and outcome/predictor units. The 95% normal-Wald
coefficient intervals are `estimate +/- qnorm(.975)*SE`. They are asymptotic mean-
coefficient uncertainty, not prediction intervals; small numbers of clusters can
make this approximation unreliable.

**Include p-values** defaults off. If enabled, the p-value column is blank with
an explanation: ordinary lme4 fits do not provide denominator degrees of freedom
or the corresponding p-values. No approximation or new dependency is added.

The main random-effect table reports grouping factor, term, variance and SD,
including residual variance and SD, directly from `VarCorr()` on the original
model. **Random-effect correlations** defaults off. When requested and at least
two random-effect coefficients are fitted, a separate symmetric correlation
matrix uses the fitted term names, diagonal values of 1, and the estimated
off-diagonal correlations. No empty matrix is shown for a single random term.
No variance-component intervals are supplied.

Singular-fit diagnostics appear immediately below the model specification,
before the estimates. They describe rank deficiency and advise caution. Redundant lme4 boundary
messages are removed; distinct convergence diagnostics remain visible there.
Research citations are analysis-level references, not unexplained numbers under
the fixed-effect table.
The adapter does not invoke the baseline refits, ICC or supplementary variance-
reduction calculations in `estimates.lmerMod()`. No generic fit table or model
comparison is included.

## Visualization

X, up to two panels and color/symbol roles are restricted to fitted-model
variables. Use an actual predictor for x; the grouping variable belongs in panels
or color/symbol. The grouping selection initializes a panel without overwriting
a restored grouping-color layout. Plot types are explicitly labeled:

- **Population-level** excludes estimated random effects (`re.form=NA`).
- **Cluster-specific** includes the original model's estimated random effects
  (`re.form=NULL`), and requires grouping to be displayed. The adapter selects
  the existing conditional prediction stream before rendering, so the additional
  population reference stream cannot be mistaken for a second compared model.

- **Cluster-adjusted scatterplot** calls the existing
  `cluster_adjusted_scatter(formula, object=fitted_model, adjust_re="all", smooth=FALSE)`.
  It removes the estimated random contribution from each fitted observation and
  draws the original model's fixed-effects curve. No model is refitted and no
  separate descriptive smoother is added. It uses all eligible fitted observations,
  so the displayed-cluster-count control is disabled for this mode. Displayed
  numeric moderators use CAS bin means, and undisplayed covariates use its
  observation-weighted marginal standardization. This remains distinct from the
  population and cluster-specific prediction displays.

CAS retains the existing package limitations: one grouping factor, no offsets
or transformed responses, and original fixed predictors must be recoverable
from the model frame. Supported quadratics and interactions are passed through.
Errors from CAS are displayed rather than replaced with another adjustment or
smoother. Its existing ghost restrictions also remain unchanged.

Predictions use the existing full-reference marginal machinery: displayed
predictors are substituted at the plotting coordinates, while undisplayed
covariates retain all eligible reference observations. The existing bin-median
convention is retained for numeric panels. Both modes display observations from
up to three randomly selected clusters by default, configurable from 1 to 20.
Population curves can therefore be identical across cluster panels. Neither
mode refits on the selected clusters. Full eligible outcome-axis coverage is
retained. There are **no prediction confidence bands or descriptive smoothers**.

Unsupported layouts produce an explanatory plot error. In particular, an
intercept-only model has no predictor for an x-axis plot; a cluster-specific view
needs grouping in panels/color; and the existing formula-text grouping extractor
cannot reliably visualize nonstandard grouping names containing spaces or
formula punctuation. Such names can still be used for fitting/reporting. These
are optional plotting limitations, not blockers for the supported one-group
analysis. Mixed-model comparison is available as described below.

### Custom plot labels

The collapsible **Plot Labels** editor lists only elements in the generated plot:
x/y titles, an applicable color/line/symbol legend title and its categories,
and row/column panel titles and the displayed facet levels. Numeric bins and
the selected displayed clusters come from the actual plot, not a second binning
algorithm. Edit the Custom label cells; leave a cell blank to retain its automatic
label. Labels affect returned ggplot objects only, never model data, group keys,
contrasts, facet membership, or predictions.

Label edits update the visualization while reusing the existing fit and selected
clusters. Custom labels are keyed by variable, role, and original level, so they
survive unrelated settings changes and saved-analysis restoration. Removing a
plot role hides its controls but retains its labels for later reuse. The same
editor supports population, cluster-specific, and CAS displays. Label metadata
is returned through jamovi's options response so the editor and saved options
stay synchronized.

## Model comparison

**Compare two models** defaults off. Model 2 has independent main effects,
interaction and polynomial switches, squared terms, random intercept/slopes,
and ML/REML estimation. Both models share the outcome, grouping variable and
selected predictor pool. The shared visualization roles may use predictors in
either model. Saved settings and dependent selections follow the same native
synchronization rules as Model 1.

The original Model 1 tables retain its requested estimation and its own eligible
observations. Separate comparison fits use identical complete observations over
the union of variables actually required by either formula, including random
slopes. Consequently, comparison Model 1 can differ from the original table
when Model 2 requires additional variables with missing values; the comparison
sample size and this distinction are stated in the output.

All comparison AIC, BIC and likelihood calculations use ML copies, obtained with
`lme4::refitML()` when needed. REML likelihoods are never compared across fixed
effects. The original fits, their requested ML/REML settings, and the fits used
for visualization are preserved. The existing `bf.bic()` supplies the BIC-based
approximation: Model 1's value favors Model 1 over Model 2 and vice versa. These
reciprocal values are not exact Bayes factors.

An ordinary asymptotic chi-square LRT p-value is reported only for nested
fixed-effect design spaces with identical actual random-effect designs and
converged, nonsingular ML fits. Fixed nesting is checked using model matrices,
including contrasts and polynomial terms. Nested random-effect comparisons
retain the LR statistic and parameter-count difference, but do not receive an
ordinary chi-square p-value. A concise note explains the missing p-value only
when p-values are enabled. Random nesting requires the smaller correlated
block's actual random-design components to be a subset of the larger block's.
Nonnested/equivalent comparisons omit the LRT and the random-effects p-value note.

Singular but successfully optimized ML fits retain meaningful nested LR
statistics and degrees of freedom, along with descriptive information criteria;
the existing conservative omission of BIC Bayes factors and ordinary p-values
is preserved. Convergence failures omit comparison statistics. Unavailable cells
are blank and explained. **Include p-values** controls the entire p-value column,
not just its cell values; it defaults off.

The comparison visualization replaces the single-model image while comparison
is enabled, using `compare_fits()` with two named prediction streams. Population
and cluster-specific views use the comparison-sample original fits, never the
ML copies created solely for statistics. Existing x/panel selections and custom
labels are retained. Model colors and line types distinguish Model 1 and Model 2;
there are no prediction confidence bands. Cluster-specific views require the
shared grouping variable in panels. CAS during comparison displays Model 1 and Model 2 side by side, each with its
own adjustment and fixed-effects predictions. Second-axis grouping for numeric-outcome
comparisons remains unsupported; use panels for grouping. Intercept-only models have valid statistics but no predictor
for an x-axis plot. Existing shared-renderer restrictions on grouping-variable
names also remain in effect. No shared prediction algorithms have been changed.

## Development and GUI verification

```sh
Rscript --vanilla dev/jamovi.R ui
Rscript --vanilla dev/jamovi.R test
Rscript --vanilla dev/jamovi.R fixture --force
Rscript --vanilla dev/jamovi/check-fixture.R
Rscript --vanilla dev/jamovi.R install --open
```

The generated smoke document includes a random-intercept example, an ML
intercept-free random-slope/quadratic example, and a correlated-slope model with a
fixed-effect interaction. Native option serialization/restoration, independent
lmer comparisons, installed UI-handler contracts and R6 rendering tests are
automated. Actual jamovi layout and user interaction require a GUI smoke pass.

### GUI synchronization regression coverage

Mixed Model synchronization starts at jamovi's `ready` / `view_updated` hook,
not `view_loaded`, which precedes saved-option initialization. During host option
loading, handlers respect `flags.updating`. Dependent changes are made in one
native option edit with a re-entry guard. This prevents the grouping-only state
from repeatedly assigning the grouping factor to x and then clearing it, and
prevents partially restored options from replacing a saved model with an
intercept-only specification.

Tests use the installed native option setters and View initialization events,
then pass the resulting server option batches into the actual R6 analysis and
compare fits/predictions with independent lmer calculations. Fresh GUI analyses,
GUI save/reopen, subsequent edits and required-variable removal were also checked
in jamovi 2.7.41. Updates require rebuilding/installing the module; reopen an
analysis/document to load the corrected UI bundle.


## Compact results presentation

The Cluster Variable selector retains the saved `cluster` option. Model formulas
and their estimation methods are displayed together; comparison mode displays
Model 1 and Model 2 on separate compact lines without repeating Model 1 above.
Ordinary comparison notes identify the common ML sample, preserve the original
estimation methods, and explain approximate reciprocal BIC Bayes factors when
available. Calibration, singularity, and convergence notes appear only as needed.

The analysis menu order is Flexplot, Linear Model, Generalized Linear Model,
Mixed Models. Internal analysis identifiers are unchanged. jamovi 2.7 supplies
the Flexplot ribbon icon through its own built-in stylesheet; this module cannot
replace it with `inst/icons/FlexplotLogo-sm.png` through a supported icon option.
The original logo assets and host icon are retained without modifying jamovi.


## Model 2 fixed-effects selector

When comparison is enabled, **Model 2 — Fixed Effects** remains visible, independently
of the interaction and polynomial switches. It is a native variable target supplied
from the selected predictors. A new comparison initializes it from Model 1's fixed
predictors. Removing a variable prunes its Model 2 terms and squared terms; adding
it back adds its main effect. Model 1 is unchanged. Random slopes remain separate
and do not select fixed effects. Interaction and squared-term builders use the
Model 2 selection. Existing saved explicit terms initialize the new control.

Comparison plots do not have a separate implementation-detail paragraph. The
comparison table briefly identifies ML statistics and use of original fits for plots.


## CAS with model comparison enabled

CAS shows Model 1 on the left and Model 2 on the right. Both use the same common
complete-case observations, but each removes its own estimated random contribution:
`adjusted outcome = outcome - (conditional prediction - fixed-effects prediction)`.
The original requested ML/REML fits are used, not the ML copies for comparison statistics.

Both curves use the same x coordinates, moderator categories, numeric bins and
within-bin means. Undisplayed covariates are observation-weighted marginalized
across the same full estimation population. Variables omitted from one model can
still be displayed without adding them to that model. Axes and group encodings
are shared; existing facets remain inside each model display.

**Show other model's predictions** is available only for CAS comparisons and is
off by default. Solid curves are the panel's own fixed-effects predictions;
dashed, translucent ghosts are the competing model's fixed-effects predictions
under the same conditions. Ghosts are not fitted to that panel's adjusted dots.
Less scatter after a more flexible random-effects adjustment does not by itself
indicate better model fit. No prediction confidence bands are added.

Custom axis, group and facet labels apply to both plots. Unsupported layouts or
failures in either plot produce a terminal error rather than a partial comparison
or indefinite rendering state. Existing CAS restrictions (one grouping factor,
untransformed numeric response, no offsets, required raw fixed-effect variables)
continue to apply. Single-model CAS and other comparison modes are unchanged.
