#' Model-specific estimates and effect-size summaries
#'
#' Dispatch to an estimates method for the fitted model's class. Outputs differ
#' by model type; this is not a common coefficient extractor or a general
#' interface to marginal effects.
#' @param object A fitted model. Specialized methods are registered for lm, glm,
#'   lmerMod, glmerMod, party's RandomForest, and pscl's zeroinfl classes.
#' @param mc Request nested model comparisons for the lm method. Other methods
#'   currently ignore this argument. Method-specific defaults may differ.
#' @details
#' \describe{
#'   \item{lm}{Model R-squared, sequential ANOVA contributions, numeric coefficients
#'     and standardized coefficients, conditional factor predictions and pairwise
#'     differences, residual SD, and optional term-deletion model comparisons.}
#'   \item{glm}{Link-dependent coefficient summaries and response prediction
#'     differences. Binomial logit models also provide conditional IRT estimates
#'     and categorical marginal predicted probabilities. Poisson log-link
#'     coefficients describe multiplicative expected-count changes.}
#'   \item{lmerMod}{Fixed effects, original-model random-effect variances,
#'     variance reductions relative to a fitted random-intercept baseline, and
#'     baseline ICC/design-effect calculations.}
#'   \item{glmerMod}{Fixed effects, random-effect variances and baseline
#'     ICC/design-effect calculations. No generalized-model R-squared is currently
#'     supplied; see the method's restrictions on ICC interpretation.}
#'   \item{RandomForest}{party::cforest out-of-bag predictive performance and
#'     permutation importance. This is not the randomForest package's class.}
#'   \item{zeroinfl}{Limited summaries for aligned count and logit-zero components
#'     of pscl zero-inflated models; see the method's restrictions.}
#' }
#' Classes without a specialized method use estimates.default, which calls
#' summary(object) and replaces its class. A successful fallback does not imply
#' that flexplot implements model-specific effect sizes for that class.
#'
#' The methods do not report the same estimands: conditional coefficients,
#' reference-profile predictions, standardized coefficients, marginal probabilities
#' and out-of-bag performance answer different questions. Consult the method page
#' before comparing results across classes. In particular, baseline mixed models
#' and lm term-deletion comparisons involve refitting; binomial GLM marginal
#' probabilities use the original fitted model without refitting.
#' @return A model-dependent object. The lm, glm and mixed-model methods use
#'   custom print classes; intercept-only lm models instead return a data frame.
#'   Zero-inflated models may return a coefficient table or a list.
#' @seealso \code{\link{estimates.lm}}, \code{\link{estimates.glm}}, \code{\link{estimates.lmerMod}},
#'   \code{\link{estimates.glmerMod}}, \code{\link{estimates.RandomForest}}, \code{\link{estimates.zeroinfl}},
#'   \code{\link{estimates.default}}
#' @examples
#' model = lm(y ~ x + a, data=small)
#' result = estimates(model, mc=FALSE)
#' result$numbers.summary
#' result$factor.summary
#' @export
estimates = function(object, mc=TRUE){
  UseMethod("estimates")
}

#' Summary fallback for models without an estimates method
#'
#' Call summary(object) and replace its class with estimates. This fallback does
#' not calculate standardized coefficients, conditional effects, marginal
#' predictions, confidence intervals, or model comparisons of its own.
#' @param object An object with a suitable summary method.
#' @param mc Ignored by this method.
#' @return The result of summary(object), with its class replaced by estimates.
#' @details
#' The mathematical definitions, units, treatment of categorical predictors and
#' interactions, and any uncertainty measures are entirely those of the object's
#' summary method. The class replacement does not preserve its original print
#' method: print.estimates expects particular list components and may omit output
#' or fail for incompatible summaries, including atomic summary objects. Thus this
#' is a limited fallback, not a guarantee of support for every fitted-model class.
#' Inspect the returned components rather than assuming an APA-style table.
#' No dedicated estimates methods currently exist for rpart, ranger or the
#' randomForest package's randomForest class.
#' @examples
#' if (requireNamespace("rpart", quietly=TRUE)) {
#'   model = rpart::rpart(y ~ x, data=small)
#'   result = estimates(model)
#'   names(result)
#' }
#' @seealso \code{\link{estimates}}
#' @export
estimates.default = function(object, mc=TRUE){
	out = summary(object)
	class(out) = "estimates"
	out
}

#' Estimates for linear mixed-effects models
#'
#' Return fixed effects and random-effect components from a fitted lmerMod model,
#' together with summaries relative to a newly fitted intercept-only baseline.
#' @param object An lme4 lmerMod model, including compatible subclasses using
#'   this S3 method.
#' @param mc Currently ignored. Baseline fitting and comparison occur regardless
#'   of its value.
#' @return A list of class lmer_estimates with fixed, r.squared, rand and icc
#'   components, printed as fixed effects, random effects, ICC/design effect and
#'   R Squared. These are not the GLM IRT or marginal-probability tables.
#' @details
#' \describe{
#'   \item{fixed}{The original model's lme4::fixef coefficients. Numeric slopes
#'     have response units per predictor unit; factor coefficients follow the
#'     fitted contrasts. Interactions retain their conditional coefficient meaning
#'     at zero/reference moderator settings. No coefficient standardization,
#'     marginal averaging or coefficient confidence intervals are added.}
#'   \item{rand}{The original model's lme4::VarCorr object. Random-intercept
#'     variances have squared response units, random-slope variances have squared
#'     response-per-predictor units, and correlations are dimensionless. Printing
#'     uses lme4's variance-component presentation, typically SDs/correlations.}
#'   \item{r.squared}{Proportional reductions in matched variance components
#'     compared with the fitted baseline: (baseline variance - full variance) /
#'     baseline variance. Components are labeled by the matched random-effect
#'     term and Residual. These are variance reductions, not marginal/conditional
#'     R-squared or variance of fixed predictions divided by total variance.
#'     Values can be negative or nonfinite; no interval is supplied.}
#'   \item{icc}{The baseline model's first variance component divided by the sum
#'     of its VarCorr vcov entries, and design.effect = 1 + ICC*(m-1). For a
#'     single random-intercept Gaussian baseline, m is the observation count
#'     divided by the cluster count, ICC = tau-squared/(tau-squared+sigma-squared),
#'     and both reported quantities are dimensionless. These describe the baseline,
#'     not the adjusted full-model ICC.}
#' }
#' The baseline is formed by update(object, response ~ 1 + (1 | group)), using
#' the existing grouping-variable extractor. Random slopes and fixed predictors
#' are removed from that baseline, but the original fitted model is not replaced.
#' The model-comparison helper may refit REML models with ML for comparison.
#'
#' This workflow is intended for a single, simply named grouping variable. Nested
#' or crossed grouping structures, complex grouping expressions and multiple
#' random terms are not robustly handled by the formula-text extractor. A baseline
#' with missing-data differences may also invoke model-comparison row handling.
#' Only variance components present in the baseline are matched; do not interpret
#' this output as an R-squared for every random slope. Zero baseline variance,
#' singular fits and convergence problems can make summaries unstable or undefined.
#' @examples
#' model = lme4::lmer(y ~ x + (1 | id), data=small_mixed)
#' result = estimates(model)
#' result$fixed
#' result$rand
#' result$r.squared
#' result$icc
#' @seealso \code{\link{estimates}}, \code{\link{estimates.glmerMod}}, \code{\link{icc}}, \code{\link{rsq_change}}
#' @export
estimates.lmerMod = function(object, mc=TRUE){
  fixed = lme4::fixef(object)
  rand = lme4::VarCorr(object)

  
  # fit a baseline model
  baseline = fit_baseline_model(object)
  icc_stats = unlist(icc(baseline))
  
  # compute rsq 
  rsq = model.comparison(object, baseline)$r_squared_change
  
  # return the objects
  ret = list( fixed = fixed,
              r.squared=rsq,
              rand = rand, 
              icc = icc_stats
             )
  attr(ret, "class") = "lmer_estimates"
  return(ret)
}



#' Estimates for generalized mixed-effects models
#'
#' Report fixed effects and random-effect variances from the original glmerMod
#' model and calculate ICC/design-effect summaries from a fitted baseline.
#' @param object An lme4 glmerMod model.
#' @param mc Currently ignored. The baseline is fitted even when FALSE.
#' @return A list with fixed, r.squared, rand and icc, currently assigned class
#'   lmer_estimates (not glmer_estimates). The r.squared component is normally NULL:
#'   model.comparison does not currently return variance-reduction R-squared for
#'   glmerMod models.
#' @details
#' fixed contains lme4::fixef(object) on the fitted link scale. For a binomial
#' logit model these are log-odds coefficients; for a Poisson log model they are
#' log expected-count coefficients. The method does not exponentiate, standardize,
#' or attach confidence intervals to them. Numeric coefficients are per predictor
#' unit on the link scale, factor coefficients follow fitted contrasts, and
#' interactions are conditional coefficient changes at zero/reference settings.
#' No marginal probabilities or GLM IRT estimates are calculated by this method.
#'
#' rand is the original VarCorr object, with random-effect variances and
#' correlations on the model's latent linear-predictor scale (and any dispersion
#' scale reported by lme4). It is not a response-scale variance decomposition.
#'
#' The baseline replaces the fixed predictors and random slopes with
#' response ~ 1 + (1 | group), retaining the original family/link through update.
#' The icc helper divides its first reported variance component by the sum of its
#' VarCorr vcov entries, then calculates 1 + ICC*(m-1), with m obtained from its
#' residual count and baseline coefficient-based cluster count.
#'
#' \strong{Important limitation:} this calculation does not add distribution-
#' specific observation variance. In particular it does not add pi-squared/3 for
#' a binomial logit model, and is not a conventional latent-scale binomial ICC.
#' With only one reported component the ratio can be 1, or NaN if it is zero;
#' the resulting design effect should not be interpreted as a validated GLMM
#' sampling-design correction. The same simple-grouping and baseline-refitting
#' restrictions as estimates.lmerMod apply. Family support is conditional on the
#' existing baseline/model-comparison helpers; this method is not a universal
#' generalized mixed-model effect-size implementation. Matrix-valued binomial
#' responses such as cbind(successes, failures), and transformed responses, can
#' fail baseline construction because its response extractor retains only a
#' variable name rather than the complete response expression.
#' @examples
#' # A reproducible Bernoulli response derived from the package fixture:
#' d = small_mixed
#' d$binary = as.integer(d$y > median(d$y))
#' model = lme4::glmer(binary ~ x + (1 | id), data=d, family=binomial)
#' result = estimates(model)
#' result$fixed
#' result$rand
#' is.null(result$r.squared)
#' # Read the ICC limitation above before interpreting result$icc.
#' @seealso \code{\link{estimates}}, \code{\link{estimates.lmerMod}}, \code{\link{estimates.glm}}, \code{\link{icc}}
#' @export
estimates.glmerMod = function(object, mc=FALSE){
  fixed = lme4::fixef(object)
  rand = lme4::VarCorr(object)
  
  
  # fit a baseline model
  baseline = fit_baseline_model(object)
  icc_stats = unlist(icc(baseline))
  
  # compute rsq 
  rsq = model.comparison(object, baseline)$r_squared_change
  
  # return the objects
  ret = list( fixed = fixed,
              r.squared=rsq,
              rand = rand, 
              icc = icc_stats
  )
  attr(ret, "class") = "lmer_estimates"
  return(ret)
}



#' Estimates for ordinary linear regression
#'
#' Summarize an lm fit with coefficients, standardized coefficients, conditional
#' factor predictions and differences, and model-fit measures. The calculations
#' described below are the current implementation, including its approximations.
#' @param object An lm model, normally fitted with an intercept.
#' @param mc If TRUE and there are at least two non-interaction terms, refit
#'   term-deletion models and return R-squared changes and BIC-based comparisons.
#' @return Normally a list of class estimates containing r.squared, semi.p,
#'   correlation, factor.summary, difference.matrix, factors, numbers.summary,
#'   numbers, sigma and model.comparison. Absent numeric or factor tables are NA.
#'   An intercept-only fit instead returns a data frame with Mean, Lower, Upper
#'   and d; see details.
#' @details
#' \describe{
#'   \item{r.squared}{summary(object)$r.squared and approximate lower/upper limits,
#'     rounded to four decimals. With R2 the fitted R-squared and n the model-frame
#'     row count, the standard-error approximation is
#'     sqrt(4*R2*(1-R2)^2*(n-2)^2 / ((n^2-1)*(n+3))). Limits are R2 plus/minus
#'     qt(.975, n-2) times that value. They are not clipped to [0,1] and are not
#'     exact model-specific confidence bounds for arbitrary multiple regression.}
#'   \item{semi.p}{Despite its historical name and printed label, this is the
#'     sequential ANOVA sum of squares for each term divided by the sum of term
#'     and residual sums of squares. It is dimensionless and order-dependent,
#'     not a general unique/semi-partial contribution obtained by dropping each
#'     term from the full model. The interaction-related message does not change
#'     this calculation.}
#'   \item{correlation}{The ordinary Pearson response/predictor correlation only
#'     when there is one numeric non-interaction term and no factors; otherwise
#'     NA. It is dimensionless and not a partial correlation.}
#'   \item{numbers.summary}{The intercept and numeric main-term coefficients,
#'     with lower/upper = coefficient plus/minus 1.96*SE. Slopes have response
#'     units per predictor unit; the intercept has response units. Standardized
#'     coefficients equal coefficient*SD(model-matrix column)/SD(response), with
#'     SE rescaled by the same factor and analogous 1.96-SE bounds. These are
#'     dimensionless conditional coefficients, not average marginal effects.
#'     The standardized intercept and its bounds are set to zero.}
#'   \item{factor.summary}{For each categorical main predictor and level,
#'     model-based response predictions and approximate intervals from
#'     compare_fits(prediction_method="reference", report.se=TRUE). Other numeric
#'     predictors are held at their means and other categorical predictors at
#'     their modes under the existing reference-profile machinery. The interval
#'     calculation uses coefficient covariance, not a raw group SD. These are
#'     conditional reference predictions in response units, not observed group
#'     means or full-population marginal means.}
#'   \item{difference.matrix}{All within-factor level pairs, labeled with their
#'     signed prediction difference in response units. For k levels, residual SD
#'     sigma, df = n - length(coef(object)) and cell counts n_i,n_j, the implemented
#'     half-width is qtukey(.95,k,df)*sigma*sqrt(1/n_i+1/n_j). This is a
#'     Tukey-style construction using raw factor counts; it is not a covariance-
#'     based contrast interval for arbitrary adjusted predictions, and omits the
#'     sqrt(2) divisor of the usual Tukey-Kramer width. cohens.d is the signed
#'     difference divided by the full model's residual SD, not a separately pooled
#'     pairwise raw-group SD.}
#'   \item{sigma}{summary(object)$sigma, the residual SD in response units.}
#'   \item{model.comparison}{NULL if mc=FALSE or fewer than two non-interaction
#'     terms; otherwise a table with all.terms, rsq and bayes.factor. The Full
#'     Model row holds the full R-squared. Other rsq rows contain full minus
#'     term-deleted R-squared, not the deleted model's R-squared. Bayes factors
#'     approximate evidence for retaining the term via the BIC difference; they
#'     are not exact integrated likelihoods. Models are refitted, so marginality
#'     and interaction interpretation require care.}
#' }
#' The factors and numbers components record which main-term predictors enter
#' each table. Factor/character predictors use modeled level predictions instead
#' of printing their contrast coefficients; numeric tables omit explicit
#' interaction coefficients. The original model's interactions still affect its
#' predictions, and main numeric coefficients retain the fitted contrast/zero-
#' moderator interpretation. Do not interpret them as slopes at moderator means.
#'
#' For an intercept-only fit, Mean and its bounds come from compare_fits with
#' report.se=TRUE and d = intercept/residual SD. This d is relative to zero,
#' not a between-group effect. This branch returns a data frame, not the usual
#' estimates list. The mean is the fitted intercept (weighted if the lm fit is
#' weighted), rather than necessarily an unweighted raw arithmetic mean.
#'
#' \strong{Limitations.} Unestimable numeric coefficients cause an error. There
#' is no dedicated robust/survey-weighted inference: weighted lm fits can pass
#' through parts of this machinery, but raw counts, ordinary SDs and ANOVA-based
#' calculations should not be treated as design-based estimators. Transformed
#' terms, no-intercept fits, rank deficiency and complex factor structures are
#' not uniformly supported across helpers. Standardized interaction coefficients
#' are not printed in numbers.summary. The implementation also assigns the model
#' frame to a global dataset variable for integration compatibility.
#' @examples
#' model = lm(y ~ x + z + a, data=small)
#' result = estimates(model, mc=FALSE)
#' result$numbers.summary
#' result$factor.summary
#' result$difference.matrix
#' result$semi.p
#' estimates(lm(y ~ 1, data=small))
#' # Optional term-deletion comparisons involve additional fitted models:
#' estimates(lm(y ~ x + z, data=small), mc=TRUE)$model.comparison
#' @seealso \code{\link{estimates}}, \code{\link{standardized.beta}}, \code{\link{compare_fits}}
#' @export
estimates.lm = function(object, mc=TRUE){

	#### generate list of coefficients
	terms = attr(terms(object), "term.labels")
	
	variables = as.character(attr(terms(object), "variables")); variables = variables[-1]
	outcome = variables[1]
	predictors = variables[-1]

	# for intercept only models, return the mean
	if (length(predictors) == 0 ) {
	  f = as.formula(paste0(outcome, "~1"))
	  est = compare_fits(formula = f, data=object$model, model1=object, model2=NULL, return.preds=T, report.se=T)[1,]
	  return = est[c("prediction.fit", "prediction.lwr", "prediction.upr")]
	  names(return) = c("Mean", "Lower", "Upper")
	  return$d = coef(object)/summary(object)$sigma
	  return(return)
	}

  #### look for interaction terms
	terms = remove_interaction_terms(object)
	
	#### get dataset
	d = object$model
	
  #### identify factors
	variable_types = which_terms_are_factors_or_numbers(d, terms)
	factors = variable_types$factors
	numbers = variable_types$numbers
	
	# compute semi-partial
  semi.p = compute_semi_partial(object)
  
  # get summaries for factors
  factor_summaries = populate_estimates_factors(object, factors)
  coef.matrix = factor_summaries$coef.matrix
  difference.matrix = factor_summaries$difference.matrix
  
  # get summaries for numeric
  coef.matrix.numb = populate_estimates_numeric(object, numbers)
	
  # compute r squared and its CI
  r.squared = compute_r_squared(object)
	
	#### report correlation
  correlation = compute_correlation(object)
  
  # model comparison (I think this is only for JASP)
  dataset<<-object$model
  mod.comps = return_model_comparisons(object, terms, mc)

	ret = list(r.squared=r.squared, semi.p=semi.p, correlation = correlation, factor.summary = coef.matrix, 
	           difference.matrix=difference.matrix, factors=factors, numbers.summary=coef.matrix.numb, numbers=numbers, 
	           sigma=summary(object)$sigma,
	           model.comparison = mod.comps)
	attr(ret, "class") = "estimates"
	return(ret)
}


#' Out-of-bag estimates for party conditional inference forests
#'
#' Summarize predictive performance and permutation importance for the S4
#' RandomForest class returned by party::cforest. There are no regression
#' coefficients, slopes, thresholds, or level-specific marginal estimates.
#' @param object A party::cforest RandomForest object, not an object of class
#'   randomForest from the randomForest package.
#' @param mc Currently ignored.
#' @return A list of class rf_estimates with oob, rsq and importance, and a
#'   numeric attribute identifying regression rather than classification output.
#' @details
#' For a nonnumeric response (normally a factor), oob is the fraction of all
#' response observations whose out-of-bag predicted class equals the observed
#' class, rounded to three decimals. It is correct classifications divided by
#' the total count, not correct divided by incorrect classifications. rsq is NA.
#' importance is party::varimp(object), sorted decreasingly and rounded to four
#' decimals, retaining the upstream classification permutation-importance scale.
#'
#' For a numeric response, oob contains the default five quantiles (minimum,
#' quartiles and maximum) of absolute out-of-bag prediction error, rounded to three
#' decimals and expressed in response units. rsq is the squared Pearson correlation
#' between out-of-bag predictions and observations. This is not
#' 1 - sum((y-prediction)^2)/sum((y-mean(y))^2), and does not by itself measure
#' calibration. importance sorts the permutation importance, truncates negative
#' values to zero, and reports their square roots rounded to three decimals.
#' It is a transformed importance score, not the RMSE of the forest or an exact
#' root-MSE distance between original and shuffled predictions despite the current
#' print label. No importance uncertainty intervals are calculated.
#'
#' Predictors, including factors and interactions learned by the trees, are
#' handled by the supplied forest. There is no additional factor coding,
#' reference-level conditioning, coefficient standardization, or marginal
#' averaging in this method. The implementation selects regression/classification
#' by whether the stored response is numeric: encode a classification response
#' as a factor rather than relying on numeric 0/1 coding. Missing out-of-bag
#' predictions or a constant response can produce missing performance values.
#' Forest fitting and permutation importance can depend on the random seed.
#' @examples
#' set.seed(72)
#' regression = party::cforest(y ~ x + a, data=small,
#'   controls=party::cforest_unbiased(ntree=50, mtry=2))
#' estimates(regression)
#' set.seed(72)
#' classification = party::cforest(b ~ x + a, data=small,
#'   controls=party::cforest_unbiased(ntree=50, mtry=2))
#' estimates(classification)
#' @seealso \code{\link{estimates}}, \code{\link[party]{cforest}}, \code{\link[party]{varimp}}
#' @export
estimates.RandomForest = function(object, mc=TRUE) {
  
  y = unlist(attr(object, "data")@get("response"))
  ### compute OOB
  
  oob = predict(object, OOB=T, type="response")
  if (!is.numeric(y)){
    numeric=F
    oob = round(length(which(oob==y))/length(y), digits=3)
    rsq = NA
  } else {
    ### quantile of differences
    numeric = T
    rsq = cor(oob, y)[1,1]^2
    oob = round(quantile(abs(oob-y)), digits=3)
  }
  
  #### compute variable importance
  importance = party::varimp(object)
  if (!numeric){
    importance = round(sort(importance, decreasing=T), digits=4)
  } else {
    vals = sort(importance, decreasing=T)
    vals[vals<0] = 0
    importance = round(sqrt(vals), digits=3)
    sds = 
    importance_sd = importance/sd(vals)
  }
  
  estimates = list(oob=oob, rsq = rsq, importance=importance)
  attr(estimates, "class") = "rf_estimates"
  attr(estimates, "numeric") = numeric
  return(estimates)
  
}



#' Interpretable estimates for generalized linear models
#'
#' Report coefficients and effect sizes from the original fitted model. For
#' binomial logit models, printing retains Raw Estimates, IRT Estimates, and
#' Standardized Estimates, followed by Marginal Predicted Probabilities when
#' categorical predictors are present. No models are refitted.
#'
#' @param object A fitted glm object.
#' @param mc Should model comparisons be performed? Currently not used.
#' @return The existing coefficient-level glm_estimates object. For binomial logit
#'   models, additional attributes are irt (predictor-level conditional estimates),
#'   conditioning (a one-row reference profile), and marginal_probabilities
#'   (Variable, Level, Probability; NULL if no categorical predictors exist).
#'   The continuous attribute identifies predictors eligible for the printed IRT
#'   table according to their fitted expressions, rather than raw storage types.
#'   Coefficient rows, their names, and numeric missing IRT values are retained.
#' @details
#' \strong{Other GLM families and links.} Outside the binomial-logit branch,
#' column selection depends primarily on the fitted link, not just the family.
#' A log link (including Poisson, log-Gamma and compatible glm subclasses such as
#' MASS::glm.nb) returns raw.coefficients, multiplicative.coef = exp(beta), and
#' std.mult.coef = exp(beta*SD(model-matrix column)). These multiply the conditional
#' expected response per original unit or one coded-column SD, respectively.
#' For Poisson models the response is an expected count; with a modeled exposure
#' offset the same multiplier applies to the rate at fixed exposure. Factor
#' coefficients and interaction coefficients retain their fitted contrast and
#' conditional link-scale interpretations, rather than generic mean differences.
#'
#' An inverse link (for example the default Gamma link) instead reports
#' raw.coefficients, inverse.coef = 1/beta, and std.mult.coef =
#' 1/(beta*SD(model-matrix column)). These reciprocals are \emph{not} multiplicative
#' response effects: because mu = 1/eta, the response change depends on the whole
#' linear predictor. The standardized-intercept reciprocal can be infinite because
#' the intercept column has SD zero. Do not interpret the legacy column name as
#' a constant mean ratio.
#'
#' Other links, including Gaussian identity and binomial probit/cloglog, use
#' summary(object)$coefficients (coefficient, standard error, test statistic and
#' p-value), plus the existing prediction-difference column. Names may be sanitized
#' by data.frame conversion. Their units are those of the fitted linear predictor;
#' link inversion is needed for a response-scale interpretation. The extended IRT
#' and categorical marginal-probability attributes described below are currently
#' provided only for binomial logit fits, not all binomial links. Quasibinomial
#' logit fits still use the older coefficient-alignment path and are not guaranteed
#' to support interactions or multilevel factors.
#'
#' In these older paths, numeric main-term prediction differences use absolute
#' response predictions at mean plus/minus one SD (rounded to two decimals), with
#' other numeric terms at means and factors at their first observed values.
#' Factor rows attempt signed response differences relative to a reference level
#' identified from coefficient names; a single unordered factor also places its
#' reference prediction in the intercept row. These are response units (counts
#' for Poisson, positive-response units for Gamma), not always probabilities.
#' This legacy name-based calculation is not robust to arbitrary contrasts,
#' transformed terms or complex factor interactions. It should not be confused
#' with binomial logit marginal standardization. Other numeric coefficient columns
#' are generally rounded to three decimals when numeric predictors are present.
#'
#' The following definitions apply to binomial logistic regression. The modeled
#' success is the outcome recognized by the fitted binomial model: for a two-level
#' factor it is the second level. A proportion or two-column binomial response
#' retains the original model's success coding.
#'
#' \strong{Raw Estimates.} If eta = X beta is the logit of success probability,
#' raw.coefficients contains beta. A continuous main-effect coefficient measures
#' log-odds per predictor unit, conditional on the remaining covariates. In an
#' interaction model it is the effect at zero/reference moderator settings under
#' the fitted parameterization, not necessarily at the IRT conditioning settings.
#' OR = exp(beta) is the corresponding odds multiplier; inverse.OR = exp(-beta)
#' is its reciprocal. Treatment-coded factor coefficients compare a level with
#' its reference; sum, ordered-polynomial and other contrasts describe their
#' fitted contrast, not necessarily a pairwise level comparison. Interaction
#' coefficients describe changes in these effects.
#'
#' \strong{IRT Estimates.} Only continuous predictors are printed here; factors
#' remain in Raw and Standardized Estimates. Other continuous predictors are
#' held at their estimation-row means and categorical predictors at fitted
#' reference levels. Nondefault treatment references are respected. When the
#' fitted contrasts have no reference level (for example sum or polynomial
#' contrasts), the first fitted category is used. Inspect the conditioning
#' attribute for the actual settings. These are conditional estimates at specified
#' covariate values, not average marginal effects.
#'
#' The instantaneous_slope is the derivative of predicted success probability
#' with respect to the original predictor at target probability p. Its units are
#' probability per predictor unit. The intercept_threshold is the predictor value
#' where that same conditional curve reaches p. estimates uses p = 0.5;
#' \code{\link{logistic_coefficients}} exposes location to request another p, changing both
#' calculations consistently. For a conditional affine logit a + b*x, these are
#' b*p*(1-p) and (qlogis(p)-a)/b. Interactions retain all fitted terms, evaluated
#' at the documented mean/reference settings, so results may change at other
#' moderator values. Log and square-root focal transformations use the chain
#' rule and original predictor units. General nonlinear transformations, including
#' quadratic terms, return numeric NA when uniqueness cannot be established.
#' Unreachable targets and nonfinite derivatives also yield NA. A zero conditional
#' slope is reported as zero with an NA threshold because no unique crossing exists.
#'
#' A threshold outside the observed range is an extrapolated crossing, not evidence
#' by itself of a poor predictor. Baseline risk may remain below or above 0.5 over
#' the entire observed range even when a predictor has a substantial effect.
#' Always interpret the slope, observed range, uncertainty and model adequacy
#' alongside the threshold; this table does not provide uncertainty intervals.
#'
#' \strong{Categorical expressions in formulas.} Direct factor(x), as.factor(x),
#' ordered(x) and as.ordered(x) constructions (including base-qualified calls)
#' are categorical even when the original x column is numeric. Types, levels
#' and contrast names are obtained from the fitted terms and model frame.
#' Predictions substitute a valid original input corresponding to each fitted
#' level and reevaluate the original expression; they do not use the mean of a
#' numeric column that the model treats as categorical. Marginal summaries use
#' the original column name in Variable and fitted labels in Level. Consequently,
#' conditioning can contain a numeric input value for factor(x); the continuous
#' attribute, not the storage type of that input, controls IRT eligibility.
#'
#' Reordered or relabeled levels are supported when each fitted level maps to
#' one original input and the expression retains the same labels when evaluated
#' on prediction profiles. Otherwise an informative error recommends creating an
#' explicit factor column and refitting. Composite categorical transformations
#' such as factor(x^2) or cut(x), multiple fitted representations of the same
#' source column (for example x + factor(x)), and profile-dependent labels are
#' not inferred or inverted automatically. Stable relabeling can be specified
#' with explicit levels and labels in factor(), or by creating a factor column.
#'
#' \strong{Standardized Estimates.} Standardization multiplies each coefficient
#' by the standard deviation of its fitted model-matrix column. The displayed OR
#' is exp(beta*SD(column)) and inverse.OR is its reciprocal. For continuous main
#' effects this is the odds multiplier for one SD; factor contrasts and interaction
#' columns use their own coded-column SD and are not generic between-level effects.
#' The intercept has column SD zero and standardized OR one by convention.
#'
#' Prediction Difference (+/- 1 SD) retains the absolute difference between
#' response-scale predictions at a continuous main predictor's mean plus and minus
#' one SD, with other predictors at the same conditional settings. This spans two
#' SD and is not an odds ratio. Supported treatment-coded factor rows instead
#' display signed probability differences from the reference level; when there is
#' one unordered factor, the intercept row also displays its reference probability.
#' Contrasts without a single reference comparison, interaction coefficient rows,
#' and transformed coefficient rows have NA prediction differences rather than
#' inventing a discrete comparison or attaching a predictor result positionally.
#'
#' \strong{Marginal Predicted Probabilities.} For each categorical predictor and
#' each fitted level, set that predictor to the level for every eligible estimation
#' observation, retain all other observed covariates, predict with the original
#' model on the response scale, and take the arithmetic mean. Equivalently,
#' probability(level) = mean_i inverse_logit(eta_i with that level substituted).
#' This accounts for interactions and contrasts without interpreting coefficient
#' names. It is observation-weighted standardization over estimation rows, not a
#' prediction at mean covariates, equal-cluster weighting, a pairwise difference,
#' or automatically a causal effect. Fitting weights do not become averaging
#' weights. Missing rows excluded during fitting do not reenter this population.
#'
#' Existing output for other GLM families/links is unchanged. For transformed
#' predictors, original estimation-row predictor values must be recoverable from
#' the stored frame or original data call. Coefficient-level values retain existing
#' display rounding; the summary attributes retain full numerical precision.
#' @examples
#' model = glm(injury ~ safety + attention + gender,
#'             data=tablesaw.injury, family=binomial)
#' result = estimates(model)
#' result
#' # Raw OR: conditional odds multiplier for one original safety unit.
#' exp(coef(model)["safety"])
#' # IRT: probability slope and 50 percent crossing with attention at its mean
#' # and gender at its fitted reference category. Check those actual values:
#' attr(result, "conditioning")
#' # The 70 percent threshold and tangent slope describe a different point:
#' logistic_coefficients(model, location=.7)
#' # Standardized OR uses one SD; prediction difference spans mean +/- one SD.
#' # Gender probabilities average over observed safety and attention instead:
#' attr(result, "marginal_probabilities")
#' interacting = glm(injury ~ safety * gender + attention,
#'                   data=tablesaw.injury, family=binomial)
#' estimates(interacting)
#' # The safety IRT values are conditional on reference gender; the marginal
#' # gender probabilities retain each observation's actual safety and attention.
#' # Log-link counts: exp(beta) multiplies the conditional expected count.
#' counts = glm(y_pois ~ x + a, data=small, family=poisson)
#' estimates(counts)
#' # A log-Gamma fit uses the same multiplicative interpretation for its mean.
#' estimates(glm(y_gam ~ x, data=small, family=Gamma(link="log")))
#' # Inverse-link reciprocals are not constant response mean ratios.
#' estimates(glm(y_gam ~ x, data=small, family=Gamma))
#' # Other links retain their ordinary coefficient-summary table.
#' estimates(glm(y ~ x, data=small, family=gaussian))
#' estimates(glm(y_bin ~ x, data=small, family=binomial(link="probit")))
#' # Numeric source columns can be explicitly modeled as categorical:
#' categorical = glm(am ~ wt + factor(cyl), data=mtcars, family=binomial)
#' estimates(categorical)
#' @seealso \code{\link{estimates}}, \code{\link{logistic_coefficients}}, \code{\link{estimates.lm}}
#' @export
estimates.glm = function(object, mc=FALSE){
  if (identical(stats::family(object)$family, "binomial") &&
      identical(stats::family(object)$link, "logit")) {
    result = logistic_estimates_table(object)
    attr(result, "class") = "glm_estimates"
    return(result)
  }

	#### generate list of coefficients
	terms = remove_interaction_terms(object)
	
	#### get dataset
	d = extract_data_from_fitted_object(object) %>% data.frame
	factor_or_number = which_terms_are_factors_or_numbers(d, terms)
	numbers = factor_or_number$numbers
	factors = factor_or_number$factors
  preds = output_glm_predictions(object, terms)
	
	#### output coefficients
  coef.matrix = compute_factor_differences(object=object)
  

  attr(coef.matrix, "class") = "glm_estimates"
	return(coef.matrix)
}



#' Limited estimates for pscl zero-inflated count models
#'
#' Summarize count coefficients and logit-zero coefficients for a pscl zeroinfl
#' model. The specialized method is registered, but its component alignment and
#' prediction helpers restrict which model specifications are reliable.
#' @param object A pscl zeroinfl model, with a logit zero component and matching
#'   additive terms in the count and zero components.
#' @param mc Currently ignored.
#' @return With numeric predictors only, a coefficient data frame. When the
#'   factor-prediction path succeeds, a list containing coef.matrix and means.
#'   No dedicated custom estimates print class is assigned.
#' @details
#' For a zero-inflated model, the conditional response mean is (1-pi)*mu, where
#' mu is the count-component mean and pi is the structural-zero probability.
#' The table column named for object$dist contains raw count coefficients,
#' and multiplicative.coefficient is their exponential. For a log count component,
#' exp(beta) multiplies mu per predictor unit or fitted factor contrast; it is
#' not necessarily a multiplier of the overall mean when pi also changes.
#' logit contains the zero-component log-odds coefficients and OR their exponential,
#' describing structural-zero odds rather than success odds or the count mean.
#' Numeric main-predictor Prediction Difference (+/- 1 SD) is the absolute
#' response-mean prediction difference at its mean plus/minus one SD, rounded to
#' two decimals, using the original fitted model. Other numeric predictors are
#' held at means and categorical predictors at the first observed values by the
#' anchor-prediction helper. Tables otherwise retain existing three-decimal
#' rounding. No coefficient standardization or uncertainty intervals are added.
#'
#' The optional means table attempts reference-profile predictions for factor
#' combinations, with undisplayed numeric predictors at means and categorical
#' predictors at modes under generate_predictors. Its prediction column is the
#' overall expected response count. Despite its name, its logit column comes from
#' predict(type="zero") and is the structural-zero probability, not log odds.
#' It is not a table of marginal probabilities or raw group proportions.
#'
#' \strong{Limitations.} The implementation pairs count/zero coefficients
#' positionally and hard-codes a logit column. Different term sets, unequal
#' component dimensions, alternative zero links and component-specific contrasts
#' are not generally supported and can fail or misalign results. Interactions
#' and transformed terms are not robustly handled by the term-name helpers.
#' Factor-only prediction configurations can fail (including a single multilevel
#' factor); the existence of a registered method does not establish full support
#' for these cases. Use the numeric, matching-component example below, inspect
#' the underlying model coefficients/predictions, and avoid interpreting unsupported
#' configurations. These restrictions are documented without changing calculations.
#' @examples
#' if (requireNamespace("pscl", quietly=TRUE)) {
#'   model = pscl::zeroinfl(y_pois ~ x | x, data=small)
#'   result = estimates(model)
#'   result
#' }
#' @seealso \code{\link{estimates}}, \code{\link[pscl]{zeroinfl}}, \code{\link{anchor.predictions}}
#' @export
estimates.zeroinfl = function(object, mc=FALSE){
  coef.matrix = compute_factor_differences(object=object)
  
  #### generate list of coefficients
  terms = remove_interaction_terms(object)
  
  #### get dataset
  d = extract_data_from_fitted_object(object)
  
  factor_or_number = which_terms_are_factors_or_numbers(d, terms)
  numbers = factor_or_number$numbers
  factors = factor_or_number$factors
  
  if (length(factors)==0) return(coef.matrix)
  factors_equation = make.formula(names(d)[1], factors)
  preds_factors = generate_predictors(d, factors_equation, object) %>%
    .[!duplicated(.),] %>%
    `rownames<-`( NULL ) 
  preds_factors$prediction = predict(object, preds_factors)
  preds_factors$logit      = predict(object, preds_factors, type="zero")
  
  return(list(coef.matrix=coef.matrix, means = preds_factors))
}

