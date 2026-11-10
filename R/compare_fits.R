#' Compare fitted models using marginal, observed, or reference predictions
#'
#' Compare predictions from one or two supplied models using a
#' \code{\link{flexplot}} formula. Marginal standardization is the default;
#' \code{compare.fits()} is an exact backward-compatible alias.
#'
#' @param formula A flexplot formula specifying displayed predictors and
#'   conditioning variables using original variable names. Put transformations
#'   and interactions in the fitted model formulas, not the plotting formula.
#' @param data Original observations, retaining their original row names.
#' @param model1 A fitted model with an untransformed response matching the
#'   plotting response.
#' @param model2 An optional second fitted model with the same response.
#' @param return.preds Return the prediction data instead of a plot.
#' @param report.se Include prediction standard errors for supported lm/glm
#'   models. Standard errors for averaged predictions use coefficient covariance
#'   and the delta method, rather than averaging individual standard errors.
#'   The returned \code{prediction.fit}, \code{prediction.lwr}, and
#'   \code{prediction.upr} columns contain the estimate and approximate 95\%
#'   confidence limits. Unsupported model classes give an error.
#' @param re Include random-effects predictions for mixed models. With one model,
#'   \code{TRUE} returns fixed and random predictions; with two models it returns
#'   their random-effects predictions.
#' @param pred.type Prediction scale, normally \code{"response"}. GLMs also
#'   support \code{"link"}. Predictions are averaged on the requested scale;
#'   transformed model responses are unsupported and are not inverse-transformed.
#' @param num_points Number of equally spaced numeric x coordinates for marginal
#'   predictions when x has at least ten distinct values. Must be an integer
#'   greater than or equal to two. Other prediction methods do not use it.
#' @param clusters Number of mixed-model clusters to display, using stratified
#'   cluster sampling.
#' @param prediction_method One of \code{"marginal"} (default),
#'   \code{"observed"}, or \code{"reference"}; see Details.
#' @param observation.type Observed-data display: \code{"raw"} (default),
#'   \code{"binned"}, or \code{"none"}. Binned observations require binomial
#'   GLMs, a numeric x axis, \code{prediction_method="observed"}, and
#'   \code{pred.type="response"}. \code{raw.data=FALSE} hides observations in
#'   every mode. This display option does not affect \code{return.preds=TRUE}.
#' @param ... Arguments passed to plotting and prediction-grid helpers, including
#'   display \code{bins}, \code{breaks}, \code{labels}, and observation sampling.
#'
#' @details
#' \code{"marginal"} uses observation-weighted marginal standardization. At each
#' displayed coordinate, all displayed predictors and moderators are set to that
#' coordinate in a common reference dataset. Predictions are then averaged over
#' the undisplayed covariates, retaining their joint observed distribution and
#' the fitted model's transformations and interactions. The reference population
#' contains all common eligible estimation observations before display-cluster selection;
#' the same rows are used at every coordinate and for both models. Marginal
#' averaging does not subsample observations or consume random numbers.
#' This is observation weighting, not
#' equal-cluster weighting, and is not automatically a causal effect.
#'
#' Displayed categorical moderators use their observed factor levels. Numeric
#' moderators use flexplot's existing bin-based representative values (bin means
#' rounded to three decimal places). Numeric x uses \code{num_points} coordinates
#' when it has at least ten distinct values; otherwise it uses observed values.
#' Intercept-only displays return a single prediction coordinate.
#'
#' With \code{observation.type="binned"}, hollow circles show unweighted observed
#' success proportions and colored points show the models' mean predicted probabilities.
#' All three summaries use the exact same eligible observations in each x bin,
#' panel, and supported second-axis group. Predictions use actual covariate
#' values; bin midpoints are only plotting coordinates. Numeric x with at least
#' ten distinct values retains the existing nine quantile intervals (fewer when
#' boundaries coincide); otherwise observed x values are used. Empty cells are
#' omitted, while singleton and all-zero/all-one cells retain their exact observed
#' proportions on a 0--1 probability axis. Sampling affects raw observations
#' only, not these summaries.
#' Factor outcomes are coded globally using the fitted models' success event;
#' models with different success coding cannot share this display. No log-odds
#' correction, additional model fitting, or change to prediction calculations is
#' applied. The binned option does not support categorical x/magnet plots;
#' their existing raw display is unchanged.
#'
#' \code{"observed"} predicts at actual eligible observations and averages within
#' displayed categories, groups, and conditioning bins. Numeric x with at least
#' ten distinct values uses the existing quantile-bin summaries, positioned at
#' bin midpoints. This conditional averaging differs from marginal standardization
#' because the distribution of undisplayed covariates can vary between groups.
#'
#' \code{"reference"} uses the historical prediction-grid approach: undisplayed
#' numeric predictors are held at their mean and categorical predictors at their
#' mode, rather than averaging predictions over observations.
#'
#' All methods use complete observations for plotting and required model variables,
#' intersected with identifiable estimation rows from both models. Original row
#' names and stored model-frame values must agree. Mixed-model clusters are
#' sampled after establishing this common sample, using the existing cluster
#' selection policy. That separate step can consume random numbers and select
#' different clusters under different seeds. For marginal predictions, this
#' selection controls the plotted grid and observations, while averaging uses
#' the full common eligible population. Each displayed cluster identity is set
#' throughout that population before prediction, using the original fitted model.
#' The plot's \code{sample}
#' argument controls displayed observations, not the prediction reference population.
#'
#' Predictions must provide one numeric response per observation. Binary
#' tree/forest models use probabilities for the second response level. Ordinal
#' \code{polr} models retain predicted-class scores and require numeric category
#' labels. Multicategory probability matrices and models without identifiable
#' estimation observations are unsupported.
#'
#' @return A ggplot, or a data frame containing displayed coordinates,
#'   \code{prediction}, and \code{model}. Standard errors and confidence limits
#'   are included when requested.
#' @author Dustin Fife
#' @export compare_fits
#' @examples
#' m1 = lm(y ~ x + z, data=small)
#' m2 = lm(y ~ x, data=small)
#' compare_fits(y ~ x, data=small, model1=m1, model2=m2)
#' compare_fits(y ~ x, data=small, model1=m1, prediction_method="reference")
#' compare_fits(y ~ a, data=small, model1=lm(y ~ a + z, data=small),
#'              return.preds=TRUE)
#' full = glm(y_bin ~ x + a + z, data=small, family=binomial())
#' reduced = glm(y_bin ~ x, data=small, family=binomial())
#' compare_fits(y_bin ~ x | a, small, full, reduced,
#'              prediction_method="observed", observation.type="binned")
compare_fits = function(formula, data, model1, model2=NULL,
                        return.preds=FALSE, report.se=FALSE, re=FALSE,
                        pred.type="response", num_points=50, clusters=3,
                        prediction_method="marginal",
                        observation.type=c("raw", "binned", "none"), ...) {
  observation.type = match.arg(observation.type)
  if (length(prediction_method) != 1 || is.na(prediction_method) ||
      !prediction_method %in% c("marginal", "observed", "reference")) {
    stop('prediction_method must be "marginal", "observed", or "reference".', call.=FALSE)
  }
  data = as.data.frame(data)
  if (!compare_fits_plain_expression(formula)) {
    stop("Use original variable names in the compare_fits() plotting formula; ",
         "put transformations and interactions in the fitted model formula.", call.=FALSE)
  }
  single_model = is.null(model2)
  models = if (single_model) list(model1) else list(model1, model2)
  responses = character(length(models))
  for (i in seq_along(models)) {
    responses[i] = compare_fits_model_response(models[[i]], paste0("model", i))
  }
  terms = lapply(models, compare_fits_model_terms)
  outcome = responses[1]
  if (!single_model && !identical(outcome, responses[2])) {
    stop("The models must have the same outcome variable.", call.=FALSE)
  }
  plotted = all.vars(formula)
  if (!identical(plotted[1], outcome)) {
    stop("The plotting formula and fitted models must have the same outcome.", call.=FALSE)
  }
  available = unique(unlist(lapply(terms, `[[`, "predictors")))
  compare_fits_errors(data, plotted[1], plotted[-1], available)
  variables = unique(c(plotted, available, outcome))
  data = compare_fits_sample(data, models, variables)
  reference_data = data
  if (inherits(model1, "merMod")) {
    if (length(lme4::getME(model1, "flist")) != 1 ||
        (!single_model && (!inherits(model2, "merMod") ||
                           !identical(names(lme4::getME(model1, "flist")), names(lme4::getME(model2, "flist")))))) {
      stop("Mixed-model comparisons require one shared grouping variable.", call.=FALSE)
    }
    data = prepare_data_for_compare_fits(data, model1, formula, clusters=clusters)
  } else if (!single_model && inherits(model2, "merMod")) {
    stop("Both models must be mixed models when comparing random effects.", call.=FALSE)
  }
  data = droplevels(data)
  model_labels = paste0(paste(deparse(substitute(model1)), collapse=""), " (", class(model1)[1], ")")
  if (!single_model) {
    label2 = paste0(paste(deparse(substitute(model2)), collapse=""), " (", class(model2)[1], ")")
    model_labels = make.unique(c(model_labels, label2))
  }
  
  plot_data = data
  if (inherits(model1, "polr")) {
    scores = suppressWarnings(as.numeric(as.character(plot_data[[outcome]])))
    if (!anyNA(scores)) plot_data[[outcome]] = scores
  }
  flexplot_reverse_categorical(formula, plot_data, allow_reverse=FALSE)
  
  if (prediction_method == "reference") {
    prediction = compare_fits_reference(models, data, formula, re, pred.type, model_labels,
                                        single_model, report.se, ...)
  } else if (prediction_method == "marginal") {
    prediction = compare_fits_marginal(models, data, formula, re, pred.type, model_labels,
                                       single_model, report.se, num_points,
                                       reference_data=droplevels(reference_data), ...)
  } else {
    frames = vector("list", length(models))
    for (i in seq_along(models)) {
      frames[[i]] = compare_fits_observed(models[[i]], data, formula, re, pred.type,
                                          model_labels[i], single_model, report.se, ...)
    }
    prediction = do.call(rbind, frames)
  }
  attr(prediction, "observation_groups") = NULL
  rownames(prediction) = NULL
  if (return.preds) return(prediction)
  
  plot_options = list(...)
  show_observations = !identical(plot_options$raw.data, FALSE) && observation.type != "none"
  if (observation.type != "raw") plot_options$raw.data = FALSE
  axis = flexplot_axis_given(formula)$axis
  if (identical(axis[1], "1")) {
    if (observation.type == "binned") stop("Binned observations require a numeric x predictor.", call.=FALSE)
    p = do.call(flexplot, c(list(formula=formula, data=plot_data,
                                suppress_smooth=TRUE, se=FALSE), plot_options))
    if (!show_observations) {
      for (i in seq_along(p$layers)) p$layers[[i]]$aes_params$alpha = 0
    }
    return(p)
  }

  observed = NULL
  if (observation.type == "binned") {
    if (prediction_method != "observed" || pred.type != "response") {
      stop("observation.type='binned' requires prediction_method='observed' and pred.type='response'.", call.=FALSE)
    }
    if (!is.numeric(data[[axis[1]]])) stop("Binned observations require a numeric x predictor.", call.=FALSE)
    matched = compare_fits_binned_observations(models, data, frames, outcome)
    observed = matched$summary
    plot_data[[outcome]] = matched$response
  }
  
  limit_data = if (inherits(model1, "merMod")) reference_data else plot_data
  logistic_link = identical(pred.type, "link") && inherits(model1, "glm") &&
    model1$family$family %in% c("binomial", "quasibinomial")
  if (!logistic_link && observation.type != "binned" && is.numeric(limit_data[[outcome]]) &&
      any(prediction$prediction < min(limit_data[[outcome]]) |
          prediction$prediction > max(limit_data[[outcome]]))) {
    warning("Some of the model's predicted values are beyond the range of the original y-values. ",
            "I'm truncating the y-axis to preserve the original scale.")
  }
  attr(prediction, "flexplot_comparison") = TRUE
  attr(prediction, "flexplot_comparison_call") = match.call()
  p = do.call(flexplot, c(list(formula=formula, data=plot_data, prediction=prediction,
                              suppress_smooth=TRUE, se=FALSE), plot_options))
  if (observation.type == "binned") {
    if (!is.numeric(p$data[[axis[1]]])) {
      stop("Binned observations require an x predictor rendered on a numeric axis.", call.=FALSE)
    }
    p = compare_fits_binned_layers(p, observed, axis[1], show_observations)
  }
  if (observation.type == "binned") {
    p = p + ggplot2::coord_cartesian(ylim=c(0, 1))
  } else if (!logistic_link) {
    p = p + return_lims_geom(outcome, limit_data, model1)
  }
  if (single_model && length(axis) < 2) p = p + ggplot2::theme(legend.position="none")
  p
}

#' @rdname compare_fits
#' @export compare.fits
compare.fits = compare_fits

return_lims_geom = function(outcome, data, model1) {
  if (!inherits(model1, c("lm", "glm", "merMod")) || !is.numeric(data[[outcome]])) {
    return(ggplot2::theme_bw())
  }
  ggplot2::coord_cartesian(ylim=range(data[[outcome]], na.rm=TRUE))
}
