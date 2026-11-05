#' Compare fitted models using observed or reference predictions
#'
#' Compare model predictions with a \code{\link{flexplot}} formula. The default
#' changed from the historical reference-profile approach to observed-data
#' conditional averaging. \code{compare.fits()} is a supported alias with the
#' same arguments and behavior.
#'
#' @param formula A flexplot formula specifying the displayed predictors and
#'   conditioning variables, using original variable names. Transformations and
#'   interactions may appear in the fitted model formula, not in the plotting formula.
#' @param data Original observations, with their original row names.
#' @param model1 A fitted model.
#' @param model2 An optional second fitted model.
#' @param return.preds Return the prediction data instead of a plot.
#' @param report.se Return prediction standard errors for lm/glm models. For
#'   observed averages these use coefficient covariance and the delta method;
#'   individual standard errors are not averaged. Compatible \code{prediction.fit},
#'   \code{prediction.lwr} and \code{prediction.upr} fields contain the estimate
#'   and approximate 95\% confidence limits. Unsupported classes give an error.
#' @param re Include random-effects predictions for mixed models. A single model
#'   returns both fixed and random predictions when TRUE; two models return their
#'   random-effects predictions.
#' @param pred.type Prediction scale, normally \code{"response"}; GLMs also
#'   support \code{"link"}. Predictions are averaged on the requested scale.
#' @param num_points Retained for positional/API compatibility. The historical
#'   grid and experimental observed-bin algorithms do not use this argument.
#' @param clusters Number of mixed-model clusters to display, using the existing
#'   stratified cluster-sampling convention.
#' @param prediction_method Either \code{"observed"} (default) or \code{"reference"}.
#' @param ... Arguments passed to flexplot, including display bins, breaks, labels,
#'   sampling and other plotting controls.
#'
#' @details
#' \code{"observed"} predicts on actual, common eligible estimation observations,
#' retaining their original predictor combinations. It averages these predictions
#' within the displayed categories, grouping variables and conditioning bins.
#' For numeric x with at least ten distinct values, the existing experimental
#' algorithm uses up to nine quantile bins and plots each average at its bin
#' midpoint. Otherwise it averages at each observed x value. Empty groups are not
#' generated. Numeric grouping/conditioning uses the same display-break helpers
#' as flexplot, without decoding numeric values from bin-label text.
#'
#' \code{"reference"} uses the historical prediction-grid approach: omitted
#' numeric predictors are held at their mean and omitted categorical predictors
#' at their mode. Use this method to reproduce historical reference profiles.
#'
#' Both methods use complete observations for the plotted and required model
#' variables, intersected with the models' identifiable estimation rows. Row names
#' and stored model-frame values must match; otherwise alignment fails explicitly.
#' Mixed-model clusters are sampled after establishing this common sample.
#'
#' Neither method is automatically a causal effect. Observed conditional averaging
#' is not marginal standardization, partial dependence or accumulated local effects
#' (ALE). Correlated omitted predictors can make the two methods differ substantially.
#'
#' Ordinal polr models retain the historical predicted-class score convention and
#' require numeric category labels. Multicategory probability matrices and models
#' without identifiable estimation observations are unsupported. No neural-network
#' prediction adapter is supplied by this package. Binary tree/forest predictions
#' use probabilities for the second response level; regression trees/forests use
#' their numeric fitted responses.
#'
#' @return A ggplot, or a data frame with displayed coordinates, \code{prediction}
#'   and \code{model}; standard errors and confidence limits are added when requested.
#' @author Dustin Fife
#' @export
#' @examples
#' m1 = lm(y ~ x + z, data=small)
#' m2 = lm(y ~ x, data=small)
#' compare_fits(y ~ x, data=small, model1=m1, model2=m2)
#' compare_fits(y ~ x, data=small, model1=m1, prediction_method="reference")
#' compare_fits(y ~ a, data=small, model1=lm(y ~ a + z, data=small),
#'              return.preds=TRUE)
compare_fits = function(formula, data, model1, model2=NULL,
                        return.preds=FALSE, report.se=FALSE, re=FALSE,
                        pred.type="response", num_points=50, clusters=3,
                        prediction_method="observed", ...) {
  if (length(prediction_method) != 1 || is.na(prediction_method) ||
      !prediction_method %in% c("observed", "reference")) {
    stop('prediction_method must be "observed" or "reference".', call.=FALSE)
  }
  data = as.data.frame(data)
  if (!compare_fits_plain_expression(formula)) {
    stop("Use original variable names in the compare_fits() plotting formula; ",
         "put transformations and interactions in the fitted model formula.", call.=FALSE)
  }
  single_model = is.null(model2)
  models = if (single_model) list(model1) else list(model1, model2)
  terms = lapply(models, compare_fits_model_terms)
  outcome = terms[[1]]$response
  if (!single_model && !identical(outcome, terms[[2]]$response)) {
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
  labels = paste0(paste(deparse(substitute(model1)), collapse=""), " (", class(model1)[1], ")")
  if (!single_model) {
    label2 = paste0(paste(deparse(substitute(model2)), collapse=""), " (", class(model2)[1], ")")
    labels = make.unique(c(labels, label2))
  }

  plot_data = data
  if (inherits(model1, "polr")) {
    scores = suppressWarnings(as.numeric(as.character(plot_data[[outcome]])))
    if (!anyNA(scores)) plot_data[[outcome]] = scores
  }
  flexplot_reverse_categorical(formula, plot_data, allow_reverse=FALSE)

  if (prediction_method == "reference") {
    prediction = compare_fits_reference(models, data, formula, re, pred.type, labels,
                                         single_model, report.se, ...)
  } else {
    frames = vector("list", length(models))
    for (i in seq_along(models)) {
      frames[[i]] = compare_fits_observed(models[[i]], data, formula, re, pred.type,
                                          labels[i], single_model, report.se, ...)
    }
    prediction = do.call(rbind, frames)
  }
  attr(prediction, "observation_groups") = NULL
  rownames(prediction) = NULL
  if (return.preds) return(prediction)

  axis = flexplot_axis_given(formula)$axis
  if (identical(axis[1], "1")) {
    return(flexplot(formula, data=plot_data, suppress_smooth=TRUE, se=FALSE, ...))
  }

  if (is.numeric(plot_data[[outcome]]) &&
      any(prediction$prediction < min(plot_data[[outcome]]) |
          prediction$prediction > max(plot_data[[outcome]]))) {
    warning("Some of the model's predicted values are beyond the range of the original y-values. ",
            "I'm truncating the y-axis to preserve the original scale.")
  }
  attr(prediction, "flexplot_comparison") = TRUE
  p = flexplot(formula, data=plot_data, prediction=prediction,
                suppress_smooth=TRUE, se=FALSE, ...) + return_lims_geom(outcome, plot_data, model1)
  if (single_model && length(axis) < 2) p = p + ggplot2::theme(legend.position="none")
  p
}

#' @rdname compare_fits
#' @export
compare.fits = compare_fits

return_lims_geom = function(outcome, data, model1) {
  if (!inherits(model1, c("lm", "glm", "merMod")) || !is.numeric(data[[outcome]])) {
    return(ggplot2::theme_bw())
  }
  ggplot2::coord_cartesian(ylim=range(data[[outcome]], na.rm=TRUE))
}
