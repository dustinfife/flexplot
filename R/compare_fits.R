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
                        prediction_method="marginal", ...) {
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
  } else if (prediction_method == "marginal") {
    prediction = compare_fits_marginal(models, data, formula, re, pred.type, labels,
                                       single_model, report.se, num_points, ...)
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
