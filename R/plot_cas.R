.get_re_term = function(m) {
  if (!inherits(m, "lmerMod")) stop("CAS requires a fitted linear mixed-effects model (lmerMod).", call.=FALSE)
  fl = lme4::getME(m, "flist")
  if (length(fl) != 1L) {
    stop("CAS currently supports exactly one random-effects grouping factor.", call.=FALSE)
  }
  names(fl)[1]
}

cas_model_frame = function(m, outcome_name=NULL) {
  .get_re_term(m)
  response = stats::formula(m)[[2]]
  if (!is.symbol(response)) stop("CAS requires an untransformed response variable.", call.=FALSE)
  outcome = as.character(response)
  if (!is.null(outcome_name) && !identical(outcome_name, outcome)) {
    stop("The CAS outcome must match the fitted model response.", call.=FALSE)
  }
  d = stats::model.frame(m)
  if (!is.numeric(d[[outcome]])) stop("CAS requires a numeric response.", call.=FALSE)
  d
}

cas_prediction_rows = function(prediction, d) {
  rows = rownames(d)
  if (!is.null(names(prediction))) {
    if (anyDuplicated(names(prediction)) || !all(rows %in% names(prediction))) {
      stop("Named CAS predictions must identify all fitted model rows uniquely.", call.=FALSE)
    }
    return(as.numeric(prediction[rows]))
  }
  if (length(prediction) != nrow(d)) stop("CAS predictions must align with the fitted model frame.", call.=FALSE)
  as.numeric(prediction)
}

cas_model_predictions = function(m, d, adjust, pred_fixed=NULL, pred_cond=NULL, pred_int=NULL) {
  if (is.null(pred_fixed)) pred_fixed = stats::predict(m, re.form=NA)
  fixed = cas_prediction_rows(pred_fixed, d)
  if (identical(adjust, "none")) return(list(fixed=fixed, selected=fixed))
  if (identical(adjust, "all")) {
    if (is.null(pred_cond)) pred_cond = stats::predict(m, re.form=NULL)
    return(list(fixed=fixed, selected=cas_prediction_rows(pred_cond, d)))
  }
  if (identical(adjust, "intercept")) {
    intercepts = sum(vapply(lme4::getME(m, "cnms"), function(x) sum(x == "(Intercept)"), integer(1)))
    if (intercepts != 1L) stop("Intercept adjustment requires exactly one random-intercept term.", call.=FALSE)
    if (is.null(pred_int)) {
      grouping = reformulas::findbars(stats::formula(m))[[1]][[3]]
      re_formula = structure(call("~", call("(", call("|", 1, grouping))),
                             class="formula", .Environment=environment(stats::formula(m)))
      pred_int = stats::predict(m, re.form=re_formula)
    }
    return(list(fixed=fixed, selected=cas_prediction_rows(pred_int, d)))
  }
  selected = stats::predict(m, re.form=adjust)
  list(fixed=fixed, selected=cas_prediction_rows(selected, d))
}

cas_adjustment_mode = function(adjust) {
  if (is.null(adjust)) return("intercept")
  if (inherits(adjust, "formula")) return(adjust)
  if (length(adjust) == 1L && is.na(adjust)) return("none")
  match.arg(adjust, c("all", "intercept", "none"))
}

compute_cas_from_model = function(m, outcome_name=NULL, adjust_re=NULL) {
  d = cas_model_frame(m, outcome_name)
  outcome = as.character(stats::formula(m)[[2]])
  reserved = intersect(names(d), c(paste0(outcome, "_adj"), "pred_fixed", ".re_term"))
  if (length(reserved)) stop("CAS metadata would overwrite model-frame columns: ", paste(reserved, collapse=", "), call.=FALSE)
  predictions = cas_model_predictions(m, d, cas_adjustment_mode(adjust_re))
  d[[paste0(outcome, "_adj")]] = d[[outcome]] - (predictions$selected - predictions$fixed)
  d$pred_fixed = predictions$fixed
  d$.re_term = .get_re_term(m)
  d
}

cas_external_rows = function(d, fitted) {
  index = match(rownames(d), rownames(fitted))
  omitted = names(attr(fitted, "na.action"))
  if (any(is.na(index) & !rownames(d) %in% omitted)) {
    stop("Cannot align data with fitted model rows; preserve the original row names.", call.=FALSE)
  }
  keep = which(!is.na(index))
  shared = intersect(names(fitted), names(d))
  for (variable in shared) {
    observed = d[[variable]][keep]
    expected = fitted[[variable]][index[keep]]
    if (is.factor(observed) || is.factor(expected)) {
      observed = as.character(observed)
      expected = as.character(expected)
    }
    if (!isTRUE(all.equal(observed, expected, check.attributes=FALSE))) {
      stop("Data do not match fitted model rows; preserve row identities and values.", call.=FALSE)
    }
  }
  index
}

#' Create cluster adjusted dataset
#'
#' @param d Data containing the original fitted observations. Preserve row names
#' when reordering or subsetting. Omitted model rows receive NA when adjusting;
#' adjust="none" returns the original outcomes unchanged.
#' @param model A fitted linear mixed model, or NULL when predictions are supplied.
#' @param adjust The type of cluster adjustment: all, intercept, or none.
#' @param pred_fixed Fixed-effects predictions, optionally supplied in fitted-row
#' order or named by fitted row identifiers.
#' @param pred_cond Conditional predictions, optionally supplied.
#' @param pred_int Predictions including fixed effects and random intercepts only,
#' optionally supplied.
#' @param outcome_name The untransformed response name. Inferred from model when available.
#' @return A numeric vector aligned with d, with the selected random contribution removed.
#' @export
#' @examples
#' mod1 = lme4::lmer(y ~ x + (x | id), data=small_mixed)
#' new_small = cas_adjustment(small_mixed, model=mod1, adjust="all")
cas_adjustment = function(d, model=NULL,
                          adjust=c("all", "intercept", "none"),
                          pred_fixed=NULL, pred_cond=NULL, pred_int=NULL,
                          outcome_name=NULL) {
  adjust = match.arg(adjust)
  if (is.null(model)) {
    if (is.null(outcome_name) || !outcome_name %in% names(d)) {
      stop("Supply outcome_name when model is NULL.", call.=FALSE)
    }
    if (adjust == "none") return(d[[outcome_name]])
    selected = if (adjust == "all") pred_cond else pred_int
    if (is.null(pred_fixed) || is.null(selected)) stop("Supply fixed and selected predictions when model is NULL.", call.=FALSE)
    fixed = cas_prediction_rows(pred_fixed, d)
    selected = cas_prediction_rows(selected, d)
    return(d[[outcome_name]] - (selected - fixed))
  }
  fitted = cas_model_frame(model, outcome_name)
  outcome = as.character(stats::formula(model)[[2]])
  if (!outcome %in% names(d)) stop("The response must be present in d.", call.=FALSE)
  if (adjust == "none") return(d[[outcome]])
  index = cas_external_rows(d, fitted)
  predictions = cas_model_predictions(model, fitted, adjust, pred_fixed, pred_cond, pred_int)
  adjusted = fitted[[outcome]] - (predictions$selected - predictions$fixed)
  stats::setNames(adjusted[index], rownames(d))
}

# Convert the existing CAS fitting convention (all displayed predictors interact)
# using language objects, preserving literal identifiers and trusted expressions.
cas_fixed_rhs = function(expr) {
  if (!is.call(expr) || !as.character(expr[[1]]) %in% c("+", "|")) return(expr)
  parts = lapply(as.list(expr)[-1], cas_fixed_rhs)
  if (length(parts) == 1L) return(parts[[1]])
  Reduce(cas_interaction, parts)
}

cas_interaction = function(lhs, rhs) call("*", lhs, rhs)

cas_moderator_grid = function(moderators, data, original) {
  columns = flexplot_magnet_display_given(data, moderators)
  grid = expand.grid(lapply(data[columns], unique), KEEP.OUT.ATTRS=FALSE,
                     stringsAsFactors=FALSE)
  for (i in seq_along(moderators)) {
    variable = moderators[i]
    column = columns[i]
    if (variable != column) {
      # Match flexplot's reference-prediction convention: evaluate at the mean
      # original value in each display bin, rather than averaging predictions.
      representatives = tapply(data[[variable]], data[[column]], mean)
      grid[[variable]] = as.numeric(representatives[as.character(grid[[column]])])
    } else if (is.numeric(original[[variable]]) && is.factor(grid[[variable]])) {
      grid[[variable]] = as.numeric(as.character(grid[[variable]]))
    }
  }
  grid
}

cas_fixed_curve = function(m, d, xvar, moderators=character(), display_data=d) {
  fixed_variables = all.vars(reformulas::nobars(stats::formula(m)))[-1]
  if (!all(fixed_variables %in% names(d))) {
    stop("CAS marginal predictions require the original fixed-effect variables in the fitted model frame.", call.=FALSE)
  }
  if (!is.null(stats::model.offset(d))) stop("CAS does not currently support model offsets.", call.=FALSE)
  if (any(c(xvar, moderators) %in% c("prediction", "model"))) {
    stop("CAS prediction metadata would overwrite a displayed variable named prediction or model.", call.=FALSE)
  }
  grid = d[!duplicated(d[[xvar]]), xvar, drop=FALSE]
  grid = grid[order(grid[[xvar]]), , drop=FALSE]
  if (length(moderators)) {
    panels = cas_moderator_grid(moderators, display_data, d)
    grid = merge(grid, panels, by=NULL, sort=FALSE)
  }
  if (all(fixed_variables %in% xvar)) {
    grid$prediction = d$pred_fixed[match(grid[[xvar]], d[[xvar]])]
  } else if (all(fixed_variables %in% c(xvar, moderators))) {
    grid$prediction = unname(cas_predict_fixed(m, grid))
  } else {
    # Keep one observation-weighted nuisance distribution across all panels.
    grid$prediction = cas_marginal_predictions(m, d, grid, c(xvar, moderators))
  }
  grid$model = factor("fixed effects")
  grid
}

cas_predict_fixed = function(m, newdata) {
  prediction = tryCatch(stats::predict(m, newdata=newdata, re.form=NA), error=identity)
  if (inherits(prediction, "error")) {
    stop("CAS fixed-effects prediction failed for this model/predictor structure: ",
         conditionMessage(prediction), call.=FALSE)
  }
  if (!is.numeric(prediction) || length(prediction) != nrow(newdata) || any(!is.finite(prediction))) {
    stop("CAS requires one finite fixed-effects prediction per profile row; check the requested predictor domain.", call.=FALSE)
  }
  prediction
}

cas_marginal_predictions = function(m, d, grid, variables) {
  predictions = numeric(nrow(grid))
  # Bound temporary profile size while avoiding one predict() call per x/panel.
  batch_size = max(1L, min(64L, floor(50000/nrow(d))))
  for (start in seq.int(1L, nrow(grid), by=batch_size)) {
    index = seq.int(start, min(nrow(grid), start + batch_size - 1L))
    profile = d[rep(seq_len(nrow(d)), times=length(index)), , drop=FALSE]
    for (variable in variables) {
      profile[[variable]][] = rep(grid[[variable]][index], each=nrow(d))
    }
    values = cas_predict_fixed(m, profile)
    predictions[index] = colMeans(matrix(values, nrow=nrow(d)))
  }
  predictions
}

cas_prediction_layers = function(prediction, axis, data) {
  prediction = make_levels_same_for_prediction_dataset(data, prediction, axis)
  displayed = setdiff(intersect(names(prediction), names(data)), c("model", "prediction"))
  for (variable in displayed) {
    if (!is.factor(data[[variable]])) next
    prediction[[variable]] = factor(prediction[[variable]], levels=levels(data[[variable]]),
                                    ordered=is.ordered(data[[variable]]))
  }
  layers = flexplot_prediction_layer(prediction, axis, data)
  if (length(axis) > 1L) return(layers)
  # Preserve CAS's single-model red curve and its categorical marker ordering.
  layers = Filter(cas_is_prediction_layer, layers)
  for (i in seq_along(layers)) {
    layers[[i]]$mapping$colour = NULL
    layers[[i]]$mapping$linetype = NULL
    layers[[i]]$mapping$group = ggplot2::aes(group=1)$group
    layers[[i]]$aes_params$colour = "#bf0303"
    layers[[i]]$show.legend = FALSE
    if (inherits(layers[[i]]$geom, "GeomLine")) layers[[i]]$aes_params$linewidth = 1
    if (inherits(layers[[i]]$geom, "GeomPoint")) layers[[i]]$aes_params$size = 3
  }
  if (!is.numeric(data[[axis[1]]])) layers = rev(layers)
  layers
}

cas_is_prediction_layer = function(layer) inherits(layer, "Layer")

cas_prediction_ghost = function(prediction, data, formula, roles, options) {
  roles$given = roles$given[!is.na(roles$given) & nzchar(roles$given)]
  bins = if (is.null(options$bins)) 3 else options$bins
  predictors = all.vars(formula)[-1]
  break_me = flexplot_break_me(data, predictors, roles$given, roles$axis, bins)
  breaks = flexplot_create_breaks(break_me, options$breaks, data, options$labels, bins=bins)
  reference = create_ghost_reference(data=data, given=roles$given, axis=roles$axis,
                 ghost.reference=options$ghost.reference, bins=bins,
                 breaks=breaks, labels=options$labels)
  reference_data = flexplot_ghost_reference_data(prediction, roles$given, reference, roles$axis)
  coordinates = flexplot_ghost_prediction_data(reference_data, roles$axis, roles$given, data)
  flexplot_ghost_layer(coordinates, options$ghost.line)
}

#' Cluster adjusted scatterplot
#'
#' @param formula A flexplot-like formula using untransformed column names.
#' @param object A fitted linear mixed model with one grouping factor.
#' @param data Data used to fit a model when object is NULL. With a supplied model,
#' observations always come from its fitted model frame, irrespective of data order.
#' @param random Random-effects specification used when fitting a model. Character
#' input uses the restricted make.formula grammar; formulas/calls are trusted expressions.
#' @param adjust_re Random contributions to remove: "all", "intercept", "none",
#' or a trusted re.form formula. Defaults to "all".
#' @param smooth Add a descriptive smooth of adjusted observations, in addition to
#' the original model's fixed-effects prediction curve. Defaults to FALSE.
#' @param ... Other plotting arguments passed to flexplot.
#' @details Fixed predictions use the original model with random effects excluded.
#' Displayed moderators (second-axis groups and conditioning variables) are held
#' at their displayed categories or representative numeric values. Numeric display
#' bins use the mean original value within each bin, calculated from the full fitted
#' data, matching flexplot's reference-prediction convention. Custom bins and labels
#' are shared with the observations. Predictions are evaluated at these values,
#' rather than averaged over a bin's moderator distribution.
#' Undisplayed fixed predictors are marginalized over the same full fitted
#' model-frame covariate distribution for every curve, preserving interactions.
#' This is observation-weighted marginal standardization, not equal-cluster weighting.
#' Curves share the fitted observations' x values across displayed moderator combinations.
#' Ghosts copy a selected reference panel's model predictions to other panels,
#' excluding its own panel; they never fit a descriptive smoother. smooth=TRUE adds
#' an independent descriptive smooth of adjusted observations. A numeric
#' ghost.reference selects the bin containing that value and retains that bin's
#' representative-value curve. Prediction ghosts require conditioning and
#' currently do not support second-axis grouping. Models with
#' multiple grouping factors, offsets, transformed responses, or fixed predictors
#' unavailable in the model frame are not currently supported.
#' Model prediction methods must support the requested profiles and variable names;
#' prediction failures or non-finite results are reported rather than replaced.
#' Displayed variables named model or prediction are reserved for prediction metadata.
#' When fitting internally, displayed predictors (including conditioning variables)
#' enter with their interactions, preserving the existing CAS fitting convention.
#' @return A cluster-adjusted ggplot, or adjusted plotting data with return_data=TRUE.
#' @export
#' @examples
#' mod1 = lme4::lmer(y ~ x + (x | id), data=small_mixed)
#' cluster_adjusted_scatter(y ~ x, object=mod1)
cluster_adjusted_scatter = function(formula, object=NULL, data=NULL,
                                    random=NULL, adjust_re="all", smooth=F, ...) {
  if (!inherits(formula, "formula") || length(formula) != 3L || !is.symbol(formula[[2]])) {
    stop("CAS requires a formula with an untransformed response and predictor.", call.=FALSE)
  }
  roles = flexplot_axis_given(formula)
  xvar = roles$axis[1]
  outcome = as.character(formula[[2]])
  if (is.na(xvar) || identical(xvar, "1")) stop("Formula must specify an x predictor.", call.=FALSE)
  if (is.null(object)) {
    if (is.null(random) || is.null(data)) stop("Supply both random and data when object is NULL.", call.=FALSE)
    fit_formula = make.formula(formula[[2]], cas_fixed_rhs(formula[[3]]), random)
    environment(fit_formula) = environment(formula)
    object = lme4::lmer(fit_formula, data=data)
  }
  d = compute_cas_from_model(object, outcome_name=outcome, adjust_re=adjust_re)
  if (!all(all.vars(formula) %in% names(d))) stop("Plot variables must exist in the fitted model frame.", call.=FALSE)
  options = list(...)
  if (!is.null(options$ghost.line)) {
    if (length(roles$axis) > 1L) stop("CAS prediction ghosts do not yet support second-axis grouping.", call.=FALSE)
    if (all(is.na(roles$given) | !nzchar(roles$given))) stop("CAS prediction ghosts require a conditioning variable.", call.=FALSE)
  }
  adjusted_formula = formula
  adjusted_formula[[2]] = as.name(paste0(outcome, "_adj"))
  base_options = options
  base_options$ghost.line = NULL
  # CAS owns its model curves; avoid automatic binary-outcome magnet fitting.
  if (is.null(base_options$method)) base_options$method = "lm"
  p = do.call(flexplot, c(list(formula=adjusted_formula, data=d, suppress_smooth=TRUE), base_options))
  if (is.data.frame(p)) return(p)
  moderators = unique(c(roles$axis[-1], roles$given))
  moderators = moderators[!is.na(moderators) & nzchar(moderators)]
  curve = cas_fixed_curve(object, d, xvar, moderators, p$data)
  curve = make_levels_same_for_prediction_dataset(p$data, curve, roles$axis)
  p = p + cas_prediction_layers(curve, roles$axis, p$data)
  if (!is.null(options$ghost.line)) {
    p = p + cas_prediction_ghost(curve, p$data, adjusted_formula, roles, options)
  }
  if (isTRUE(smooth)) p = p + ggplot2::geom_smooth()
  p
}

add_random = function(form, random_terms) {
  to_piece = function(x) {
    if (inherits(x, "formula")) sub("^~", "", paste(deparse(x), collapse="")) else as.character(x)
  }
  rhs = paste(vapply(as.list(random_terms), to_piece, character(1)), collapse = " + ")
  update(form, as.formula(paste("~ . +", rhs), env = environment(form)))
}
