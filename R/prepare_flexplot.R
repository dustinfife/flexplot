#' Prepare a flexplot visualization and prediction coordinates
#'
#' Reuse flexplot's formula selection, variable classification, display binning,
#' and reference-profile conventions without fitting a model or rendering a plot.
#' @param data A data frame containing the outcome and predictor columns.
#' @param formula Optional visualization formula, such as y ~ x + group | panel.
#'   Use literal column names and + / | layout operators. Model transformations
#'   belong in model, not the visualization formula.
#' @param model Optional fitted model supported by get_terms(), or a two-sided
#'   model formula. A formula is sufficient; no fitted or artificial model is needed.
#' @param outcome Outcome column name when it cannot be inferred from model or formula.
#' @param predictors Predictor column names when model is not supplied. Defaults
#'   to the visualization variables, or all non-outcome columns if formula is NULL.
#' @param bins,breaks,labels Numeric display-binning arguments passed to the
#'   existing flexplot preparation helpers.
#' @return A list of class flexplot_preparation, with contract version 1:
#'   * formula: the selected visualization formula.
#'   * model_formula: the supplied/recovered model formula (NULL if unavailable).
#'   * data: complete eligible original observations; rows indexes the input data.
#'   * plot_data: observations processed using flexplot's display conventions.
#'   * grid: unique original-coordinate prediction profiles, including display-bin columns.
#'   * reference: a one-row data frame of settings for omitted predictors.
#'   * variables: outcome, full predictors, and numeric/categorical predictor names.
#'   * roles and display_roles: x, optional group, and panel column names before/after binning.
#'   * binning: bins, breaks, labels, and names of variables binned for display.
#'   * factor_levels and contrasts: factor metadata, with fitted-model specifications
#'     used where available. Contrast specifications can be names or matrices.
#' @details Automatic selection delegates to make_flexplot_formula(). At most four
#' predictors are displayed by that heuristic; others use existing reference values:
#' numeric means and categorical modes (ties follow factor order). This is a
#' reference-profile grid, not marginal standardization. Displayed numeric groups
#' and panels use the existing within-bin mean representatives, rounded to three
#' decimals. The grid retains observed primary-x coordinates rather than creating
#' an independently spaced sequence. Eligibility requires complete observations
#' across the outcome, full predictor set, and visualization variables.
#'
#' The outcome must be numeric or binary. Association, paired-difference, and
#' transformed visualization-formula preparation are outside this contract.
#' Fitted binomial models must use the same outcome coding as data. Variable
#' classifications describe original coordinates; plot_data records display
#' conversions, including low-cardinality numeric axes. No mixed-model cluster
#' selection or population/conditional random-effect policy is inferred.
#' Model formulas may contain transformations of predictors; downstream prediction
#' code remains responsible for evaluating those transformations and using the
#' model's contrasts. No coefficients, predictions, or posterior/prior distributions
#' are calculated here. Every required original predictor must be available in data.
#'
#' To render external draws, repeat grid once per relationship, add a finite numeric
#' prediction column and a nonmissing draw ID, and call flexplot() with formula,
#' data, and the same binning settings from the preparation. Pass prediction.draw
#' as the draw-column name. The optional model column identifies one model, not
#' individual draws. Each draw must have at most one value at each displayed
#' x/group/panel coordinate. Draw IDs never enter a color, symbol, or linetype scale.
#' Numeric x uses lines; categorical x uses points and lines with shared group
#' offsets. Facets and group colors retain flexplot's normal conventions.
#'
#' Draw mode uses supplied values directly, suppresses internally fitted trends,
#' and never averages values within bins. It supports one model with any number
#' of draws. Ordinary fitted-model comparisons retain their separate existing
#' behavior. Draw-specific ghosts and paired-difference overlays are unsupported.
#' Binary magnet draws must be supplied as response-scale probabilities; for a
#' factor outcome the success event is its second observed level, in factor order.
#' @md
#' @export
#' @examples
#' prepared = prepare_flexplot(small, model=y ~ x + a, formula=y ~ x | a)
#' predictions = do.call(rbind, lapply(seq_len(5), function(draw) {
#'   grid = prepared$grid
#'   grid$draw = draw
#'   grid$prediction = (draw - 3) / 5 + (draw / 5) * grid$x
#'   grid
#' }))
#' flexplot(prepared$formula, prepared$data, prediction=predictions,
#'          prediction.draw="draw", prediction.alpha=.25,
#'          breaks=prepared$binning$breaks, labels=prepared$binning$labels,
#'          bins=prepared$binning$bins)
prepare_flexplot = function(data, formula=NULL, model=NULL, outcome=NULL, predictors=NULL,
                             bins=3, breaks=NULL, labels=NULL) {
  data = as.data.frame(data)
  if (!is.null(model)) {
    if (!is.null(predictors)) stop("Specify model or predictors, not both.",call.=FALSE)
    terms = get_terms(model)
    if (!is.null(outcome) && !identical(outcome,terms$response)) stop("outcome must match the model response.",call.=FALSE)
    outcome = terms$response
    predictors = terms$predictors
  }
  if (is.null(outcome) && !is.null(formula)) outcome = all.vars(formula)[1]
  if (!is.character(outcome) || length(outcome)!=1L || is.na(outcome) || !outcome %in% names(data))
    stop("Specify an outcome column, a model formula/object, or a visualization formula.",call.=FALSE)
  if (is.null(predictors)) predictors = if (is.null(formula)) setdiff(names(data),outcome) else all.vars(formula)[-1]
  predictors = unique(predictors)
  if (!is.character(predictors) || anyNA(predictors) || any(!predictors %in% names(data)))
    stop("All predictor names must identify columns in data.",call.=FALSE)
  if (any(predictors %in% c("model","prediction"))) stop("Predictor names model and prediction are reserved for prediction metadata.",call.=FALSE)
  if (is.null(model)) model = make.formula(as.name(outcome),lapply(predictors,as.name))
  model_formula = tryCatch(stats::formula(model),error=function(error) NULL)
  if (!is.null(model_formula) && length(model_formula)==3L && !is.symbol(model_formula[[2]]))
    stop("Preparation requires an untransformed outcome variable.",call.=FALSE)
  if (!inherits(model,"formula")) compare_fits_validate_binomial_coding(list(model),data,outcome)
  if (is.null(formula)) formula = make_flexplot_formula(predictors,outcome,data)
  if (!inherits(formula,"formula") || length(formula)!=3L || !is.symbol(formula[[2]]) ||
      !compare_fits_plain_expression(formula) || !identical(all.vars(formula)[1],outcome))
    stop("Use an untransformed visualization formula with the same outcome and literal predictor names.",call.=FALSE)
  plotted = all.vars(formula)[-1]
  if (any(!plotted %in% predictors)) stop("Visualization variables must belong to the specified predictor set.",call.=FALSE)
  required = unique(c(outcome,predictors,plotted))
  rows = which(stats::complete.cases(data[required]))
  data = flexplot_delete_na(data,required)
  if (!nrow(data)) stop("No complete observations are available for plot preparation.",call.=FALSE)
  if (!is.numeric(data[[outcome]]) && !is_binary(data[[outcome]]))
    stop("Prediction preparation currently supports numeric or binary outcomes.",call.=FALSE)
  roles = flexplot_axis_given(formula)
  axis = roles$axis; given = roles$given
  variables = all.vars(formula,unique=FALSE)
  use_magnet = !identical(axis[1],"1") && is_binary(data[[outcome]]) && check.non.number(data[[axis[1]]])
  binned = flexplot_break_me(data,plotted,given,axis,bins)
  constructor = if (use_magnet) flexplot_magnet_breaks else flexplot_create_breaks
  breaks = constructor(binned,breaks,data,labels,bins)
  method = identify_method(data,outcome,axis,NULL,use_magnet=use_magnet)
  display = flexplot_modify_data(data=data,formula=formula,variables=variables,outcome=outcome,
    axis=axis,given=given,method=method,break.me=binned,breaks=breaks,bins=bins,labels=labels,use_magnet=use_magnet)
  if (use_magnet) {
    if (is.factor(data[[axis[1]]])) display[[axis[1]]] = data[[axis[1]]]
    display = prepare_magnet_data(display,outcome,flexplot_object_axis(display,axis),flexplot_magnet_display_given(display,given))
  }
  grid = unique(generate_predictors(data,formula,model,bins=bins,breaks=breaks,labels=labels))
  grid = bin_variables(grid,bins,labels,binned,breaks)
  if (length(binned) && anyNA(grid[paste0(binned,"_binned")]))
    stop("Existing bin representatives fall outside the display breaks; use less precise/custom bin boundaries.",call.=FALSE)
  for (variable in paste0(binned,"_binned")) {
    if (!setequal(as.character(unique(grid[[variable]])),as.character(unique(display[[variable]]))))
      stop("Existing rounded bin representatives cannot cover all display bins; rescale the predictor or supply custom breaks.",call.=FALSE)
  }
  omitted = setdiff(predictors,plotted)
  reference = grid[1,omitted,drop=FALSE]
  types = variable_types(predictors,data,return.names=TRUE)
  if (is.null(types)) types = list(numbers=character(),characters=character())
  factor_names = required[vapply(data[required],check.non.number,logical(1))]
  factor_levels = lapply(data[factor_names],flexplot_sampling_levels)
  contrasts = lapply(data[factor_names],flexplot_preparation_contrasts)
  if (inherits(model,"merMod")) contrasts = attr(lme4::getME(model,"X"),"contrasts") else if (is.list(model)) {
    if (!is.null(model$contrasts)) contrasts = model$contrasts
    if (!is.null(model$xlevels)) factor_levels[names(model$xlevels)] = model$xlevels
  }
  structure(list(version=1L,formula=formula,model_formula=model_formula,data=data,rows=rows,
    plot_data=display,grid=grid,reference=reference,
    variables=list(outcome=outcome,predictors=predictors,numeric=types$numbers,categorical=types$characters),
    roles=flexplot_preparation_roles(axis,given),
    display_roles=flexplot_preparation_roles(flexplot_object_axis(display,axis),flexplot_magnet_display_given(display,given)),
    binning=list(bins=bins,breaks=breaks,labels=labels,variables=binned),
    factor_levels=factor_levels,contrasts=contrasts),class="flexplot_preparation")
}

flexplot_preparation_roles = function(axis, given) {
  list(x=axis[1],group=if (length(axis)>1) axis[2] else NULL,panels=given[!is.na(given) & nzchar(given)])
}

flexplot_preparation_contrasts = function(values) {
  if (is.character(values)) values = factor(values)
  if (nlevels(values)<2L) return(NULL)
  stats::contrasts(values)
}
