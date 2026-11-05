
#expect_equal(.get_re_term(lme4::lmer(y~x + (x | id), data=small_mixed)), "id")
.get_re_term = function(m) {
  fl = lme4::getME(m, "flist")
  if (length(fl) == 0) stop("Model has no random-effects grouping factor.")
  names(fl)[1]  # use the first grouping term (consistent with your helpers)
}

compute_cas_from_model = function(m, outcome_name = NULL, adjust_re = NULL) {

  if (is.null(outcome_name)) {
    all_variables = all.vars(formula(m))
    outcome_name = all_variables[1]
  }
  d = m@frame
  re_term = .get_re_term(m)
  
  # predictions
  pred_fixed = predict(m, re.form = NA)
  pred_cond  = predict(m, re.form = NULL)
  
  # if user didn't specify adjust_re, default to intercept-only
  if (is.null(adjust_re) || adjust_re == "intercept") {
    adjust_re = as.formula(paste0("~(1|", re_term, ")"))
  }
  
  # prediction for the component to remove
  if (identical(adjust_re, "all")) {
    pred_adj = d[[outcome_name]] - (pred_cond - pred_fixed)
  } else {
    if (identical(adjust_re, "none")) adjust_re = NA
    pred_adj = predict(m, re.form = adjust_re, allow.new.levels = TRUE)
  }
  
  # adjusted outcome
  y = d[[all.vars(formula(m))[1]]]
  y_adj = y - (pred_adj - pred_fixed)
  
  out = d
  out[[paste0(outcome_name, "_adj")]] = y_adj
  out$pred_fixed = pred_fixed
  out$.re_term = re_term
  out
}


#' Create cluster adjusted dataset
#'
#' @param d The dataset with clustered data
#' @param model A model. Defaults to NULL. 
#' @param adjust The type of cluster adjustment (all, intercept, or none)
#' @param pred_fixed Prediction of the fixed effect. Defaults to NULL
#' @param pred_cond Prediction of the conditional effect. Defaults to NULL. 
#' @param pred_int Prediction of the interceopt. Defaults to NULL. 
#' @param outcome_name The name of the DV. Defaults to NULL. 
#' 
#' There are a lot of NULL options to make it easier for testing. The best approach
#' is to fit a model first, then provide that as input to the model. If not, you have
#' to fit the model outside of the function, which is pretty lame. See examples
#'
#' @returns a dataset with the dependency in the data removed
#' @export
#'
#' @examples
#' mod1 = lme4::lmer(y~x + (x | id), data=small_mixed)
#' new_small  = cas_adjustment(small_mixed, model = mod1, adjust = "all")
cas_adjustment = function(d, model=NULL, 
                          adjust = c("all","intercept","none"), 
                          pred_fixed=NULL, 
                          pred_cond=NULL, 
                          pred_int =NULL,
                          outcome_name = NULL) {
  
  re_term = .get_re_term(model)

  if (is.null(outcome_name) | is.null(adjust)) {
    pred_fixed = predict(model, re.form=NA)
    pred_cond  = predict(model, re.form=NULL)
  }
  
  if (is.null(pred_int)) {
    pred_int = tryCatch(
      predict(model, re.form = as.formula(paste0("~(1|", re_term, ")")), allow.new.levels = TRUE),
      error = function(e) NULL
    )
  }   
  
  if (is.null(outcome_name)) outcome_name = paste0(formula(model))[2]
  
  y = d[[outcome_name]]
  
  if (adjust == "none") {
    return(y)
  } 
  
  if (adjust == "intercept") {
    if (is.null(pred_int)) stop("Model does not have a random intercept term for ", re_term)
    y_adj = y - (pred_int - pred_fixed)
    return(y_adj)
  } 
  
  y_adj =(y - (pred_cond)) + pred_fixed
  return(y_adj)
}


#' Cluster adjusted scatterplot
#'
#' @param formula a flexplot-like formula
#' @param object a fitted lme4 object
#' @param adjust how the outcome should be adjusted
#' @param ... any other parameters passed to flexplot
#'
#' @returns a cluster-adjusted scatterplot
#' @export
#'
#' @examples
#' mod1 = lme4::lmer(y~x + (x | id), data=small_mixed)
#' cluster_adjusted_scatter(y~x, object = mod1)
cluster_adjusted_scatter = function(formula, object=NULL, data=NULL, 
                                    random = NULL, 
                                    adjust_re = "all",
                                    smooth=F,
                                    ...) {
  #adjust = match.arg(adjust, c("all","intercept","none"))
  # parse formula (flexplot-style y ~ x)
  vars = all.vars(formula)
  if (length(vars) < 2) stop("Formula must be like y ~ x.")
  outcome = vars[1]
  xvar    = vars[2]

  # fit the model if they don't provide one
  if (is.null(object)) {
    if (is.null(random) | is.null(data)) {
      stop("If you don't provide a model, you must provide both a random object and a dataset")
    }
    # remove faceting part (anything after a '|') from the formula
    base_formula_str = deparse(formula)
    base_formula_str = gsub("|", "+", base_formula_str, fixed = T)
    base_formula_str = gsub("+", "*", base_formula_str, fixed = T)
    base_formula = as.formula(trimws(base_formula_str))
    
    # build the random term string
    rand_str = if (inherits(random, "formula")) sub("^~", "", paste(deparse(random), collapse="")) else random
    
    # now combine properly
    # Build formula manually to preserve order
    lhs = all.vars(base_formula)[1]
    rhs = as.character(base_formula)[3]
    rhs_full = paste(rhs, "+", rand_str)
    form_str = paste(lhs, "~", rhs_full)
    form = as.formula(form_str, env = environment(formula))
    
    object = lme4::lmer(form, data=data)
  }
  
  
  if (!all(c(outcome, xvar) %in% names(object@frame))) {
    stop("Variables in formula must exist in the model frame.")
  }

  d = compute_cas_from_model(object, outcome_name = outcome, adjust_re = adjust_re)
  re_term = d$.re_term
  
  new_outcome = paste0(outcome, "_adj")
  form = as.formula(paste(new_outcome, " ~", deparse(formula[[3]])),
                     env = environment(formula))
  
  # get total clusters so it samples everything. 
  clusters = unique(d[[d$.re_term[1]]])
  smooth_geom = if (isTRUE(smooth)) geom_smooth() else NULL
  compare_fits(form, data=d, model1=object, clusters = length(clusters), ...) +
    smooth_geom
    
}

add_random = function(form, random_terms) {
  to_piece = function(x) {
    if (inherits(x, "formula")) sub("^~", "", paste(deparse(x), collapse="")) else as.character(x)
  }
  rhs = paste(vapply(as.list(random_terms), to_piece, character(1)), collapse = " + ")
  update(form, as.formula(paste("~ . +", rhs), env = environment(form)))
}
