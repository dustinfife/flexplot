
#expect_equal(.get_re_term(lme4::lmer(y~x + (x | id), data=small_mixed)), "id")
.get_re_term = function(m) {
  fl = lme4::getME(m, "flist")
  if (length(fl) == 0) stop("Model has no random-effects grouping factor.")
  names(fl)[1]  # use the first grouping term (consistent with your helpers)
}
usethis::use_test("plot_cas")

compute_cas_from_model = function(m, outcome_name = NULL, adjust = c("all","intercept","none")) {
  
  if (is.null(outcome_name)) {
    all_variables = all.vars(formula(m))
    outcome_name = all_variables[1]
  }
  adjust = match.arg(adjust, c("all","intercept","none"))
  d = m@frame
  re_term = .get_re_term(m)
  
  # predictions
  pred_fixed = predict(m, re.form = NA)
  pred_cond  = predict(m, re.form = NULL)
  
  # intercept-only prediction (random intercepts, no slopes)
  pred_int = tryCatch(
    predict(m, re.form = as.formula(paste0("~(1|", re_term, ")"))),
    error = function(e) NULL
  )

  y_adj = cas_adjustment(d=d, model = m, outcome_name = outcome_name, adjust=adjust,
                              pred_fixed = pred_fixed, pred_cond = pred_cond)
  out = d
  out$y_adj = y_adj
  out$pred_fixed = pred_fixed
  out$.re_term = re_term
  out
}

# cas_adjustment(small_mixed, mod1, adjust="all")
# cas_adjustment(small_mixed, model = mod2, adjust="intercept")
# expect_equal(cas_adjustment(small_mixed, mod1, adjust="none"), small_mixed$y)
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
      predict(model, re.form = as.formula(paste0("~(1|", re_term, ")"))),
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

cluster_adjusted_scatter = function(formula, object,
                                    adjust = c("all","intercept","none"),
                                    ...) {
  adjust = match.arg(adjust, c("all","intercept","none"))
  
  # parse formula (flexplot-style y ~ x)
  vars = all.vars(formula)
  if (length(vars) < 2) stop("Formula must be like y ~ x.")
  outcome = vars[1]
  xvar    = vars[2]
  
  if (!all(c(outcome, xvar) %in% names(object@frame))) {
    stop("Variables in formula must exist in the model frame.")
  }
  
  d = compute_cas_from_model(object, outcome_name = outcome, adjust = adjust)
  re_term = d$.re_term
  form = as.formula(paste("y_adj ~", deparse(formula[[3]])),
                     env = environment(formula))
  
  # get total clusters so it samples everything. 
  clusters = unique(d[[d$.re_term[1]]])
  compare.fits(form, data=d, model1=object, clusters = length(clusters), ...)
}
