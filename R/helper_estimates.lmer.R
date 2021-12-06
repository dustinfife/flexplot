fit_baseline_model = function(object) {
  dv = get_terms(object)$response
  re = get_re(object)
  form = as.formula(
    paste0(dv, "~1+(1|", re, ")")
  )
  return(update(object, formula=form))
}

