fit_baseline_model = function(object) {
  dv = get_terms(object)$response
  re = get_re(object)
  form = as.formula(
    paste0(dv, "~1+(1|", re, ")")
  )
  return(update(object, formula=form))
}

test_that("icc works", {
  data(math)
  mod = lmer(MathAch~1 + (1|School), data=math)
  expect_equal(icc(mod)$icc, .18, tol=.01)
})