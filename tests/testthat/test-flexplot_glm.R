

test_that("flexplot with logistic regression works", {
  set.seed(2323)
  
  # logistic with a character
  expect_flexplot_snapshot("flexplot_glm/logistic-character-outcome.svg", "logistic: character outcome",
    flexplot(y_char~x, data=small_logistic, method="logistic"))
  # logistic with 1/0
  expect_flexplot_snapshot("flexplot_glm/logistic-0-1-outcome.svg", "logistic: 0/1 outcome",
    flexplot(y_numb~x, data=small_logistic, method="logistic"))
  
  
  # logistic with ordered factor
  expect_flexplot_snapshot("flexplot_glm/logistic-with-ordered-factor.svg", "logistic with ordered factor",
    flexplot(y_ord~x, data=small_logistic, method="logistic"))

  mod = glm(injury~attention, data=tablesaw.injury, family=binomial)
  expect_flexplot_snapshot("flexplot_glm/logistic-with-numeric-outcome.svg", "logistic with numeric outcome",
    compare.fits(injury~attention, data=tablesaw.injury, mod, jitter=c(0, .1))
  )
  
  mod = glm(y_numb~x, data=small_logistic, family=binomial)
  expect_flexplot_snapshot("flexplot_glm/compare-fits-with-logistic.svg", "compare.fits with logistic",
    compare.fits(y_numb~x, data=small_logistic, mod, jitter=c(0, .1))
  )
  
  
  expect_flexplot_snapshot("flexplot_glm/panelled-logistic-with-sampling.svg", "panelled logistic with sampling",
    flexplot(y_bin ~ x | a, data = small, method = "logistic")
  )
})
