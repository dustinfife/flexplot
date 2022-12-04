

test_that("flexplot with logistic regression works", {
  set.seed(2323)
  
  # logistic with a character
  vdiffr::expect_doppelganger(
    "logistic: character outcome", 
    flexplot(y_char~x, data=small_logistic, method="logistic"))
  
  # logistic with 1/0
  vdiffr::expect_doppelganger(
    "logistic: 0/1 outcome", 
    flexplot(y_numb~x, data=small_logistic, method="logistic"))
  
  
  # logistic with ordered factor
  vdiffr::expect_doppelganger(
    "logistic with ordered factor",
    flexplot(y_ord~x, data=small_logistic, method="logistic"))

  mod = glm(injury~attention, data=tablesaw.injury, family=binomial)
  vdiffr::expect_doppelganger(
    "logistic with numeric outcome",
    compare.fits(injury~attention, data=tablesaw.injury, mod, jitter=c(0, .1))
  )
  
  mod = glm(y_numb~x, data=small_logistic, family=binomial)
  vdiffr::expect_doppelganger(
    "compare.fits with logistic",
    compare.fits(y_numb~x, data=small_logistic, mod, jitter=c(0, .1))
  )
  
  
  vdiffr::expect_doppelganger(
    "panelled logistic with sampling",
    flexplot(y_bin ~ x | a, data = small, method = "logistic")
  )
})
