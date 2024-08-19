test_that("generate_coef_matrix works", {
  expect_equal(generate_coef_matrix(mixed_logistic)[1,4], "0.21 (ano prediction)")
  expect_equal(generate_coef_matrix(mixed_logistic_2f)[4,4], "0.08 (relative to bno)")
  
  ## add other types of models, like logistic
})

test_that("sd_difference_matrix works", {
  expect_true(!is.na(sd_difference_numeric(object = mixed_logistic)[2,4]))
  
  no_numbers = glmer(y_binary~a + (a |id), data=small_mixed, family = "binomial")
  expect_false("Prediction Difference (+/- 1 SD)" %in% names(sd_difference_numeric(object = no_numbers)))
})
  

test_that("sd_difference_factor", {
  
  sd_difference_factors(object=mixed_logistic) %>%
    select(`Prediction Difference (+/- 1 SD)`) %>%
    is.na %>%
    sum %>%
    expect_equal(1)
  
  glmer(y_binary~x + (1|id), data=small_mixed, family="binomial") %>%
    sd_difference_factors(coef.matrix=NULL, object=.) %>%
    names %>%
    {"Prediction Difference (+/- 1 SD)" %in% .} %>%
    expect_false

})

test_that("find_referent_group works", {
  find_referent_group(1, mixed_logistic) %>%
    select(levs) %>%
    pluck(1) %>%
    {all(c("ano", "ayes") %in% .)} %>%
    expect_true
})
  