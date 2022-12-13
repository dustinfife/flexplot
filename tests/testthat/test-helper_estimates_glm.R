test_that("output_glm_predictions works", {
  mod = glm(y_bin~y + z + a, data=small, family="binomial") %>% suppressWarnings()
  expect_true(length(output_glm_predictions(mod, remove_interaction_terms(mod)))>0)
  mod = lme4::glmer(y_bin~y + z +  (z | a), data=small, family="binomial") %>% suppressWarnings()
  expect_true(output_glm_predictions(mod, remove_interaction_terms(mod)) %>%is.na)
  logistic_fit_interaction
})

test_that("n.func works", {
  mod = glm(y_bin~z + b, data=small, family="binomial")
  expect_true(length(n.func("z", mod))==2)
  expect_true(length(n.func("b", mod))==length(levels(small$b)))
})

test_that("output_coef_matrix_glm works", {
  expect_equal(names(output_coef_matrix_glm(logistic_fit))[2], "OR")
  expect_equal(names(output_coef_matrix_glm(poisson_fit))[2], "multiplicative.coef")
  expect_equal(names(output_coef_matrix_glm(gamma_fit))[2], "inverse.coef")
  expect_equal(colnames(output_coef_matrix_glm(gaussian_fit))[2], "Std. Error")  
  expect_equal(colnames(output_coef_matrix_glm(mixed_logistic))[2], "OR")  
})

test_that("round_string works", {
  expect_equal(round_string(.3939382), .39)
  expect_equal(round_string(0.0005448), "<0.01")
  expect_equal(round_string(12.587), 12.59)
})

test_that("round_coefficient_matrix works", {
  coef_matrix = output_coef_matrix_glm(logistic_fit)
  test_value = round_coefficient_matrix(coef_matrix)[1,1]
  expect_true(nchar(test_value)==5)
  expect_false(nchar(coef_matrix[1,1])==5)
})
