context("helper_estimates")

test_that("return_logistic_coefficients works", {
  expect_true(length(return_logistic_coefficients(lme4::glmer(y_bin~x + (1|a), data=small, family=binomial))) == 2)
  expect_true(length(return_logistic_coefficients(        glm(y_bin~x + a    , data=small, family=binomial))) == 3)
  expect_error(return_logistic_coefficients(        glm(y_bin~x + a    , data=small)))
  coefs = return_logistic_coefficients(lme4::glmer(y_bin ~ x + (1 | a), data = small, family = binomial))
  expect_type(coefs, "double")
  expect_named(coefs)
  expect_true("x" %in% names(coefs))
  
  model = glm(y_bin ~ x + a, data = small, family = binomial)
  coef_table = logistic_coefficients(model)
  expect_setequal(coef_table$variable, c("x", "a"))
  expect_true(logistic_coefficients(model, location = .7)$instantaneous_slope[1] != coef_table$instantaneous_slope[1])
})

test_that("gather_model_info returns expected components", {
  model = glm(y_bin ~ x + a, data = small, family = binomial)
  info = gather_model_info(model)
  
  expect_type(info, "list")
  expect_true(all(c("model", "data", "coefs", "coef_names", "term_names", "term_labels", "mm") %in% names(info)))
  expect_s3_class(info$model, "glm")
  expect_true(is.data.frame(info$data))
  expect_type(info$coefs, "double")
  expect_named(info$coefs)
  expect_type(info$coef_names, "character")
  expect_type(info$term_names, "character")
  expect_type(info$term_labels, "integer")
  expect_true(is.matrix(info$mm))
})

test_model = glm(y_bin ~ x + a, data = small, family = binomial)

test_that("compute_logistic_summary_for_variable returns correct structure", {
  result = compute_logistic_summary_for_variable("x", model = test_model)
  
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("variable", "instantaneous_slope", "intercept_threshold"))
  expect_equal(result$variable, "x")
  expect_type(result$instantaneous_slope, "double")
  expect_type(result$intercept_threshold, "double")
})

test_that("threshold value is within reasonable range", {
  result = compute_logistic_summary_for_variable("x", model = test_model)
  expect_true(result$intercept_threshold > -100 && result$intercept_threshold < 100)
})

test_that("factors do not break compute_logistic_summary_for_variable", {
  small$a = as.factor(small$a)
  model = glm(y_bin ~ x + a, data = small, family = binomial)
  result = compute_logistic_summary_for_variable("a", model = model)
  expect_equal(result$variable, "a")
  expect_equal(result$intercept_threshold, NA)
})


test_that("gather_model_info works with glmer models", {
  model = lme4::glmer(y_bin ~ x + (1 | a), data = small, family = binomial)
  info = gather_model_info(model)
  
  expect_equal(class(info$model)[1], "glmerMod")
  expect_true("(Intercept)" %in% info$coef_names)
})


test_that("factor terms do not break threshold extraction", {
  small$a = as.factor(small$a)
  mod = glm(y_bin ~ x + a, data = small, family = binomial)
  out = logistic_coefficients(mod)
  expect_true("a" %in% out$variable)
})


test_that("match_term_names works", {
  mod = glm(y_bin~x + a, data=small, family=binomial)
  expect_equal("a", match_term_names(mod, name="ab"))
  expect_equal("unknown", match_term_names(mod, name="yoyo"))
})
test_that("return_levels_or_mean", {
  expect_equal(return_levels_or_mean("a", c("a"), small)%>%as.character, "a")
  expect_equal(return_levels_or_mean("z", "a", small), .3853, tol=.01)
})

test_that("return_averages works", {
  model = lm(y~a + z +b , data=small)
  expect_equal(return_averages(model, "a", TRUE)$a%>%as.character, "a")
  expect_equal(return_averages(model, "z", TRUE)$z, .3853, tol=.01)
  expect_message(return_averages(model, "a", FALSE))
  expect_null(return_averages(model, character(0)))
})

test_that("return_factor_levels works", {
  expect_equal(return_factor_levels("a", small)$a%>%as.character, c("a", "b"))
  expect_null(return_factor_levels(character(0), small))  
  expect_equal(return_factor_levels(c("a", "b"), small)$b%>%as.character, c("z", "y", "x"))
})

test_that("return_plus_minus_one_sd works", {
  expect_equal(return_plus_minus_one_sd(1:10)[1], 8.52765, tol=.01)
  expect_equal(length(return_plus_minus_one_sd(1:10)), 2)
})

test_that("anchor.predictions works for categorical predictors", {
  linear.model = lm(weight.loss~health + gender, data= exercise_data)
  expect_message(anchor.predictions(linear.model, "gender")$prediction[1])
  linear.model = lm(weight.loss~therapy.type + gender, data= exercise_data)
  expect_equal(anchor.predictions(linear.model, c("therapy.type", "gender"))$prediction[1],
               4.3475, tolerance = .002)
})

test_that("bf.bif works", {
  expect_equal(bf_bic(lm(y~x + a, data=small), lm(y~x * a, data=small)), 3.339977, tol = .01)
  expect_equal(bf_bic(lm(y~x + a, data=small), lm(y~x * a, data=small), invert=T), 1/3.339977, tol = .01)
})

test_that("which_terms_are_factors_or_numbers works", {
  expect_true(length(which_terms_are_factors_or_numbers(small, "a")$factors)==1)
  expect_true(length(which_terms_are_factors_or_numbers(small, "x")$factors) ==0)
  expect_true(length(which_terms_are_factors_or_numbers(small, c("x", "y", "a", "b"))$factors) == 2)
})

test_that("compute_semi_partial works", {
  mod = lm(y~x, data=small)
  expect_equal(as.numeric(compute_semi_partial(mod)), summary(mod)$r.squared)
  expect_equal(names(compute_semi_partial(lm(y~a, data=small))), "a")
  expect_equal(names(compute_semi_partial(lm(y~a+b+z, data=small))), c("a", "b", "z"))
  expect_message(compute_semi_partial(lm(y~a*b, data=small)))
})

test_that("populate_estimates_factors works", {
  expect_equal(populate_estimates_factors(lm(y~x, data=small))$coef.matrix, NA)
  expect_equal(levels(populate_estimates_factors(lm(y~a, data=small))$coef.matrix$variables)[2], "a")
})

test_that("populate_estimates_numeric works", {
  expect_equal(dim(populate_estimates_numeric(lm(y~x, data=small))), c(2,7))
  expect_equal(dim(populate_estimates_numeric(lm(y~a, data=small))), NULL)
})

test_that("compute_r_squared works", {
  expect_equal(length(compute_r_squared(lm(y~x, data=small))), 3)
  expect_true(all(compute_r_squared(lm(y~1, data=small))==0))
})

test_that("compute_correlation works", {
  expect_true(is.numeric(compute_correlation(lm(y~x, data=small))))
  expect_true(is.na(compute_correlation(lm(y~a, data=small))))
  expect_true(is.na(compute_correlation(lm(y~x + z, data=small))))
})

test_that("bic works", {
  model.me = lm(weight.loss ~ motivation+therapy.type, data = exercise_data)
  model.int = lm(weight.loss ~ motivation*therapy.type, data = exercise_data)
  expect_equal(bf.bic(model.me, model.int, invert=T), .01049, tolerance=.001)
})  

test_that("icc works", {
  data(math)
  mod = lmer(MathAch~1 + (1|School), data=math)
  expect_equal(icc(mod)$icc, .18, tol=.01)
})

test_that("removing interactions works", {
  mod = lm(kills~agility*speed, data=avengers)
  expect_true(length(remove_interaction_terms(mod))==2)
})

test_that("generate_grid_predictions", {
  expect_equal(nrow(generate_grid_predictions(list(a=1:3, b=4:5), list(c = c('a', 'b')), NULL)), 12)
  expect_null(generate_grid_predictions(NULL, NULL, NULL))
})

