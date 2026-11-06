context("hidden_functions works as expected")
options(warn=-1)
data(exercise_data)
d = exercise_data
set.seed(1212)
test_that("rescale works", {
  expect_true(round(mean(rescale(d$weight.loss, 0, 2)), 0) == 0)
  expect_true(round(sd(rescale(d$weight.loss, 0, 2)), 0) == 2)
})

test_that("floor_ceiling work", {
  testval = c(-1, 0, 0, 60, 100, 101)
  expect_true(min(floor_ceiling(testval, 0))==0)
  expect_true(max(floor_ceiling(testval, 0, 100))==100)
})


test_that("nested model comparisons returns bf", {
  mod = lm(weight.loss~motivation + therapy.type + gender, data=exercise_data)
  expect_output(print(nested_model_comparisons(mod)), "1212109")
})


test_that("add_bin_to_new_dataset works", {
  # make fake mixed model data
  mod = lmer(weight.loss~motivation + muscle.gain +
         (motivation | satisfaction), data=exercise_data %>%
         mutate(across(c(weight.loss, motivation, muscle.gain), scale)))
  plot = flexplot(weight.loss~motivation | muscle.gain, data=exercise_data)
  new_bins = add_bin_to_new_dataset(plot,
                         d=exercise_data,
                         terms=c("weight.loss", "motivation", "muscle.gain", "satisfaction"),
                         term.re = "satisfaction", outcomevar="weight.loss")$muscle.gain_binned
  old_bins = plot$data$muscle.gain_binned
  # this failed prior to flexplot 0.11.1 because negative numbers weren't accounted for
  expect_true(all(levels(new_bins) == levels(old_bins)))
})

test_that("check.non.number returns nonnumber", {
  expect_false(check.non.number(c(1,1,2,3,2,1)))
  expect_true(check.non.number(c(letters[1:10])))
  expect_true(check.non.number(factor(c(letters[1:10]))))
})

test_that("variable types figures out right variables", {
  tst = variable_types(variables = c("gender", "motivation", "therapy.type"), data=d)
  expect_true(sum(tst$characters)==2)
  expect_true(sum(tst$numbers)==1)
})

test_that("make flexplot formula works", {
  predictors = c("Grad.School", "Years", "GPA", "Profession")
  data("graduate_income")
  data = graduate_income
  outcome = "Income"
  tst = as.character(make_flexplot_formula(predictors = predictors, outcome, data))[3]
  expect_output(print(tst),'\\[1\\] "Years \\+ Grad\\.School \\| Profession \\+ GPA"')

  tst = as.character(make_flexplot_formula(predictors = "Years", outcome, data))[3]
  expect_output(print(tst),'Years')

  tst = as.character(make_flexplot_formula(predictors = c("Years", "GPA", "gender:GPA"), outcome, data))[3]
  expect_output(print(tst),'Years \\| GPA')

  expect_equal(paste0(make_flexplot_formula(NULL, "therapy.type", exercise_data))[3], "1")
})

test_that("match.jitter works", {
  expect_equal(match_jitter_categorical(.2), c(.2, 0))
  expect_equal(match_jitter_categorical(.1), c(.1, 0))
  expect_equal(match_jitter_categorical(T), c(.2, 0))
  expect_equal(match_jitter_categorical(c(.2, .1)), c(.2, .1))
  expect_equal(match_jitter_categorical(F), c(0, 0))
})


test_that("sample.subset returns a dataset the right rows", {
  set.seed(232)
  d = exercise_data
  expect_equal(nrow(sample.subset(10, d)), 10)
  expect_equal(nrow(sample.subset(Inf, d)), nrow(d))
  # when user asks to sample more than the number of rows
  expect_equal(nrow(sample.subset(111111111, d)), nrow(d))
})


test_that("factor.to.logistic works", {
  expect_equal(levels(factor.to.logistic(exercise_data, "therapy.type")$therapy.type), c("control", "cog", "beh"))
  expect_equal(length(levels(factor.to.logistic(exercise_data, "gender", labels=T))), 2)
  expect_equal(unique(factor.to.logistic(exercise_data, "gender", method="logistic")$gender), c(1,0))
  expect_equal(unique(factor.to.logistic(exercise_data, "gender", method="loess")$gender)%>%as.character, c("female", "male"))
  expect_equal(unique(factor.to.logistic(small, "y_bin")$y_bin), c(1,0))
})


test_that("there are no 'browser' or 'save' calls", {
  expect_message(fif("browser()", where="R"), "(No results found)")
  expect_message(fif("save", where="R"), "(No results found)")
})


test_that("remove_nonlinear_terms works", {
  formula = y~a*b + I(c^2) + I(d^4)
  terms = attr(terms(formula), "term.labels")
  expect_true(all(c("a", "b") %in% remove_nonlinear_terms(terms)))
})


test_that("compare.fits_subroutines work", {
  mod1 = lm(weight.loss~1, data=exercise_data)
  mod2 = lm(weight.loss~therapy.type, data=exercise_data)
  testthat::expect_equal(length(all.vars(formula(whats_model2(mod1)))), 1)
  testthat::expect_equal(length(all.vars(formula(whats_model2(mod1, mod2)))), 2)

  model = suppressWarnings(party::cforest(weight.loss~therapy.type, data=exercise_data))
  testthat::expect_equal(get_terms(model)$predictors, "therapy.type")
  model = lm(weight.loss~therapy.type, data=exercise_data)
  testthat::expect_equal(get_terms(model)$predictors, "therapy.type")
})

lm_mod = lm(weight.loss~therapy.type, data=exercise_data)
glm_mod = glm(kills~superpower, data=avengers %>% mutate(kills = kills + 1), family=Gamma)
rf_mod_cf = party::cforest(kills~superpower+ speed , data=avengers)
rf_mod_rf = randomForest::randomForest(kills~superpower+ speed , data=avengers)

test_that("extract_data_from_fitted_objects works", {

  expect_true(nrow(extract_data_from_fitted_object(lm_mod))==200)
  expect_true(nrow(extract_data_from_fitted_object(glm_mod))==812)
  expect_true(nrow(extract_data_from_fitted_object(rf_mod_cf))==812)
  expect_true(nrow(extract_data_from_fitted_object(rf_mod_rf))==812)
})

test_that("get_predictors works", {
  expect_true(get_predictors(lm_mod) == "therapy.type")
  expect_true(get_predictors(glm_mod)== "superpower")
  expect_true(get_predictors(rf_mod_cf)[1] == "superpower")
  expect_true(get_predictors(rf_mod_rf)[1] == "superpower")
})

test_that("check_nested works", {
  lm_mod2 = lm(weight.loss~therapy.type + motivation, data=exercise_data)
  expect_true(check_nested(lm_mod, lm_mod2))
  glm_mod2 = glm(kills~north_south, data=avengers %>% mutate(kills = kills + 1), family=Gamma)
  expect_false(check_nested(glm_mod, glm_mod2))
  expect_false(check_nested(rf_mod_cf, rf_mod_rf))
})

test_that("check_model_rows works", {
  mod1 = lm(weight.loss~therapy.type, data=exercise_data)
  mod2 = lm(weight.loss~therapy.type + motivation, data=exercise_data %>% mutate(motivation = ifelse(motivation>70, NA, motivation)))
  expect_message(check_model_rows(mod1, mod2, T))
  new_mods = suppressMessages(check_model_rows(mod1, mod2, T))
  expect_true(nrow(new_mods[[1]]$model) == nrow(new_mods[[2]]$model))
})


test_that("check_all_variables_exist_in_data works", {
  expect_null(check_all_variables_exist_in_data(c("weight.loss", "therapy.type"), exercise_data))
  expect_null(check_all_variables_exist_in_data(NULL, exercise_data))
  expect_error(check_all_variables_exist_in_data(c("weight.loss", "therrapy.type"), exercise_data))
})

test_that("formula_functions works", {
  a = 1:5
  b = a*2
  y = 1:5*.1
  d = data.frame(a,b, y)
  expect_true(is.factor(formula_functions(y~a + as.factor(b), d)$data$b))
})


test_that("extract_numbers_from_binned_var works", {
  expect_true(is.factor(extract_numbers_from_binned_var(c("-3-8", "8-11", "-3-8", "8-11"), c(5, 10, -2, 9))[1]))
})


options(warn=0)
#
