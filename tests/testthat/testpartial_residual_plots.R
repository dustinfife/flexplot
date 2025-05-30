context("partial_residual_plots")
set.seed(2323)

test_that("partial_residual_plot works", {
  mod = lm(health~motivation + weight.loss , data=exercise_data)
  vdiffr::expect_doppelganger("partial_residual plot with one variable",
                              partial_residual_plot(health~weight.loss, 
                                                    model=mod, 
                                                    added_term = ~weight.loss, 
                                                    data=exercise_data))
  mod = lm(health~motivation + weight.loss + therapy.type , data=exercise_data)
  vdiffr::expect_doppelganger("partial_residual plot with two variables",
                              partial_residual_plot(health~weight.loss + therapy.type, 
                                                    model=mod, 
                                                    added_term = ~weight.loss + therapy.type, 
                                                    data=exercise_data) ) 
  mod = lm(health~weight.loss + motivation * therapy.type, data=exercise_data)
  vdiffr::expect_doppelganger("partial_residual plot with formula term",
                              partial_residual_plot(health~ motivation + therapy.type, 
                                                    model=mod,
                                                    added_term = ~motivation*therapy.type, 
                                                    data=exercise_data))
  load(file=system.file("datasets", "ideation.rda", package="flexplot"))
  right_model = lm(ideation~depression_c*friend_ideation_c + stress_c + I(stress_c^2) + health, data=ideation)
  p = partial_residual_plot(ideation~friend_ideation_c | depression_c,
                            model=right_model,
                            added_term = ~friend_ideation_c*depression_c, data=ideation)
  vdiffr::expect_doppelganger("partial_residual with interaction specified backward", 
                              p)
  
  #prp with no added_term arguments
  vdiffr::expect_doppelganger("prp with no added_term argument specified", 
                              partial_residual_plot(ideation~friend_ideation_c | depression_c,
                                                    model=right_model,
                                                    data=ideation,
                                                    method="quadratic"))
})

test_that("prp with binary predictor", {
  data("small", package = "flexplot")
  vdiffr::expect_doppelganger("prp with binary predictor on x axis",
                    partial_residual_plot(x~y_bin + a, data=small,
                        model=lm(x~y_bin + a + z, data=small),
                        added_term = ~y_bin))
})

test_that("return_term_location works", {
  model = lm(weight.loss~therapy.type + motivation + health, data=exercise_data)
  expect_error(return_term_location(model, NULL))
  expect_error(return_term_location(model, "motiv"))
  expect_error(return_term_location(model, c("motiv", "health")))  
  expect_equal(return_term_location(model, "therapy.type"), 2)
  expect_equal(return_term_location(model, c("therapy.type", "health")), c(2,4))
})

test_that("partial_residual works", {
  mod = lm(health~motivation + therapy.type + muscle.gain, data=exercise_data)
  expect_equal(sum(partial_residual(mod, c("motivation", "therapy.type"))), -894.6831, tol=.001) 
  expect_equal(round(as.numeric(partial_residual(mod, ~motivation)[1])*100), -358)
})

test_that("return_matching_terms works", {
  model = lm(health~weight.loss + motivation * therapy.type, data=exercise_data)
  expect_true(all(return_matching_terms(~therapy.type*motivation, model)==c("motivation", "therapy.type", "motivation:therapy.type")))
  expect_true(all(return_matching_terms(~therapy.type+motivation, model)==c("therapy.type","motivation")))
              expect_true(all(return_matching_terms(~motivation*therapy.type, model)==c("motivation", "therapy.type", "motivation:therapy.type")))
})
test_that("reorder_interaction_terms works", {
  expect_true(reorder_interaction_terms("a:c:b")=="a:b:c")
  expect_true(reorder_interaction_terms("a")=="a")
})
test_that("terms_to_modelmatrix works", {
  expect_true(ncol(terms_to_modelmatrix("motivation", exercise_data))==2)
              expect_true(ncol(terms_to_modelmatrix(~motivation, exercise_data))==2)
})
test_that("get_columns_as_list works", {
  d1 = data.frame(a=1:5, b=1:5, c=6:10)
  d2 = data.frame(a=1:5, d =6:10*3)
  expect_true(length(data_columns_as_list(d1, d2)) == 5)
})
test_that("keep_singles and keep_duplicate works", {
  original_model = model.matrix(~satisfaction*motivation + health, data=exercise_data)
  new_model = model.matrix(~motivation*satisfaction, data=exercise_data)
  expect_true(keep_singles(original_model, new_model)=="health")
  expect_false("health" %in% names(keep_duplicates(original_model, new_model)))
              
})
test_that("get_same_columns works", {
  d = data.frame(a=1:5*.1, b=1:5*.2, c=1:5*.4)
  d$ab = d$a*d$b
  e = d[,c("a", "b", "ab")]
  names(d)[3] = "ba"
  expect_true(duplicated(list(d$ab, get_same_columns(d,e)[,"ab"]))[2])
})