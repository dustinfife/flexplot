context("hidden_functions works as expected")

data(exercise_data)
d = exercise_data
set.seed(1212)
test_that("standardized difference works", {
  
  mod1 = lm(weight.loss~therapy.type, data=d)	
  mod2 = lm(weight.loss~therapy.type+gender, data=d)	
  diff = standardized_differences(mod1, mod2)
  expect_output(print(diff), "0.156")
  
  data("criminal_data")
  mod1 = glm(aggression~ses + empathy + depression, data=criminal_data, family=Gamma)
  mod2 = glm(aggression~ses * empathy + depression, data=criminal_data, family=Gamma)
  diff = standardized_differences(mod1, mod2)
  expect_output(print(diff), "0.294")
})

test_that("nested model comparisons returns bf", {
  mod = lm(weight.loss~motivation + therapy.type + gender, data=exercise_data)
  expect_output(print(nested_model_comparisons(mod)), "1212109")
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
  data = graduate_income
  outcome = "Income"
  tst = as.character(make_flexplot_formula(predictors = predictors, outcome, data))[3]
  expect_output(print(tst),'\\[1\\] "Years \\+ Grad\\.School \\| Profession \\+ GPA"')
})

test_that("match.jitter works", {
  expect_equal(match_jitter_categorical(.2), c(.2, 0))
  expect_equal(match_jitter_categorical(T), c(.2, 0))
  expect_equal(match_jitter_categorical(c(.2, .1)), c(.2, .1))
  expect_equal(match_jitter_categorical(F), c(0, 0))
  expect_warning(match_jitter_categorical(c(F, T)))
}
          
)

