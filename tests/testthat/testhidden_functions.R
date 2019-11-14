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
  data("graduate_income")
  data = graduate_income
  outcome = "Income"
  tst = as.character(make_flexplot_formula(predictors = predictors, outcome, data))[3]
  expect_output(print(tst),'\\[1\\] "Years \\+ Grad\\.School \\| Profession \\+ GPA"')
  
  tst = as.character(make_flexplot_formula(predictors = "gender", outcome, data))[3]
  expect_output(print(tst),'gender')
})

test_that("match.jitter works", {
  expect_equal(match_jitter_categorical(.2), c(.2, 0))
  expect_equal(match_jitter_categorical(T), c(.2, 0))
  expect_equal(match_jitter_categorical(c(.2, .1)), c(.2, .1))
  expect_equal(match_jitter_categorical(F), c(0, 0))
  expect_warning(match_jitter_categorical(c(F, T)))
})

test_that("prep.breaks works", {
  expect_output(print(prep.breaks(variable = "satisfaction", data = relationship_satisfaction, breaks=NULL, bins=3)), "46.66667")
  expect_output(print(prep.breaks(variable = "satisfaction", data = relationship_satisfaction, breaks=c(20, 60))), "-7  20  60 117")
  expect_output(print(prep.breaks(variable = "satisfaction", data = relationship_satisfaction, breaks=NULL, bins=NULL)), "46.66667")
})

test_that("bin.me works", {
  res = levels(bin.me(variable="satisfaction", data=relationship_satisfaction, bins=3))
  expect_output(print(res), "46.7-58")
  res = levels(bin.me(variable="satisfaction", data=relationship_satisfaction, breaks = c(20, 60)))
  expect_output(print(res), "-7-20")
  res = levels(bin.me(variable="satisfaction", data=relationship_satisfaction, breaks = c(20, 60), check.breaks = F))
  expect_output(print(res), "20-60")
  res = levels(bin.me(variable="satisfaction", data=relationship_satisfaction, labels = c("a", "b", "c")))  
  expect_output(print(res), "b")
  res = bin.me(variable="satisfaction", data=relationship_satisfaction, breaks = list(c(20, 60, 80)), return.breaks=TRUE)
  expect_output(print(res), "-7  20  60  80 117")  
})