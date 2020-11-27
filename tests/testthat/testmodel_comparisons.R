context("model.comparison works")
options(warn=-1)

data("exercise_data")

test_that("rlm and lm can be compared", {
  model1 = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  model2 = MASS::rlm(weight.loss~motivation + therapy.type, data=exercise_data)
  results = model.comparison(model1, model2)
  expect_output(print(results), "0.000 0.004 0.012 0.183 0.194")
})

test_that("nested models have correct r squared", {
  a = lm(weight.loss~motivation, data=exercise_data)
  b = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  model1 = a; model2 = b
  results = model.comparison(a,b)$statistics$r.squared[2]
  expect_equal(results, .217)
})

test_that("nonnested models correctly report null for p value", {
  a = lm(weight.loss~motivation + gender, data=exercise_data)
  b = lm(weight.loss~motivation + therapy.type, data=exercise_data)		
  results = model.comparison(a,b)
  expect_null(results$p.value, NULL)
})

test_that("glm vs lm works", {
  a = lm(health~motivation + gender, data=exercise_data)
  b = glm(health~motivation + gender, data=exercise_data, family=Gamma)		
  results = model.comparison(a,b)
  expect_equal(results$statistics$aic[1], 1382.206)
})

test_that("glm and glm works", {
  data("tablesaw.injury")
  head(tablesaw.injury)
  model1 = glm(injury~safety, data=tablesaw.injury, family=binomial)  
  model2 = glm(injury~safety + attention, data=tablesaw.injury, family=binomial)  
  res = model.comparison(model1, model2)
  expect_output(print(res), "1765.236 1775.706 6.145538e-03")
  model1 = glm(safety~attention, data=tablesaw.injury, family=poisson)  
  model2 = glm(safety~gender + attention, data=tablesaw.injury, family=poisson) 
  expect_true(model.comparison(model1, model2)$statistics$p.value[1] == "0.000356")
})

test_that("interaction vs lm works", {
  a = lm(weight.loss~motivation *therapy.type, data=exercise_data)
  b = lm(weight.loss~motivation+therapy.type, data=exercise_data)
  results = model.comparison(a,b)
  expect_equal(results$statistics$bayes.factor[1], .01)
})



test_that("random forest vs lm works returns predictions only", {
  set.seed(1212)
  a = randomForest(weight.loss~therapy.type, data=exercise_data)
  b = lm(weight.loss~therapy.type, data=exercise_data)
  results = model.comparison(a,b)
  expect_output(print(results), "0.001 0.032 0.058 0.102 0.256")
})

test_that("model comparisons with missing data", {
  
  a = lm(weight.loss~muscle.gain.missing+therapy.type, data=exercise_data)
  b = lm(weight.loss~therapy.type, data=exercise_data)
  expect_message(model.comparison(a,b), "Note: your models were fit to two different datasets.")
})


test_that("model comparisons with mixed models", {
  data(alcuse)
  mod1 = lme4::lmer(ALCUSE~1 + (1|ID), data=alcuse)
  mod2 = lme4::lmer(ALCUSE~AGE_14 + (1|ID), data=alcuse)
  mc = model.comparison(mod1, mod2)
  expect_equal(mc$statistics$bayes.factor[2], 826.257, tolerance = 0.01)
})


test_that("generate_predictions_table works", {
  big = glm(died~willpower + minutes.fighting + superpower + damage.resistance + speed + agility + iq + strength + flexibility, 
             data=avengers, family=binomial)
  small = glm(died~minutes.fighting, data=avengers, family=binomial)
  expect_true(generate_predictions_table(big)[1,1] == 17)
  expect_true(generate_predictions_table(small)[1,1] == 0)
  expect_true(length(unique(check_logistic_all_same(big)))==2)
  expect_true(length(levels(check_logistic_all_same(small)))==2)

})

test_that("sensitivity.table works", {
  small = glm(died~minutes.fighting, data=avengers, family=binomial)
  expect_true(sensitivity.table(small)$npv == 0)
  set.seed(232)
  rfmod = party::cforest(died~minutes.fighting, data=avengers, control = party::cforest_unbiased(ntree=10))
  expect_true(sensitivity.table(rfmod)$acc %>% round(2) ==.88)
})

test_that("missing data in a variable doesn't screw things up", {
  d = avengers
  d$willpower[1:10] = NA
  full = lm(ptsd~superpower + willpower, data=d)
  reduced = lm(ptsd~superpower, data=d)
  model.comparison(full, reduced)
})


test_that("get_p_value works", {
  full = glmer(progressed~redroom + class_size  + (1 | class_id), data=photo_lessons, family=binomial)         
  reduced = glmer(progressed~redroom  + (1 | class_id), data=photo_lessons, family=binomial) 
  expect_true(get_p_value(full, reduced)>.6 & get_p_value(full, reduced)<.63)
  full = glm(died~superpower + speed, data=avengers, family=binomial)
  reduced = glm(died~superpower, data=avengers, family=binomial)
  expect_true(get_p_value(full, reduced)<0.001)
})

options(warn=0)

















#