context("model.comparison works")
options(warn=-1)
require(lme4)
data("exercise_data")
data("tablesaw.injury")

# fit all sorts of models I'll use
nlr = lm(weight.loss~motivation, data=exercise_data)
nlf = lm(weight.loss~motivation + therapy.type, data=exercise_data)
nnl = lm(weight.loss~health + therapy.type, data=exercise_data)
log_full = glm(injury~safety + attention, data=tablesaw.injury, family=binomial)
log_reduced = glm(injury~attention, data=tablesaw.injury, family=binomial)
pois_full = glm(convictions~ses + empathy + depression, data=criminal_data, family=poisson)
pois_reduced = glm(convictions~ses + empathy , data=criminal_data, family=poisson)
gamma_full = glm(aggression~ses + empathy + depression, data=criminal_data, family=Gamma)
gamma_reduced = glm(aggression~ses + empathy , data=criminal_data, family=Gamma)
mixed_full = lmer(MathAch~SES + Sex + (SES | School), data=math)
mixed_reduced = lmer(MathAch~ Sex + (1 | School), data=math)
mixed_pois = glmer(ALCUSE~COA + PEER + AGE + (AGE | ID), data=alcuse %>% mutate(ALCUSE = ALCUSE + 1), family=Gamma)
mixed_pois_reduced = glmer(ALCUSE~COA + (1 | ID), data=alcuse %>% mutate(ALCUSE = ALCUSE + 1), family=Gamma)
rf = randomForest::randomForest(weight.loss~therapy.type + motivation, data=exercise_data)
rf2 = randomForest::randomForest(weight.loss~therapy.type , data=exercise_data)
rlm = MASS::rlm(weight.loss~motivation, data=exercise_data)
rlm2 = MASS::rlm(weight.loss~motivation + therapy.type, data=exercise_data)


test_that("model_comparisons works", {
  expect_true(length(model.comparison(nlr, nlf))==2)
  expect_true(length(model.comparison(log_full, log_reduced))==3)
  expect_true(length(model.comparison(rf, rf2))==1)
  model.comparison(mixed_full, mixed_reduced) %>% 
    names() %>% 
    purrr::pluck(3) %>% 
    {if(.=="r_squared_change") TRUE else FALSE} %>% 
    expect_true()
})
test_that("model_comparison_table works", {
  # identical models
  expect_true(
    model_comparison_table(nlf, nlf, "A", "B", TRUE)$bayes.factor[1]==1)
  # nested linear models
  mc = model_comparison_table(nlf, nlr, "A", "B", TRUE)
  expect_equal(mc[1,1], 1220.523, tol=.001)
  # linear models non nested
  mc = model_comparison_table(nlf, nnl, "A", "B", FALSE)
  expect_equal(mc[2,1], 1250.756, tol=.001)
  
  # logistic, poisson, gamma, glmer, lmer
  a = model_comparison_table(log_full, log_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 1749.818, tol=.001)
  a = model_comparison_table(pois_full, pois_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 430.5225, tol=.001)
  a = model_comparison_table(gamma_full, gamma_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 1127.837, tol=.001)
  a = model_comparison_table(log_full, log_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 1749.818, tol=.001)
  
  # mixed models
  a = model_comparison_table(mixed_full, mixed_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 46605.88, tol=.001)
  
  # glm mixed 
  a = model_comparison_table(mixed_pois, mixed_pois_reduced, "A", "B", TRUE)
  expect_equal(a[1,1], 399.4922, tol=.001)
  
  # rf
  expect_null(model_comparison_table(rf, rf2, "A", "B", TRUE))
  
  # rlm
  a = model_comparison_table(rlm, rlm2, "A", "B", TRUE)
  expect_equal(a[1,1], 1238.581, tol=.001)
  a = model_comparison_table(rlm, nlf, "A", "B", TRUE)
  expect_equal(a[1,1], 1238.581, tol=.001)
})

test_that("get_p_value works", {
  
  # linear nested
  expect_true(get_p_value(nlf, nlr, T)<.0001)
  
  # glm mixed
  expect_equal(get_p_value(mixed_pois, mixed_pois_reduced), 000, tol=.001)
  
  # logistic glm
  full = glm(died~superpower + speed, data=avengers, family=binomial)
  reduced = glm(died~superpower, data=avengers, family=binomial)
  expect_true(get_p_value(full, reduced)<0.001)
})

test_that("get_r_squared works", {
  # linear models
  a = sum(get_r_squared(nlf, nlr))
  expect_equal(a, .341, tol=.001)
  b = sum(get_r_squared(nlr, nnl))
  expect_equal(b, .214, tol=.001)
  # other models
  c = sum(get_r_squared(mixed_full, mixed_reduced))
  expect_equal(c, .501, tol=.001)
  
})


test_that("standardized difference works", {

  diff = standardized_differences(nlr, nlf)
  expect_output(print(diff), "1.077")

  diff = standardized_differences(nlr, nnl)
  expect_output(print(diff), "1.767")
  
  diff = standardized_differences(log_full, log_reduced)
  expect_output(print(diff), "0.098")
  
  diff = standardized_differences(pois_full, pois_reduced)
  expect_output(print(diff), "0.059")
  
  diff = standardized_differences(gamma_full, gamma_reduced)
  expect_output(print(diff), "0.157")  
  
  diff = standardized_differences(mixed_full, mixed_reduced)
  expect_output(print(diff), "1.062") 
  
  diff = standardized_differences(mixed_pois, mixed_pois_reduced)
  expect_output(print(diff), "0.152")
  
  diff = standardized_differences(rf, rf2)
  expect_output(print(diff), "50%")  
  
  diff = standardized_differences(rlm, rlm2)
  expect_output(print(diff), "1.115")   
  
  diff = standardized_differences(rf, rlm2)
  expect_output(print(diff), "0%")   
  
  expect_error(standardized_differences(gamma_full, pois_reduced))
  
})

test_that("rlm and lm can be compared", {
  model1 = lm(weight.loss~motivation + therapy.type, data=exercise_data)
  model2 = MASS::rlm(weight.loss~motivation + therapy.type, data=exercise_data)
  results = model.comparison(model1, model2)
  expect_output(print(results), "0.000 0.004 0.012 0.183 0.194")
})

test_that("nested models have correct r squared", {
  results = model.comparison(nlr, nlf)$statistics
  expect_equal(results$rsq[2], .217)
})

test_that("nonnested models correctly report null for p value", {
  results = model.comparison(nlr, nnl)
  expect_false("p" %in% names(results$statistics))
})

test_that("glm vs lm works", {
  a = lm(health~motivation + gender, data=exercise_data)
  b = glm(health~motivation + gender, data=exercise_data, family=Gamma)		
  results = model.comparison(a,b)
  expect_equal(results$statistics$aic[1], 1382.206)
})


test_that("interaction vs lm works", {
  a = lm(weight.loss~motivation *therapy.type, data=exercise_data)
  b = lm(weight.loss~motivation+therapy.type, data=exercise_data)
  results = model.comparison(a,b)
  expect_equal(results$statistics$bayes.factor[1], .01)
})


test_that("random forest vs lm works returns predictions only", {
  set.seed(1212)
  a = randomForest::randomForest(weight.loss~therapy.type, data=exercise_data)
  b = lm(weight.loss~therapy.type, data=exercise_data)
  results = model.comparison(rf2, nlf)
  expect_equal(length(results), 1)
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
  expect_message(model.comparison(full, reduced))
})



test_that("compare_sensitivity_specificity works", {
  head(tablesaw.injury)
  model1 = glm(injury~safety + attention + gender, data=tablesaw.injury, family=binomial)
  model2 = glm(injury~attention, data=tablesaw.injury, family=binomial)
  s = compare_sensitivity_specificity(model1, model2, "model1", "model2")
  expect_equal(round(s[1,1]*100), 65)
  
  model2 = glm(probs~attention, data=tablesaw.injury)
  expect_null(compare_sensitivity_specificity(model1, model2, "model1", "model2"))
})

test_that("is_model_outcome_binary works", {
  
  model = glm(injury~safety + attention + gender, data=tablesaw.injury, family=binomial)
  expect_true(is_model_outcome_binary(model))
  model = glm(probs~safety + attention + gender, data=tablesaw.injury)
  expect_false(is_model_outcome_binary(model))
  
  # what happens when I remove a level but never refactor?
  mod = glm(gender~weight.loss, data=exercise_data %>% dplyr::filter(therapy.type!="control"), family=binomial)
  expect_true(is_model_outcome_binary(mod))
})

test_that("model_comparison p-values are right", {
  full = lm(weight.loss~therapy.type + muscle.gain, data=exercise_data)
  reduced = lm(weight.loss~therapy.type, data=exercise_data)
  a = model.comparison(full, reduced)$statistic$p[1]
  b = as.character(round(anova(full, reduced)$`Pr(>F)`[2], digits=3))
  expect_true(a==b)
})


test_that("rsq comparison doesn't error", {
  full    = lmer(MathAch~SES + Minority + (SES | School), data=math)
  reduced = lmer(MathAch~SES            + (SES | School), data=math)
  expect_true(names(model.comparison(full, reduced))[1] == "statistics")
  
})
options(warn=0)

















#