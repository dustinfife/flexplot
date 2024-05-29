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


test_that("missing data in a variable doesn't screw things up", {
  d = avengers
  d$willpower[1:10] = NA
  full = lm(ptsd~superpower + willpower, data=d)
  reduced = lm(ptsd~superpower, data=d)
  expect_message(model.comparison(full, reduced))
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