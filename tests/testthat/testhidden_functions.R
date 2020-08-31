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
  
  tst = as.character(make_flexplot_formula(predictors = "Years", outcome, data))[3]
  expect_output(print(tst),'Years')

  tst = as.character(make_flexplot_formula(predictors = c("Years", "GPA", "gender:GPA"), outcome, data))[3]
  expect_output(print(tst),'Years \\| GPA')
})

test_that("match.jitter works", {
  expect_equal(match_jitter_categorical(.2), c(.2, 0))
  expect_equal(match_jitter_categorical(.1), c(.1, 0))
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

test_that("sample.subset returns a dataset the right rows", {
  set.seed(232)
  d = exercise_data
  expect_equal(nrow(sample.subset(10, d)), 10)
  expect_equal(nrow(sample.subset(Inf, d)), nrow(d))
})

test_that("points.func works",{
  expect_output(print(points.func(axis.var="therapy.type", data=exercise_data, jitter=NULL)), 
                '\\[1\\] "geom_jitterd\\(data=sample\\.subset\\(sample, exercise_data\\), alpha=raw\\.alph\\.func\\(raw\\.data, alpha=alpha\\), width=0\\.2, height=0\\)"')
  expect_output(print(points.func("therapy.type", exercise_data, T)),
                '\\[1\\] "geom_jitterd\\(data=sample\\.subset\\(sample, exercise_data\\), alpha=raw\\.alph\\.func\\(raw\\.data, alpha=alpha\\), width=0\\.2, height=0\\)"')
  expect_output(print(points.func("therapy.type", exercise_data, F)),
                '\\[1\\] "geom_jitterd\\(data=sample\\.subset\\(sample, exercise_data\\), alpha=raw\\.alph\\.func\\(raw\\.data, alpha=alpha\\), width=0, height=0\\)"')
  expect_output(print(points.func("motivation", exercise_data, NULL)),
                '\\[1\\] "geom_jitterd\\(data=sample\\.subset\\(sample, exercise_data\\), alpha=raw\\.alph\\.func\\(raw\\.data, alpha=alpha\\), width=0, height=0\\)"')
  expect_output(print(points.func(c("motivation", "therapy.type"), exercise_data, NULL)),
                '\\[1\\] "geom_jitterd\\(data=sample\\.subset\\(sample, exercise_data\\), alpha=raw\\.alph\\.func\\(raw\\.data, alpha=alpha\\), width=0, height=0\\)"')  
  expect_output(print(points.func(c("gender", "therapy.type"), exercise_data, NULL)),
                '\\[1\\] "geom_point\\(data=sample\\.subset\\(sample, exercise_data\\), alpha=raw\\.alph\\.func\\(raw\\.data, alpha=alpha\\), position=position_jitterdodged\\(jitter\\.width=0\\.2, jitter\\.height=0, dodge\\.width=\\.5\\)\\)"')  
  expect_output(print(points.func(c("gender", "therapy.type"), exercise_data, c(.2, .1))),
                '\\[1\\] "geom_point\\(data=sample\\.subset\\(sample, exercise_data\\), alpha=raw\\.alph\\.func\\(raw\\.data, alpha=alpha\\), position=position_jitterdodged\\(jitter\\.width=0\\.2, jitter\\.height=0\\.1, dodge\\.width=\\.5\\)\\)"')    
})

test_that("factor.to.logistic works", {
  expect_error(factor.to.logistic(exercise_data, "therapy.type"))
  expect_equal(length(levels(factor.to.logistic(exercise_data, "gender", T))), 2)
  expect_equal(levels(factor.to.logistic(exercise_data, "gender")$gender), NULL)  
})

test_that("fit.function works for numeric predictors", {
  expect_identical(fit.function("motivation", "weight.loss", data=exercise_data, suppress_smooth=T), "xxxx")
  expect_error(fit.function("motivation", "weight.loss", method="logistic", data=exercise_data))
  expect_identical(fit.function("gender", "weight.loss", method="logistic", data=exercise_data),
    "geom_smooth(method = \"glm\", method.args = list(family = \"binomial\"), se = se)")
  expect_identical(fit.function("motivation", "weight.loss", method="rlm", data=exercise_data),
                   "geom_smooth(method = \"rlm\", se = se)")
  expect_identical(fit.function("motivation", "weight.loss", method="lm", data=exercise_data),
                   "stat_smooth(method=\"lm\", se=se)")  
  expect_identical(fit.function("motivation", "weight.loss", method="cubic", data=exercise_data),
                   "stat_smooth(method=\"lm\", se=se, formula=y ~ poly(x, 3, raw=TRUE))") 
  expect_identical(fit.function("motivation", "weight.loss", method="quadratic", data=exercise_data),
                   "stat_smooth(method=\"lm\", se=se, formula=y ~ poly(x, 2, raw=TRUE))")   
  expect_identical(fit.function("motivation", "weight.loss", method="loess", data=exercise_data),
                   "geom_smooth(method=\"loess\", se=se)")     
  
})


test_that("fit.function works for categorical predictors", {
  expect_identical(fit.function("weight.loss", "gender", data=exercise_data, suppress_smooth=T), "xxxx+xxxx+xxxx")
  expect_output(print(fit.function("weight.loss", "gender", data=exercise_data, method="stdev")), "\\+xxxx")
  expect_identical(fit.function("weight.loss", "gender", data=exercise_data, mean.line=T), 
                   "stat_summary(fun='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.max = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, position=position_dodge(width=.2), color = '#bf0303')+stat_summary(aes_string(group= axis[2]), geom=\"line\", fun=\"mean\", position=position_dodge(width=.2), color = \"#bf0303\")")
  expect_identical(fit.function("weight.loss", "gender", data=exercise_data, mean.line=F), 
                   "stat_summary(fun='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.max = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, position=position_dodge(width=.2), color = '#bf0303')+xxxx")  
  expect_identical(fit.function("weight.loss", "gender", spread="sterr", data=exercise_data, mean.line=T), 
                   "stat_summary(fun='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.max = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, position=position_dodge(width=.2), color = '#bf0303')+stat_summary(aes_string(group= axis[2]), geom=\"line\", fun=\"mean\", position=position_dodge(width=.2), color = \"#bf0303\")")
  expect_identical(fit.function("weight.loss", "gender", spread="sterr", data=exercise_data, mean.line=F), 
                   "stat_summary(fun='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){mean(z)-1.96*(sd(z)/sqrt(length(z)-1))}, fun.max = function(z){mean(z)+1.96*(sd(z)/sqrt(length(z)-1))}, width=.2, size = 1.25, position=position_dodge(width=.2), color = '#bf0303')+xxxx")
  expect_identical(fit.function("weight.loss", "gender", spread = "quartiles", data=exercise_data, mean.line=F), 
                   "stat_summary(fun='median', geom='point', size=3, position=position_dodge(width=.4), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){quantile(z, .25)},size = 1.25,  fun.max = function(z) {quantile(z, .75)}, fun=median, width=.2, position=position_dodge(width=.4), color = '#bf0303')+xxxx")
  expect_identical(fit.function("weight.loss", "gender", spread = "quartiles", data=exercise_data, mean.line=T), 
                   "stat_summary(fun='median', geom='point', size=3, position=position_dodge(width=.4), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){quantile(z, .25)},size = 1.25,  fun.max = function(z) {quantile(z, .75)}, fun=median, width=.2, position=position_dodge(width=.4), color = '#bf0303')+stat_summary(aes_string(group=axis[2]), geom=\"line\", fun=\"median\", position=position_dodge(width=.4), color = \"#bf0303\")")  
  expect_identical(fit.function("weight.loss", "gender", spread = "stdev", data=exercise_data, mean.line=F), 
                   "stat_summary(fun='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){mean(z)-sd(z)}, fun.max = function(z) {mean(z)+sd(z)}, fun=median, size = 1.25, width=.2, position=position_dodge(width=.5), color = '#bf0303')+xxxx")      
  expect_identical(fit.function("weight.loss", "gender", spread = "stdev", data=exercise_data, mean.line=T), 
                   "stat_summary(fun='mean', geom='point', size=3, position=position_dodge(width=.5), color = '#bf0303')+stat_summary(geom='errorbar', fun.min = function(z){mean(z)-sd(z)}, fun.max = function(z) {mean(z)+sd(z)}, fun=median, size = 1.25, width=.2, position=position_dodge(width=.5), color = '#bf0303')+stat_summary(aes_string(group= axis[2]), geom=\"line\", fun=\"mean\", position=position_dodge(width=.5), color = \"#bf0303\")")        
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












#