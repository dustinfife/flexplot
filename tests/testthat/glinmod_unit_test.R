# rm(list=ls())
# 
# ### detach all packages
# invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
# 
# 	#### BASE FLEXPLOT FUNCTIONS
# #devtools::document("research/RPackages/flexplot")
# #devtools::install("research/RPackages/flexplot")
# require(flexplot)
# data(exercise_data)	
# d = exercise_data
# 
# 	### buggy stuff
# glinmod(weight.loss~gender, data=exercise_data)
# 		##### one categorical
# cat1 = lm(weight.loss~therapy.type, data=d)	
# estimates(cat1)
# 
# compare.fits(weight.loss~therapy.type, data=d, model1=cat1, report.se=T, return.preds=T)
# formula = weight.loss~therapy.type
# 		##### two categoricals
# mod = lm(weight.loss~therapy.type + gender, data=d)	
# estimates(mod)
# compare.fits(weight.loss~therapy.type |gender, data=d, model1=mod)
# 
# 		##### interaction
# mod = lm(weight.loss~therapy.type * gender, data=d)	
# estimates(mod)
# compare.fits(weight.loss~therapy.type | gender, data=d, model1=mod)
# 
# 
# 		#### numeric variables
# cat1 = lm(weight.loss~motivation, data=d)	
# estimates(cat1)
# compare.fits(weight.loss~motivation, data=d, model1=cat1)
# 
# 		##### two categoricals
# mod = lm(weight.loss~motivation + income, data=d)	
# estimates(mod)
# compare.fits(weight.loss~motivation | income, data=d, model1=mod)
# 
# 		##### interaction
# mod = lm(weight.loss~motivation * income, data=d)	
# estimates(mod)
# compare.fits(weight.loss~ motivation | income, data=d, model1=mod)		
# 
# 
# 		##### both categorical and numeric
# mod = lm(weight.loss~motivation * therapy.type, data=d)	
# estimates(mod)
# compare.fits(weight.loss~ motivation | therapy.type, data=d, model1=mod)	
# 
# 
# 		##### two categorical and one numeric
# mod = lm(weight.loss~motivation + therapy.type + gender, data=d)	
# estimates(mod)
# compare.fits(weight.loss~ motivation | therapy.type + gender, data=d, model1=mod)		
# 
# 		##### two numeric and one categorical
# mod = lm(weight.loss~motivation + income + gender, data=d)	
# estimates(mod)
# compare.fits(weight.loss~ motivation | income + gender, data=d, model1=mod)		
# 
# 		##### polynomial
# mod = lm(weight.loss~motivation + I(motivation^2), data=d)	
# estimates(mod)
# compare.fits(weight.loss~ motivation, data=d, model1=mod)		
# 
# 		#### previous "bugs"
# #mod = lm(satisfaction~interests + separated, data = d)
# #estimates(mod)		
# #compare.fits(satisfaction~interests + separated, data=d, model1=mod)		
# 
# 


data(exercise_data)
d = exercise_data
 ##### one categorical
 require(testthat)
  cat1 = lm(weight.loss~therapy.type, data=d)	
  expect_equal(estimates(cat1)$difference.matrix$cohens.d[1], .6305667, tolerance = 0.002)

  
  ##### two categoricals
  mod = lm(weight.loss~therapy.type + gender, data=d)	
  expect_equal(estimates(mod)$difference.matrix$cohens.d[4], -.1024, tolerance = 0.002)
  
  ##### interaction
  mod = lm(weight.loss~therapy.type * gender, data=d)	
  expect_true(estimates(mod)$semi.p<0.01 & estimates(mod)$semi.p>0.009)
  
  #### numeric variables
  cat1 = lm(weight.loss~motivation, data=d)	
  expect_equal(estimates(cat1)$numbers.summary$estimate[2], .1856, tolerance = 0.002)
  
  ##### categorical + numeric
  mod = lm(weight.loss~motivation + income, data=d)	
  expect_equal(estimates(mod)$numbers.summary$std.upper[3], -.1898, tolerance = 0.002)
  
  ##### two categorical and one numeric
  mod = lm(weight.loss~motivation + therapy.type + gender, data=d)	
  expect_equal(estimates(mod)$difference.matrix$difference[4], -.93229, tolerance = 0.002)
  
  ##### two numeric and one categorical
  mod = lm(weight.loss~motivation + income + gender, data=d)
  expect_true(estimates(mod)$semi.p[3]<0.01 & estimates(mod)$semi.p[3]>0.007)	

  ##### polynomial
  mod = lm(weight.loss~motivation + I(motivation^2), data=d)	
  expect_equal(estimates(mod)$numbers.summary$std.upper[3], -.53, tolerance = 0.002)
