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
]