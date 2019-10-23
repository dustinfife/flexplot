# 
# ### detach all packages
# #invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
# #source("research/RPackages/flexplot/R/hidden_functions.R")
# 	#### BASE FLEXPLOT FUNCTIONS
# #devtools::document("research/RPackages/flexplot")
# #devtools::install("research/RPackages/flexplot")
# #devtools::check("research/RPackages/flexplot")
# require(flexplot)
# 
# 
# 	#### univariate plots
# data(exercise_data); d= exercise_data
# flexplot(income~1, data=d)
# flexplot(gender~1, data=d)
# 
# 	#### previous bugs
# data(birthweight)
# d = birthweight
# mod = lm(Birthweight~motherage + fheight + mheight + smoker, data=d)
# flexplot(Birthweight~mheight+smoker|motherage, data=d)
# 
# d = read.csv("research/Statistical Framework/grants/CSM Seed Grant/data/classroom_method.csv")
# d$instruction.method = relevel(d$instruction.method, ref="lecture")
# flexplot(test.score~funding|instruction.method, data=d, se=F, alpha=.05, ghost.reference=list(instruction.method="lecture"), ghost.line="gray")
# flexplot(test.score~instruction.method, data=d, sample=50)
# 
# require(flexplot)
# data(exercise_data)	
# d = exercise_data
# glinmod(weight.loss~gender + motivation, data=exercise_data, se=F, method="lm")
# model = lm(weight.loss~motivation+therapy.type, 
#            data=exercise_data)
# visualize(model)
# object = model
# 
# d = exercise_data
# 
# 		# # #### histograms and barcharts
# 
# 
# 
# # ### scatter plot
# require(MASS)
# flexplot(weight.loss~motivation, data=d)	
# flexplot(weight.loss~motivation, data=d, method="lm", se=FALSE)	
# flexplot(weight.loss~motivation, data=d, method="rlm", se=FALSE)	
# flexplot(weight.loss~motivation, data=d, method="rlm", se=FALSE, raw.data=F)	
# flexplot(weight.loss~health, data=d, method="polynomial", se=FALSE)	
# flexplot(weight.loss~health, data=d, method="cubic", se=FALSE)	
# flexplot(gender~health, data=d, method="logistic", se=FALSE, jitter=c(0, .1))	
# flexplot(weight.loss~therapy.type + gender, 
#     data=exercise_data, se=F, alpha=.3)
# 
# 
# data("tablesaw.injury") ### also simulated data available in flexplot package
#                         ### always remember to be safe and attentive when woodworking
# flexplot(injury~attention, data=tablesaw.injury, 
#              method="logistic", jitter=c(0, .05))
# 
# 
# # ### mean plots
# flexplot(weight.loss~therapy.type, data=d)
# data(relationship_satisfaction)
# flexplot(satisfaction~separated, data=relationship_satisfaction)
# flexplot(weight.loss~therapy.type, data=d, spread="stdev")
# flexplot(weight.loss~therapy.type, data=d, spread="sterr")
# flexplot(weight.loss~therapy.type, data=d, raw.data=FALSE)	
# flexplot(weight.loss~therapy.type, data=d, jitter=F)		
# flexplot(weight.loss~therapy.type, data=d, jitter=T)		
# flexplot(weight.loss~therapy.type, data=d, jitter=c(.3,0))		
# flexplot(weight.loss~therapy.type, data=d, jitter=c(.3))
# flexplot(weight.loss~therapy.type, data=d, jitter=c(.3, .5))	
# flexplot(gender~therapy.type, data=d, jitter=c(.3, .5), method="logistic")		
# 	### intentional error
# 
# 
# 
# ## related T
# k = d
# deleteme = which(k$rewards=="no rewards")
# k = k[-(deleteme[1:2]),]
# table(k$rewards)
# flexplot(weight.loss~rewards, data=k, related=T)
# flexplot(weight.loss~rewards, data=k, related=T, jitter=F)
# flexplot(weight.loss~rewards, data=k, related=T, jitter=T)
# flexplot(weight.loss~rewards, data=k, related=T, jitter=c(.05,0))
#  	## without raw data
# 
# # ### CHI SQUARE PLOT (categorical on categorical)
# flexplot(gender~rewards, data=d, jitter=c(.05,0))
# 
# 
# #options(warn=-1)
# 
# 
# # ### INTERACTION PLOT			
# flexplot(weight.loss~therapy.type + gender, data=d, alpha=.4, jitter=F)
# flexplot(weight.loss~therapy.type + gender, data=d, alpha=.4, jitter=F)
# flexplot(weight.loss~therapy.type + gender, data=d, alpha=.4, jitter=c(.4,0))
# flexplot(weight.loss~therapy.type + gender, data=d, sample=50)	
# flexplot(weight.loss~therapy.type | gender, data=d, sample=50)	
# flexplot(gender~weight.loss | therapy.type, data=d, sample=50, method="logistic")	
# flexplot(gender~weight.loss + therapy.type, data=d, sample=50, method="logistic")	
# 
# # #### ANCOVA PLOT
# flexplot(weight.loss~motivation + gender, data=d, se=FALSE)	### remove se
# added.plot(weight.loss~motivation + gender, data=d, se=FALSE)	### remove se
# 
# 
# 
# # #### 2N PLOT (2 NUMERIC VARIABLE PLOTS)
# flexplot(formula = weight.loss~motivation + income, data=d, se=FALSE, method="lm", breaks=NULL, bins=3, labels=NULL)
# #source("research/RPackages/flexplot/R/flexplot.R")
# #source("research/RPackages/flexplot/R/hidden_functions.R")
# flexplot(formula = weight.loss~motivation + income, data=d, se=FALSE, method="lm", 
#  	breaks = list(income=c(95000, 100000, 105000)),
#  	labels=list(income=c("<95K", "<100K", "<105K", ">105K")))		
# flexplot(formula = gender~motivation + income, data=d, se=FALSE, method="logistic", 
#  	breaks = list(income=c(95000, 100000, 105000)),
#  	labels=list(income=c("<95K", "<100K", "<105K", ">105K")))
# 
# # #### 3N plot
# flexplot(weight.loss~motivation + income + health, data=d, se=FALSE, method="lm")	
#  		## different lines for income
# flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", third.eye=c(T))	
# flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="polynomial", third.eye=c(T))	
#  		## different panels for income
# flexplot(formula = weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", 
#  	breaks = list(income = c(95000, 100000, 105000)),
#  	labels=list(income = c("<95K", "<100K", "<105K", ">105K")))	
# 
# 
# #### ghost lines
# flexplot(weight.loss~motivation + gender | satisfaction + health, 
#          data=exercise_data, 
#          method="lm", se=F, bins=2, ghost.line="black", alpha=.1,
#          ghost.reference = list(c("satisfaction"=0, "health"=10)))
#            
#          
# flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red",
# 	breaks = list(income = c(95000, 100000, 105000)),
#  	labels=list(income = c("<95K", "<100K", "<105K", ">105K")))	
# flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31, "income"=90000))	
# flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31))
# flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31, "income"=90000))	
# flexplot(weight.loss~motivation + gender | income + health, data=d, se=FALSE, method="lm", ghost.line="gray", ghost.reference=list("health"=31, "income"=90000, gender="female"))	
# flexplot(weight.loss~motivation + gender | income + health, data=d, se=FALSE, method="lm", ghost.line="gray", ghost.reference=list("health"=31, "income"=90000))	
# 
# 
# 		#### VISUALIZE FUNCTIONS -- Linear Models
# ## t-test
# mod = lm(weight.loss~rewards, data=d)
# visualize(mod)
# object = mod
# ### regression
# mod = lm(weight.loss~motivation, data=d)
# visualize(mod)
# visualize(mod, plot="residuals")
# visualize(mod, plot="model")
# 
# ### ancova
# mod = lm(weight.loss~motivation + rewards, data=d)
# visualize(mod)
# visualize(mod, plot="residuals")
# visualize(mod, plot="model")
# 
# model.me = lm(weight.loss ~ motivation+therapy.type, data = exercise_data)
# model.int = lm(weight.loss ~ motivation*therapy.type, data = exercise_data)
# compare.fits(weight.loss ~ motivation | therapy.type, 
#              data = exercise_data, model.me, model.int, ghost.line = "black", return.preds=F)             
# 
# 
# ### factorial anova
# mod = lm(weight.loss~gender + rewards, data=d)
# visualize(mod)
# visualize(mod, plot="residuals")
# visualize(mod, plot="model")
# 
# ### multiple regression
# mod = lm(weight.loss~gender + rewards + motivation, data=d)
# visualize(mod)
# visualize(mod, plot="residuals")
# visualize(mod, plot="model")
# 
# data(birthweight)
# k= birthweight
# added.plot(Birthweight~mheight + fheight + motherage + smoker, data=k, method="lm")
# mod = lm(Birthweight~mheight + fheight + motherage, data=k)
# mod2 = lm(Birthweight~mheight + fheight + motherage, data=k)
# visualize(mod, plot="residuals")
# 
# 	#### mixed models
# require(flexplot)
# require(lme4)
# data(math)
# model = lmer(MathAch~ SES + Sex + (SES|School), data=math)
# flexplot::visualize(model, formula = MathAch~ SES | Sex + School, plot="model")
# visualize(model, formula = MathAch~ SES + School| Sex, sample=100)
# object = model
# visualize(model, formula = MathAch~ Sex | SES+ School, sample=3)
# visualize(model, formula = MathAch~ Sex + School| SES, sample=30)
# 
# d = exercise_data
# 	#### COMPARE.FITS FUNCTIONS -- linear models
# mod = lm(weight.loss~rewards, data=d)
# require(MASS)
# mod2 = rlm(weight.loss~rewards, data=d)
# compare.fits(weight.loss~rewards, data=d, model1=mod)
# formula = weight.loss~rewards; data=d; model1=mod; model2=NULL
# compare.fits(formula=weight.loss~rewards, data=d, model1=mod, model2=mod2)
# 
# mod = lm(weight.loss~motivation, data=d)
# compare.fits(weight.loss~motivation, data=d, model1=mod)
# 
# 	### compare interaction and non-interaction models
# d$wl = d$weight.loss + .8*d$motivation*as.numeric(d$rewards)
# mod = lm(wl ~motivation+rewards, data=d)
# mod2 = lm(wl ~motivation*rewards, data=d)
# compare.fits(wl~motivation|rewards, data=d, model1=mod, model2=mod2)
# 
# 	### compare interaction and non-interaction models
# d$wl = d$weight.loss + .8*d$motivation*as.numeric(d$rewards)
# mod = lm(wl ~gender+rewards, data=d)
# mod2 = lm(wl ~gender*rewards, data=d)
# compare.fits(wl~gender|rewards, data=d, model1=mod, model2=mod2)
# 
# 		##### compare predictions with random forest
# require(randomForest)
# model1 = randomForest(wl~motivation + gender + rewards, data=d)
# model2 = lm(wl~motivation * gender * rewards, data=d)		### use the same predictors in both models
# compare.fits(wl~motivation | gender + rewards, data=d, model1, model2)
# 
# 		##### predictions with generalize lidnear model
# d$weight.loss = d$weight.loss + 1 + abs(min(d$weight.loss, na.rm=T))
# mod1 = glm(weight.loss~motivation + health + gender, data=d, family="Gamma")
# mod2 = lm(weight.loss~motivation + health + gender, data=d)
# compare.fits(weight.loss~motivation | health + gender, data=d, mod1, mod2)
# compare.fits(weight.loss ~ motivation | health, data=d, model1=mod1, ghost.line="blue")
# 
# 
# 
# compare.fits(weight.loss ~ motivation | health, data=d, model1=mod1, model2=mod2, ghost.line="red")
# 
# 
# mod1 = lm(weight.loss~therapy.type * motivation * health * muscle.gain * I(motivation^2), data=d)
# mod2 = lm(weight.loss~therapy.type + motivation + health + muscle.gain + I(motivation^2), data=d)
# compare.fits(weight.loss ~muscle.gain | motivation + health, data=d, model1=mod1, model2=mod2)
# compare.fits(weight.loss ~muscle.gain | motivation + health, data=d, model1=mod1, model2=mod2)
# compare.fits(weight.loss ~muscle.gain +therapy.type | motivation + health, data=d, model1=mod1)
# flexplot::third.eye(weight.loss~muscle.gain|motivation + health, data=d, which.perms=1:2, se=F, ghost.line="gray", method="lm")
# 
# require(fifer)
# data(authors); d= authors[1:1000,]
# mod1 = lm(Daily.Units.Sold~Sale.Price*Publisher, data=d)
# mod2 = glm(Daily.Units.Sold~Sale.Price*Publisher, data=d, family=quasipoisson(link="log"))
# compare.fits(Daily.Units.Sold~Sale.Price|Publisher, data=d, mod1, mod2)
# 
# 
# 
# 
