require(testthat)
require(vdiffr)
context("Univariate plots")

require(flexplot)
data(exercise_data)
d = exercise_data

#### univariate plots
histcont = flexplot(income~1, data=d)
histcat = flexplot(gender~1, data=d)
vdiffr::expect_doppelganger("histogram", histcont)
vdiffr::expect_doppelganger("barchart", histcat)


# ### scatter plots
require(MASS)
set.seed(1212)
vdiffr::expect_doppelganger("scatter", flexplot(weight.loss~motivation, data=d))	
vdiffr::expect_doppelganger("scatter lm no se", flexplot(weight.loss~motivation, data=d,method="lm", se=FALSE))	
vdiffr::expect_doppelganger("scatter rlm no se no raw", flexplot(weight.loss~motivation, data=d,method="rlm", se=FALSE, raw.data=F))	
vdiffr::expect_doppelganger("scatter poly", flexplot(weight.loss~motivation, data=d,method="polynomial"))	
vdiffr::expect_doppelganger("scatter cubic", flexplot(weight.loss~motivation, data=d,method="cubic"))	
vdiffr::expect_doppelganger("scatter logistic jittery", flexplot(gender~health, data=d, se=FALSE, method="logistic", jitter=c(0, .1)))	

# ### mean plots
vdiffr::expect_doppelganger("mean plot", flexplot(weight.loss~therapy.type, data=d))
vdiffr::expect_doppelganger("mean stdev", flexplot(weight.loss~therapy.type, data=d, spread="stdev"))
vdiffr::expect_doppelganger("mean sterr", flexplot(weight.loss~therapy.type, data=d, spread="sterr"))
vdiffr::expect_doppelganger("mean no raw", flexplot(weight.loss~therapy.type, data=d, raw.data=FALSE)	)
vdiffr::expect_doppelganger("mean no jitter", flexplot(weight.loss~therapy.type, data=d, jitter=F)		)
vdiffr::expect_doppelganger("mean jitter true", flexplot(weight.loss~therapy.type, data=d, jitter=T)		)
vdiffr::expect_doppelganger("mean xjitter", flexplot(weight.loss~therapy.type, data=d, jitter=c(.3,0))	)	
vdiffr::expect_doppelganger("mean one jitter", flexplot(weight.loss~therapy.type, data=d, jitter=c(.3)))
vdiffr::expect_doppelganger("mean both jitter", flexplot(weight.loss~therapy.type, data=d, jitter=c(.3, .5))	)
#vdiffr::expect_doppelganger("flexplot(gender~therapy.type, data=d, jitter=c(.3, .5), method="logistic")		)
	### intentional error


## unconventional plots
k = d
deleteme = which(k$rewards=="no rewards")
k = k[-(deleteme[1:2]),]
vdiffr::expect_doppelganger("related T", flexplot(weight.loss~rewards, data=k, related=T))
vdiffr::expect_doppelganger("association plot", flexplot(gender~rewards, data=d, jitter=c(.05,0)))
vdiffr::expect_doppelganger("interaction plot", flexplot(weight.loss~therapy.type + gender, data=d, alpha=.4))
vdiffr::expect_doppelganger("interaction plot panel", flexplot(weight.loss~therapy.type | gender, data=d, sample=50))
vdiffr::expect_doppelganger("panelled logistic with sampling", flexplot(gender~weight.loss | therapy.type, data=d, sample=50, method="logistic"))	

	

### ghost lines
a=flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red")	   
expect_doppelganger("just ghost line", a)         
b = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red",
	breaks = list(income = c(95000, 100000, 105000)),
 	labels=list(income = c("<95K", "<100K", "<105K", ">105K")))	
expect_doppelganger("ghost with breaks/labels", b)          	
c = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31, "income"=90000))	
expect_doppelganger("ghost with choosing panel", c)          	
e = flexplot(weight.loss~motivation | income + health, data=d, se=FALSE, method="lm", ghost.line="red", ghost.reference=list("health"=31))
expect_doppelganger("ghost with choosing ONE panel", e)          	
f = flexplot(weight.loss~motivation + gender | income + health, data=d, se=FALSE, method="lm", ghost.line="gray", ghost.reference=list("health"=31, "income"=90000, gender="female"))	
expect_doppelganger("ghost with choosing second slot", f)          	
g = flexplot(weight.loss~motivation + gender | income + health, data=d, se=FALSE, method="lm", ghost.line="gray", ghost.reference=list("health"=31, "income"=90000))		
expect_doppelganger("ghost with using both second slots", g)   
h = flexplot(gender~motivation | income + health, data=d, se=FALSE, method="logistic", ghost.line="gray")		
expect_doppelganger("ghost with logistic regression", h)
