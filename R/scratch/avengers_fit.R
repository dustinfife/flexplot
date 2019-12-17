data(avengers)
head(avengers)
require(flexplot)
require(tidyverse)

  ### poisson
mod1 = glm(injuries~superpower + damage.resistence, data=avengers, family=quasipoisson)
mod2 = MASS::glm.nb(injuries~superpower + damage.resistence, data=avengers)
mod3 = MASS::polr(injuries~superpower + damage.resistence, data=avengers %>% mutate(injuries = factor(injuries)))
mod4 = randomForest::randomForest(injuries~superpower + damage.resistence, data=avengers)
compare.fits(injuries~damage.resistence | superpower, data=avengers, mod3, mod2, ghost.line="gray", jitter=c(0, .5))
compare.fits(injuries~damage.resistence | superpower, data=avengers, mod1, mod4, ghost.line="gray", jitter=c(0, .75))


mod1 = glm(kills~superpower + agility, data=avengers, family=quasipoisson)
mod2 = MASS::glm.nb(kills~superpower + agility, data=avengers)
mod3 = randomForest::randomForest(kills~superpower + agility, data=avengers)
compare.fits(kills~agility | superpower, data=avengers, mod1, mod2, ghost.line="gray", jitter=c(0, .5)) + coord_cartesian(ylim=c(0,50))
compare.fits(kills~agility | superpower, data=avengers, mod1, mod4, ghost.line="gray", jitter=c(0, .75)) + scale_y_continuous(trans='log')

  ### gamma
# flexplot(minutes.fighting~1, data=avengers)
# flexplot(strength~1, data=avengers)
# flexplot(damage.resistence~1, data=avengers)
# flexplot(minutes.fighting~strength | damage.resistence, data=avengers)
mod1 = glm(minutes.fighting~strength + superpower , data=avengers, family=Gamma)
mod2 = glm(minutes.fighting~strength * superpower, data=avengers, family=Gamma)
compare.fits(minutes.fighting~strength | superpower, data=avengers, mod1, mod2)
model.comparison(mod1, mod2)

  ### normal
mod1 = lm(shots.taken~speed+superpower, data=avengers)
mod2 = lm(shots.taken~speed+superpower + speed:superpower, data=avengers)
compare.fits(shots.taken~speed|superpower, data=avengers, mod1, mod2)
model.comparison(mod1, mod2)
flexplot(kills~agility, data=avengers)
flexplot(speed~agility, data=avengers)
flexplot(agility~strength, data=avengers)

  ### logistic
require(fifer)
mod1 = glm(died~agility + strength + superpower, data=avengers, family=binomial)
mod2 = glm(died~agility * strength * superpower, data=avengers, family=binomial)
compare.fits(died~agility | strength + superpower, data=avengers, mod1, mod2)
visualize(mod1, method="logistic", plot="model")


flexplot(died~agility | strength, data=avengers, method="logistic")
# th = rfThresh(died~iq+agility+speed+strength+damage.resistence+superpower+flexibility+willpower, 
#               data=avengers, nruns = 10, importance = "gini")
# prd = rfInterp(object = th, importance = "gini", nfor.pred = 10)
# int = rfPred(prd, importance="gini")

flexplot(died~strength | damage.resistence, data=avengers, method="logistic")
