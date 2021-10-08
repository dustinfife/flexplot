# data on hulk destruction
# bp_mean, length_transformation, sleep, meditation, cloudy, location (cage, public, home, )
cor.mat = matrix(c(
  1, .5, -.3, -.3, -.1, -.6,
  .5, 1, -.2, -.5, -.15, -.4,
  -.3, -.2, 1, .4, .1, -.3,
  -.3, -.5, .4, 1, 0, .3,
  -.1, -.15, .1, 0, 1, 0,
  -.6, -.4, -.3, .3, 0, 1
), nrow=6)
require(tidyverse)
  # set up main effects 
d = data.frame(MASS::mvrnorm(2667, cor.mat, mu=c(0,0,0,0,0, 0))) %>% 
  set_names(c("bp_mean", "length_transformation", "sleep", "meditation", "cloudy", "location"))
d$deaths = exp(1.1*d$bp_mean + 1.1*d$length_transformation - 1.05*d$sleep - 1.2*d$meditation -1.8*d$location) 

# convert variables
d = d %>% 
  mutate(location = cut(location, c(-Inf, -.5, 0, Inf), 
                        labels=c("home", "public", "cage")),
         cloudy = cut(cloudy, c(-Inf, .5, Inf),
                      labels = c("sunny", "cloudy")))

# set up interaction effects
# vs sunny home
head(model.matrix(length_transformation~cloudy*location, data=d))
d$length_transformation = d$length_transformation + c(matrix(c(5, 5, 6, 15, 10, 6)*.25, nrow=1) %*% 
                                                        t(model.matrix(length_transformation~cloudy*location, data=d)))
flexplot(length_transformation~cloudy|location, data=d)

?model.matrix
#d$length_transformation = d$length_transformation +
  flexplot(length_transformation~bp_mean + meditation, data=d)

d$length_transformation = d$length_transformation + c(matrix(c(0, 1, 3.5, -1.5), nrow=1) %*% 
  t(model.matrix(length_transformation~cloudy*location, data=d)) )

flexplot(length_transformation~cloudy|location, data=d)
head(model.matrix(length_transformation~cloudy*location, data=d))
# vs. sunny at home



model.matrix(length_transformation~cloudy*location, data=d, contrasts=list(cloudy="contr.treatment", location="contr.treatment"))









head(model.matrix(length_transformation~cloudy*location, data=d))
flexplot(deaths~meditation, data=d)
plot(1:10, exp(.8*1:10))


dim(model.matrix(length_transformation~cloudy*location, data=d) )

matrix(c(0, 1, 1, .5), nrow=1) %*% t(model.matrix(length_transformation~cloudy*location, data=d))
