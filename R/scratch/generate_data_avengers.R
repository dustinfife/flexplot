    ### avengers dataset
require(tidyverse)
require(dplyr)
require(flexplot)


# specify predictor variables ---------------------------------------------

  ### correlation matrix of abilities:
  # intelligence, agility, speed, strength,
  # damage resistence, superpower (yes/no), flexibility, willpower
  # agility/speed and strength need u-shaped relationship
rho = matrix(c(
  1,  0,    0,  0,   0,  0,  0, .2,       # iq
  0,  1,   .4, .4, -.2, .6, .5, .1, # agility
  0, .4,   1, .4, -.1,  .6, .4, 0,   # speed
  0, .4,  .4,  1,  .6,  .8, -.2, .2, # strength
  0, -.2, -.1, .6,  1,  .8, -.3, .3, # damage resistence
  0, .6,   .6, .8, .8,   1,  0,  .1, # superpower
  0, .5,   .4, -.2,-.3,  0,  1,   0, # flexibility
  .2, .1,   0,  .2, .3, .1,  0,    1 # willpower
  ), nrow = 8)
  #iq ag  spd str dam sup flex will
det(rho)
rho = Matrix::nearPD(rho)$mat

  ### simulate data
d = MASS::mvrnorm(n=812, mu = rep(0, times=8), Sigma = rho) %>%
  data.frame %>%
  setNames(c("iq", "agility", "speed", "strength", "damage.resistence", "superpower", "flexibility", "willpower"))
d$speed = d$speed + -.2*d$strength^2
d$agility = d$agility + -.25*d$strength^2
  # flexplot(agility~strength, data=d)



# specify outcome variables -----------------------------------------------

  # died (logistic), kills (negative binomial), injuries (ordered logistic?),
  # minutes_fighting (gamma), shots taken (normal)

rho2 = matrix(c(
  1, -.3, .5, -.4, -.2,   # died
  -.3, 1, -.2, .5, .5,    # kills
  .5, -.2,  1, .4, .2,    # injuries
  -.4, .5, .4,  1, .5,    # minutes_fighting
  -.2, .5, .2, .5, 1      # shots taken
  ), nrow = 5)

d2 = MASS::mvrnorm(n=nrow(d), mu = rep(0, times=5), Sigma = rho2) %>%
  data.frame %>%
  setNames(c("died", "kills", "injuries", "minutes.fighting", "shots.taken"))


# link the two datasets ---------------------------------------------------

died_link = matrix(c(.1, .3, .2, .2, .4, .5, .1, .4)*.25, ncol=1)
d2$died = d2$died + as.numeric(t(died_link) %*% t(data.matrix(d)))

kills_link = matrix(c(.2, .2, .3, .4, .1, .6, .1, .3), ncol=1)
d2$kills = d2$kills + as.numeric(t(kills_link) %*% t(data.matrix(d)))

injuries_link = matrix(c(.1, .4, -.1, .2, .5, .5, .1, .1), ncol=1)
d2$injuries = d2$injuries + as.numeric(t(injuries_link) %*% t(data.matrix(d)))

minutes_link = matrix(c(-.3, .1, .1, .3, .3, .5, .1, .6), ncol=1)
d2$minutes.fighting = d2$minutes.fighting + as.numeric(t(minutes_link) %*% t(data.matrix(d)))

shots_link = matrix(c(0, .1, -.3, .1, 0, -1.85, 0, 0), ncol=1)
d2$shots.taken = d2$shots.taken + as.numeric(t(shots_link) %*% t(data.matrix(d)))


# add interactions --------------------------------------------------------

d2$died = d2$died + -.2*d$superpower*d$strength
d2$shots.taken = d2$shots.taken + .75*d$superpower*d$speed
d2$minutes.fighting = d2$minutes.fighting + -.5*d$superpower*d$damage.resistence

# combine data
d = cbind(d, d2)


# dichotomize variables ---------------------------------------------------

d$superpower = cut(d$superpower, breaks=c(-Inf, 1, Inf), labels=c("no", "yes"))
d$died = cut(d$died, breaks=c(-Inf, -1.5, Inf), labels=c("yes", "no"))
d$kills = exp(d$kills)
d$injuries = as.numeric(cut(-1*d$injuries, breaks=c(-Inf, -4, -3, -1, 0, 1, Inf), labels=c(1:6), ordered=T))
d$injuries = d$injuries - 1
d$minutes.fighting = d$minutes.fighting + 10 + exp(d$minutes.fighting)
d$minutes.fighting[d$minutes.fighting<1] = 5

# invert relationships (where applicable) ---------------------------------
# flexplot(died~agility, data=d, method="logistic")
# flexplot(died~speed, data=d, method="logistic")
# flexplot(died~strength, data=d, method="logistic")
# flexplot(died~damage.resistence, data=d, method="logistic")
# flexplot(died~flexibility, data=d, method="logistic")
# flexplot(died~willpower, data=d, method="logistic")
#
# flexplot(superpower~agility, data=d, method="logistic")
# flexplot(superpower~speed, data=d, method="logistic")
# flexplot(superpower~strength, data=d, method="logistic")
# flexplot(superpower~damage.resistence, data=d, method="logistic")
# flexplot(superpower~flexibility, data=d, method="logistic")
# flexplot(superpower~willpower, data=d, method="logistic")
#
# flexplot(kills~agility, data=d)
# flexplot(kills~speed, data=d)
# flexplot(kills~strength, data=d)
# flexplot(kills~damage.resistence, data=d)
# flexplot(kills~flexibility, data=d)
# flexplot(kills~willpower, data=d)
#
# flexplot(agility~injuries, data=d)
# flexplot(speed~injuries, data=d)
# flexplot(strength~injuries, data=d)
# flexplot(damage.resistence~injuries, data=d)
# flexplot(flexibility~injuries, data=d)
plot(injuries~agility, data=d)
d$injuries

# put variables on correct scale ------------------------------------------
require(fifer)
d = d %>% mutate(iq = rescale(iq, 110, 8),
             agility = rescale(agility, 50, 15),
             speed = rescale(speed, 5, .17),              # meter dash
             strength = rescale(strength, 500, 150),      # bench press mas
             damage.resistence = rescale(agility, 2, .21),
             flexibility = rescale(flexibility, -1, 1),   # inches beyond shoes
             willpower = rescale(willpower, 60, 18),
             shots.taken = rescale(shots.taken, 130, 30)) %>%
        mutate_at(vars(speed,damage.resistence),
                  funs(round(., 2))) %>%
        mutate_at(vars(flexibility, minutes.fighting), round, digits=1) %>%
        mutate_at(vars(iq, agility, strength, willpower, kills, shots.taken), round, digits=0)

avengers = d
usethis::use_data(avengers, overwrite = TRUE)
#' Simulated Statistics on the Final Avengers Battle
#'
#' A dataset containing combat attributes of almost 812
#' fighters in the final Avengers battle
#'
#' @format A data frame with 812 rows and 13 variables:
#' \describe{
#'   \item{iq}{Intelligence}
#'   \item{agility}{weighted scores on an obstacle course}
#'   \item{speed}{Speed in running the 40 meter dash}
#'   \item{strength}{Pounds lifted in a benchpress}
#'   \item{damage.resistence}{Amount of skin deflection (in mm) when slapped with a frozen fish}
#'   \item{flexibility}{Number of inches past their toes they can reach}
#'   \item{willpower}{Length of time they wait at a DMV for a driver's license}
#'   \item{died}{Whether the person died at the final battle}
#'   \item{kills}{Number of enemies killed in the final battle}
#'   \item{injuries}{Number of injuries sustained. Anything more than 5 is marked as a 5}
#'   \item{minutes.fighting}{Number of minutes spent fighting before dying or giving up}
#'   \item{shots.taken}{Number of shots (with a gun) or punches thrown at an enemy}
#' }
"avengers"








#