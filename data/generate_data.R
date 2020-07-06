# ### paranormal dataset
# n = 1000
# #### belief in paranormal dataset
# 
# #### conviction about paranormal beliefs - bimodal
# conviction = c(rnorm(n/2, 40, 10), rnorm(n/2, 70, 10))
# 
# ### fear.of.aliens = positive skew
# fear = c(rnorm(n, 0, 2)^2)		
# 
# ### time spent investigating paranormal paranormal outliers
# time = rnorm(n, 100, 40)
# time = 6000
# ### 999
# 
# ### kidnapped by aliens
# kidnapped = sample(c("yes", "no"), size=n, replace=T, prob=c(.98, .02))		
# flexplot(kidnapped~1, data=data.frame(kidnapped=kidnapped))
# 
# ### paranormal experiences mixed up coding
# experiences.type = sample(c("eerie feeling", "presence watching", "saw UFO", "saw ghost", "heard voice"), size=n, replace=T, prob=c(5, 1, 3, 6, 2))		
# flexplot(experiences.type~1, data=data.frame(experiences.type=experiences.type))
# 
# ### NA
# income = sample(c(">100K", "50-75K", "<50K", "75-100K"), size=n, replace=T, prob=c(1, 8, 2, 3))
# income[sample(1:n, 25)] = NA		
# table(income)
# 
# ### something that doesn't make sense (age)
# age = rnorm(n, 5, 2)^2 + 17
# age[age>70] = runif(1, 18, 75)
# age[age<18] = runif(1, 18, 65)
# age[11] = 123
# age[88] = 2
# 
# paranormal = data.frame(conviction = conviction, fear=fear, time=time, kidnapped=kidnapped, experiences.type=experiences.type, income=income, age=age)
# usethis::use_data(paranormal, overwrite = TRUE)

#' #' Simulated Dataset About Experiences with the Paranormal
#' #'
#' #' A dataset containing combat attributes of almost 812 fighters in the final
#' #' Avengers battle. This dataset illustrates several types of problems one might
#' #' encounter for univariate variables, including bimodality, zero-inflated data,
#' #' outliers, mixed up labels, etc.
#' #'
#' #' @format A data frame with 1000 rows and 7 variables: \describe{
#' #'   \item{conviction}{Degree of conviction they have about the existence of the
#' #'   paranormal} \item{fear}{How much they fear being kidnapped by aliens}
#' #'   \item{time}{How much time they spend a year researching the paranormal}
#' #'   \item{kidnapped}{Whether they've been kidnapped by aliens}
#' #'   \item{experiences.type}{What type of experiences they have had with the
#' #'   paranormal? Can be "eerie feeling," "presence watching", "saw UFO", "saw ghost", or "heard voice"} 
#' #'   \item{income}{How much money they make}
#' #'   \item{age}{Age of respondent}}
#' "paranormal"

#' #'
#' #'     ### avengers dataset
#' set.seed(2323)
#' require(tidyverse)
#' require(dplyr)
#' require(flexplot)
#' #'
#' #'
#' # specify predictor variables ---------------------------------------------
#' 
#'   ### correlation matrix of abilities:
#'   # intelligence, agility, speed, strength,
#'   # damage resistance, superpower (yes/no), flexibility, willpower
#'   # agility/speed and strength need u-shaped relationship
#'   ### adding ptsd score + north vs. south
#' rho = matrix(c(
#'   1,    0,  0,   0,  0,  0,  0,  .2, -.3,  0, # iq
#'   0,    1, .4,  .4,-.2, .6, .5,  .1, -.3,  0, # agility
#'   0,   .4,  1,  .4,-.1, .6, .4,   0, -.1,  0, # speed
#'   0,   .4, .4,   1, .6, .8,-.2,  .2, -.1,  0, # strength
#'   0,  -.2,-.1,  .6,  1, .8,-.3,  .3,  .2,  0, # damage resistance
#'   0,   .6, .6,  .8, .8,  1,  0,  .1, -.5,  0, # superpower
#'   0,   .5, .4, -.2,-.3,  0,  1,   0,   0,  0, # flexibility
#'   .2,  .1,  0,  .2, .3, .1,  0,   1,-.35,  0, # willpower
#'   -.3,-.3,-.1, -.1, .4,-.9,  0,-.35,   1, .6, # ptsd
#'   0,    0,  0,   0,  0,  0,  0,   0,  .3,  1  # north vs. south
#'   ), nrow = 10)
#'   #iq ag  spd str dam sup flex will
#' det(rho)
#' rho = Matrix::nearPD(rho)$mat
#' 
#'   ### simulate data
#' d = MASS::mvrnorm(n=812, mu = rep(0, times=nrow(rho)), Sigma = rho) %>%
#'   data.frame %>%
#'   setNames(c("iq", "agility", "speed", "strength", "damage.resistance",
#'              "superpower", "flexibility", "willpower", "ptsd", "north_south"))
#' d$speed = d$speed + -.2*d$strength^2
#' d$agility = d$agility + -.25*d$strength^2
#' d$ptsd = d$ptsd + .25*d$damage.resistance^2
#'   flexplot::flexplot(ptsd~damage.resistance, data=d)
#'   flexplot::flexplot(ptsd~superpower, data=d)
#' 
#' 
#' # specify outcome variables -----------------------------------------------
#' 
#'   # died (logistic), kills (negative binomial), injuries (ordered logistic?),
#'   # minutes_fighting (gamma), shots taken (normal)
#' 
#' rho2 = matrix(c(
#'   1, -.3, .5, -.4, -.2,   # died
#'   -.3, 1, -.2, .5, .5,    # kills
#'   .5, -.2,  1, .4, .2,    # injuries
#'   -.4, .5, .4,  1, .5,    # minutes_fighting
#'   -.2, .5, .2, .5, 1      # shots taken
#'   ), nrow = 5)
#' 
#' d2 = MASS::mvrnorm(n=nrow(d), mu = rep(0, times=5), Sigma = rho2) %>%
#'   data.frame %>%
#'   setNames(c("died", "kills", "injuries", "minutes.fighting", "shots.taken"))
#' 
#' 
#' # link the two datasets ---------------------------------------------------
#' 
#' died_link = matrix(c( .1, .3, .2, .2, .4, .5, .1, .4, .5, .5)*.25, ncol=1)
#' d2$died = d2$died +   as.numeric(t(died_link) %*% t(data.matrix(d)))
#' 
#' kills_link = matrix(c(.2, .2, .3, .4, .1, .6, .1, .3, .2, .2), ncol=1)
#' d2$kills = d2$kills + as.numeric(t(kills_link) %*% t(data.matrix(d)))
#' 
#' injuries_link = matrix(c(.1, .4, -.1, .2, .5, .5, .1, .1, .6, .2), ncol=1)
#' d2$injuries = d2$injuries + as.numeric(t(injuries_link) %*% t(data.matrix(d)))
#' 
#' minutes_link = matrix(c(-.3, .1, .1, .3, .3, .5, .1, .6, .4, .1), ncol=1)
#' d2$minutes.fighting = d2$minutes.fighting + as.numeric(t(minutes_link) %*% t(data.matrix(d)))
#' 
#' shots_link = matrix(c(0, .1, -.3, .1, 0, -1.85, 0, 0, -.3, .2), ncol=1)
#' d2$shots.taken = d2$shots.taken + as.numeric(t(shots_link) %*% t(data.matrix(d)))
#' 
#' 
#' 
#' 
#' # add interactions --------------------------------------------------------
#' 
#' d2$died = d2$died + -.2*d$superpower*d$strength
#' d2$shots.taken = d2$shots.taken + .75*d$superpower*d$speed
#' d2$minutes.fighting = d2$minutes.fighting + -.5*d$superpower*d$damage.resistance
#' 
#' 
#' # combine data
#' d = cbind(d, d2)
#' 
#' 
#' # dichotomize variables ---------------------------------------------------
#' d$superpower = cut(d$superpower, breaks=c(-Inf, 2, Inf), labels=c("no", "yes"))
#' d$north_south = cut(d$north_south, breaks=c(-Inf, median(d$north_south), Inf), labels=c("north", "south"))
#' d$died = cut(d$died, breaks=c(-Inf, -1.5, Inf), labels=c("yes", "no"))
#' d$kills = exp(d$kills)
#' d$injuries = as.numeric(cut(-1*d$injuries, breaks=c(-Inf, -4, -3, -1, 0, 1, Inf), labels=c(1:6), ordered=T))
#' d$injuries = d$injuries - 1
#' d$minutes.fighting = d$minutes.fighting + 10 + exp(d$minutes.fighting)
#' d$minutes.fighting[d$minutes.fighting<1] = 5
#' 
#' ### add problematic stuff
#' # outliers
#' d[111,c("ptsd", "strength")] = c(7, 7)
#' 
#' # invert relationships (where applicable) ---------------------------------
#' # flexplot(died~agility, data=d, method="logistic")
#' # flexplot(died~speed, data=d, method="logistic")
#' # flexplot(died~strength, data=d, method="logistic")
#' # flexplot(died~damage.resistance, data=d, method="logistic")
#' # flexplot(died~flexibility, data=d, method="logistic")
#' # flexplot(died~willpower, data=d, method="logistic")
#' #
#' # flexplot(superpower~agility, data=d, method="logistic")
#' # flexplot(superpower~speed, data=d, method="logistic")
#' # flexplot(superpower~strength, data=d, method="logistic")
#' # flexplot(superpower~damage.resistance, data=d, method="logistic")
#' # flexplot(superpower~flexibility, data=d, method="logistic")
#' # flexplot(superpower~willpower, data=d, method="logistic")
#' #
#' # flexplot(kills~agility, data=d)
#' # flexplot(kills~speed, data=d)
#' # flexplot(kills~strength, data=d)
#' # flexplot(kills~damage.resistance, data=d)
#' # flexplot(kills~flexibility, data=d)
#' # flexplot(kills~willpower, data=d)
#' #
#' # flexplot(agility~injuries, data=d)
#' # flexplot(speed~injuries, data=d)
#' # flexplot(strength~injuries, data=d)
#' # flexplot(damage.resistance~injuries, data=d)
#' # flexplot(flexibility~injuries, data=d)
#' # flexplot(ptsd~superpower, data=d)
#' # flexplot(ptsd~damage.resistance, data=d)
#' 
#' # put variables on correct scale ------------------------------------------
#' require(fifer)
#' d_final = d %>% mutate(iq = rescale(iq, 110, 8),
#'              agility = rescale(agility, 50, 15),
#'              speed = rescale(speed, 5, .17),              # meter dash
#'              strength = rescale(strength, 500, 150),      # bench press mas
#'              damage.resistance = rescale(damage.resistance, 2, .21),
#'              flexibility = rescale(flexibility, -1, 1),   # inches beyond shoes
#'              willpower = rescale(willpower, 60, 18),
#'              shots.taken = rescale(shots.taken, 130, 30),
#'              ptsd        = rescale(ptsd, 4, .6)) %>%
#'         mutate_at(vars(speed,damage.resistance),
#'                   funs(round(., 2))) %>%
#'         mutate_at(vars(flexibility, minutes.fighting, ptsd), round, digits=1) %>%
#'         mutate_at(vars(iq, agility, strength, willpower, kills, shots.taken), round, digits=0)
#' avengers = d_final
#' head(avengers)
#' usethis::use_data(avengers, overwrite = TRUE)
#' write.csv(avengers, "data/avengers.csv", row.names=F)
#' #' Simulated Statistics on the Final Avengers Battle
#' #'
#' #' A dataset containing combat attributes of almost 812
#' #' fighters in the final Avengers battle
#' #'
#' #' @format A data frame with 812 rows and 13 variables:
#' #' \describe{
#' #'   \item{iq}{Intelligence}
#' #'   \item{agility}{weighted scores on an obstacle course}
#' #'   \item{speed}{Speed in running the 40 meter dash}
#' #'   \item{strength}{Pounds lifted in a benchpress}
#' #'   \item{damage.resistance}{Amount of skin deflection (in mm) when slapped with a frozen fish}
#' #'   \item{flexibility}{Number of inches past their toes they can reach}
#' #'   \item{willpower}{Length of time they wait at a DMV for a driver's license}
#' #'   \item{ptsd}{Score on a Likert-scale PTSD questionnaire}
#' #'   \item{north_south}{whether the individual was assigned to fight on the north or south battlefield}
#' #'   \item{died}{Whether the person died at the final battle}
#' #'   \item{kills}{Number of enemies killed in the final battle}
#' #'   \item{injuries}{Number of injuries sustained. Anything more than 5 is marked as a 5}
#' #'   \item{minutes.fighting}{Number of minutes spent fighting before dying or giving up}
#' #'   \item{shots.taken}{Number of shots (with a gun) or punches thrown at an enemy}
#' #' }
#' "avengers"
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' #