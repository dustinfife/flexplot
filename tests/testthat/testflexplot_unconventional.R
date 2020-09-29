context("unconventional plots (related t, association, interaction, panelled logistic)")
set.seed(1212)
data(exercise_data)
data("relationship_satisfaction")
d = exercise_data
k = d
deleteme = which(k$rewards == "no rewards")
k = k[-(deleteme[1:2]), ]

test_that("unconventional plots", {
  vdiffr::expect_doppelganger("related T",
                              flexplot(weight.loss ~ rewards, data = k, related = T))
  m = k; m$rewards = as.character(m$rewards)
  vdiffr::expect_doppelganger("related T with character not factor",
                              flexplot(weight.loss ~ rewards, data = k, related = T))  
  vdiffr::expect_doppelganger("association plot",
                              flexplot(gender ~ rewards, data = d, jitter = c(.05, 0)))
  vdiffr::expect_doppelganger("interaction plot",
                              flexplot(
                                weight.loss ~ therapy.type + gender,
                                data = d,
                                alpha = .4
                              ))
  
  vdiffr::expect_doppelganger(
    "interaction plot panel",
    flexplot(
      weight.loss ~ therapy.type | gender,
      data = d,
      sample = 50
    )
  )
  
  data("tablesaw.injury")
  mod = glm(injury~attention, data=tablesaw.injury, family=binomial)
  vdiffr::expect_doppelganger(
    "logistic with numeric outcome",
    compare.fits(injury~attention, data=tablesaw.injury, mod, jitter=c(0, .1))
  )
  
  mod = glm(gender~attention, data=tablesaw.injury, family=binomial)
  vdiffr::expect_doppelganger(
    "logistic with categorical outcome",
    compare.fits(gender~attention, data=tablesaw.injury, mod, jitter=c(0, .1))
  )
  
  compare.fits(gender~attention, data=tablesaw.injury, mod, jitter=c(0, .1))
  vdiffr::expect_doppelganger(
    "panelled logistic with sampling",
    flexplot(
      gender ~ weight.loss |
        therapy.type,
      data = d,
      sample = 50,
      method = "logistic"
    )
  )
})

test_that("given with few categories isn't collapsed",{
  require(dplyr)
  d = exercise_data %>% mutate(gender=as.numeric(gender))
  a = flexplot(weight.loss~health | gender, data=d)
  vdiffr::expect_doppelganger("categories not collapsed", a)
  

})
