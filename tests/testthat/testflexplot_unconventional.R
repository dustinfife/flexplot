context("unconventional plots (related t, association, interaction, panelled logistic)")
set.seed(1212)
data(exercise_data)
data("relationship_satisfaction")
d = exercise_data
k = d
deleteme = which(k$rewards == "no rewards")
k = k[-(deleteme[1:2]), ]
options(warn=-1)
test_that("unconventional plots", {
  
  vdiffr::expect_doppelganger("related T",
                              flexplot(weight.loss ~ rewards, data = k, related = T))
  m = k; m$rewards = as.character(m$rewards)
  vdiffr::expect_doppelganger("related T with character not factor",
                              flexplot(weight.loss ~ rewards, data = k, related = T))  
  vdiffr::expect_doppelganger("association plot",
                              flexplot(gender ~ rewards, data = d, jitter = c(.05, 0)))
  vdiffr::expect_doppelganger("paneled association plot",
                                    flexplot(gender ~ rewards | therapy.type, data = d, jitter = c(.05, 0)))  
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
  
  

})

test_that("given with few categories isn't collapsed",{
  require(dplyr)
  d = exercise_data %>% mutate(gender=as.numeric(gender))
  a = flexplot(weight.loss~health | gender, data=d)
  vdiffr::expect_doppelganger("categories not collapsed", a)
})

test_that("y axis is truncated when predictions go beyond limits", {

  d = avengers %>% mutate(kills = kills + 1)
  full =    glm(kills~minutes.fighting*willpower, data=d, family=Gamma(link="log"))

  vdiffr::expect_doppelganger("truncated axis", 
                              compare.fits(injuries~willpower | minutes.fighting, data=d, full))
})

test_that("flexplot bins when binned variable is very small", {
  require(tidyverse)
  d = avengers %>% mutate(kills = kills *.001)
  head(avengers)
  p = flexplot(ptsd~strength | kills, data=d)
  vdiffr::expect_doppelganger("small_variable values", 
                              p)
})

test_that("flexplot with previous errors", {
  p = flexplot(Sepal.Length~Sepal.Width + Petal.Length | Petal.Width, data=iris, 
           ghost.line="red")
  vdiffr::expect_doppelganger("ghost lines previous error", 
                              p)
})
options(warn=0)
