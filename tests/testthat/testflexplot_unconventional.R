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
  
  expect_flexplot_snapshot("testflexplot_unconventional/related-t.svg", "related T",
                              flexplot(weight.loss ~ rewards, data = k, related = T))
  m = k; m$rewards = as.character(m$rewards)
  expect_flexplot_snapshot("testflexplot_unconventional/related-t-with-character-not-factor.svg", "related T with character not factor",
                              flexplot(weight.loss ~ rewards, data = k, related = T))  
  expect_flexplot_snapshot("testflexplot_unconventional/magnet-plot.svg", "magnet plot",
                              flexplot(gender ~ rewards, data = d, jitter = c(.05, 0)))
  expect_flexplot_snapshot("testflexplot_unconventional/paneled-magnet-plot.svg", "paneled magnet plot",
                                    flexplot(gender ~ rewards | therapy.type, data = d, jitter = c(.05, 0)))  
  expect_flexplot_snapshot("testflexplot_unconventional/interaction-plot.svg", "interaction plot",
                              flexplot(
                                weight.loss ~ therapy.type + gender,
                                data = d,
                                alpha = .4
                                      
                              ))
    
  expect_flexplot_snapshot("testflexplot_unconventional/interaction-plot-panel.svg", "interaction plot panel",
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
  expect_flexplot_snapshot("testflexplot_unconventional/categories-not-collapsed.svg", "categories not collapsed", a)
})

test_that("y axis is truncated when predictions go beyond limits", {

  d = avengers %>% mutate(kills = kills + 1)
  full =    glm(kills~minutes.fighting*willpower, data=d, family=Gamma(link="log"))

  expect_flexplot_snapshot("testflexplot_unconventional/truncated-axis.svg", "truncated axis",
                              compare.fits(kills~willpower | minutes.fighting, data=d, full)%>%suppressWarnings())
})

test_that("flexplot bins when binned variable is very small", {
  require(tidyverse)
  d = avengers %>% mutate(kills = kills *.001)
  head(avengers)
  p = flexplot(ptsd~strength | kills, data=d)
  expect_flexplot_snapshot("testflexplot_unconventional/small-variable-values.svg", "small_variable values",
                              p)
})

test_that("flexplot with previous errors", {
  p = flexplot(Sepal.Length~Sepal.Width + Petal.Length | Petal.Width, data=iris, 
           ghost.line="red")
  expect_flexplot_snapshot("testflexplot_unconventional/ghost-lines-previous-error.svg", "ghost lines previous error",
                              p)
})
options(warn=0)
