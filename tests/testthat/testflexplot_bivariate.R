context("Bivariate plots")
set.seed(1212)
data(exercise_data); data("relationship_satisfaction")
d = exercise_data
options(warn=-1)
test_that("things go okay when 'group' is a variable", {
  data(avengers)
  avengers$group = avengers$superpower
  vdiffr::expect_doppelganger("group in slot 3",flexplot(iq~shots.taken|group, data=avengers))
  vdiffr::expect_doppelganger("group in slot 2",flexplot(iq~shots.taken+group, data=avengers))
  vdiffr::expect_doppelganger("group in slot 4",flexplot(iq~shots.taken|kills+group, data=avengers))
  vdiffr::expect_doppelganger("group in slot 1",flexplot(iq~group+kills|shots.taken, data=avengers))
})

test_that("error messages are correct", {
  data(avengers)
  data("exercise_data")
  
  expect_error(flexplot(speed~superher, data=avengers),
               "Ru oh! Somebody done made a mistake!"
  )
  expect_error( 
    flexplot(speed~superhero),
    "Howdy! Looks like you forgot to include a dataset!")
  
  expect_error(
    flexplot(gender~therapy.type, data=exercise_data, method="logistic"),
    "Oh wise user of flexplot"
  )
  
  expect_error(
    flexplot(gender~therapy.type + therapy.type, data=exercise_data))
})

test_that("scatterplots and options work", {
  # ### scatter plots
  require(MASS)
  set.seed(1212)
  vdiffr::expect_doppelganger("scatter", flexplot(weight.loss ~ motivation, data =
                                                    d))
  vdiffr::expect_doppelganger("scatter lm no se",
                              flexplot(
                                weight.loss ~ motivation,
                                data = d,
                                method = "lm",
                                se = FALSE
                              ))
  vdiffr::expect_doppelganger(
    "scatter rlm no se no raw",
    flexplot(
      weight.loss ~ motivation,
      data = d,
      method = "rlm",
      se = FALSE,
      raw.data = F
    )
  )
  vdiffr::expect_doppelganger("scatter poly",
                              flexplot(weight.loss ~ motivation, data = d, method = "polynomial"))
  vdiffr::expect_doppelganger("scatter cubic",
                              flexplot(weight.loss ~ motivation, data = d, method = "cubic"))
  vdiffr::expect_doppelganger(
    "scatter logistic jittery",
    flexplot(gender ~ health, data = d, se = FALSE, method = "logistic", jitter = c(0, .1))
  )
  
  vdiffr::expect_doppelganger(
    "numeric in second axis",
    flexplot(weight.loss ~ motivation + satisfaction, 
           data = exercise_data, 
           bins = 2))
  
  vdiffr::expect_doppelganger(
    "line plot",
    flexplot(weight.loss ~ motivation, 
             data = exercise_data, 
             plot.type="line",
             bins = 2))
  
  vdiffr::expect_doppelganger(
    "flexplot with function in formula",
    flexplot(weight.loss ~ sqrt(motivation), 
             data = exercise_data))  
  
})

  

  
test_that("jittered density plots and options work", {
  options(warn = -1)
  set.seed(1212)
  vdiffr::expect_doppelganger("mean plot", flexplot(weight.loss ~ therapy.type, data =
                                                      d))
  formula = weight.loss ~ therapy.type; data =  d
  vdiffr::expect_doppelganger("mean stdev",
                              flexplot(weight.loss ~ therapy.type, data = d, spread = "stdev"))
  vdiffr::expect_doppelganger("mean sterr",
                              flexplot(weight.loss ~ therapy.type, data = d, spread = "sterr"))
  vdiffr::expect_doppelganger("mean no raw",
                              flexplot(
                                weight.loss ~ therapy.type,
                                data = d,
                                raw.data = FALSE
                              ))
  vdiffr::expect_doppelganger("mean no jitter",
                              flexplot(weight.loss ~ therapy.type, data = d, jitter = F))
  vdiffr::expect_doppelganger("mean jitter true",
                              flexplot(weight.loss ~ therapy.type, data = d, jitter = T))
  vdiffr::expect_doppelganger("mean xjitter",
                              flexplot(
                                weight.loss ~ therapy.type,
                                data = d,
                                jitter = c(.3, 0)
                              ))
  vdiffr::expect_doppelganger("mean one jitter",
                              flexplot(
                                weight.loss ~ therapy.type,
                                data = d,
                                jitter = c(.01)
                              ))
  vdiffr::expect_doppelganger("mean both jitter",
                              flexplot(
                                weight.loss ~ therapy.type,
                                data = d,
                                jitter = c(.3, .5)
                              ))
  expect_error(flexplot(gender~therapy.type, data=d, jitter=c(.3, .5), method="logistic"))
  
  vdiffr::expect_doppelganger("boxplot",
                              flexplot(speed~superpower, data=avengers, plot.type="boxplot")) 
  vdiffr::expect_doppelganger("violin plot",
                              flexplot(speed~superpower, data=avengers, plot.type="violin"))
})

options(warn=0)
