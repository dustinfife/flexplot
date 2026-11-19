context("Bivariate plots")
set.seed(1212)
data(exercise_data); data("relationship_satisfaction")
d = exercise_data
options(warn=-1)
test_that("things go okay when 'group' is a variable", {
  data(avengers)
  avengers$group = avengers$superpower
  expect_flexplot_snapshot("testflexplot_bivariate/group-in-slot-3.svg", "group in slot 3",flexplot(iq~shots.taken|group, data=avengers))
  expect_flexplot_snapshot("testflexplot_bivariate/group-in-slot-2.svg", "group in slot 2",flexplot(iq~shots.taken+group, data=avengers))
  expect_flexplot_snapshot("testflexplot_bivariate/group-in-slot-4.svg", "group in slot 4",flexplot(iq~shots.taken|kills+group, data=avengers))
  #vdiffr::expect_doppelganger("group in slot 1",flexplot(iq~group+kills|shots.taken, data=avengers))
})

test_that("error messages are correct", {
  data(avengers)
  data("exercise_data")
  
  expect_error(flexplot(speed~superher, data=avengers),
               "Ru oh! Somebody done made a mistake!"
  )
  
  
  expect_error(flexplot(weight.loss ~ rewards, data = k, related = T))
  expect_error( 
    flexplot(speed~superhero),
    "Howdy! Looks like you forgot to include a dataset!")

  expect_error(
    flexplot(gender~therapy.type + therapy.type, data=exercise_data))
})

test_that("scatterplots and options work", {
  # ### scatter plots
  require(MASS)
  set.seed(1212)
  expect_flexplot_snapshot("testflexplot_bivariate/scatter.svg", "scatter", flexplot(weight.loss ~ motivation, data =
                                                    d))
  expect_flexplot_snapshot("testflexplot_bivariate/scatter-lm-no-se.svg", "scatter lm no se",
                              flexplot(
                                weight.loss ~ motivation,
                                data = d,
                                method = "lm",
                                se = FALSE
                              ))
  expect_flexplot_snapshot("testflexplot_bivariate/scatter-rlm-no-se-no-raw.svg", "scatter rlm no se no raw",
    flexplot(
      weight.loss ~ motivation,
      data = d,
      method = "rlm",
      se = FALSE,
      raw.data = F
    )
  )
  expect_flexplot_snapshot("testflexplot_bivariate/scatter-poly.svg", "scatter poly",
                              flexplot(weight.loss ~ motivation, data = d, method = "polynomial"))
  expect_flexplot_snapshot("testflexplot_bivariate/scatter-cubic.svg", "scatter cubic",
                              flexplot(weight.loss ~ motivation, data = d, method = "cubic"))
  expect_flexplot_snapshot("testflexplot_bivariate/scatter-logistic-jittery.svg", "scatter logistic jittery",
    flexplot(gender ~ health, data = d, se = FALSE, method = "logistic", jitter = c(0, .1))
  )
  
  expect_flexplot_snapshot("testflexplot_bivariate/numeric-in-second-axis.svg", "numeric in second axis",
    flexplot(weight.loss ~ motivation + satisfaction, 
           data = exercise_data, 
           bins = 2))
  
  expect_flexplot_snapshot("testflexplot_bivariate/line-plot.svg", "line plot",
    flexplot(weight.loss ~ motivation, 
             data = exercise_data, 
             plot.type="line",
             bins = 2))
  
  expect_flexplot_snapshot("testflexplot_bivariate/flexplot-with-function-in-formula.svg", "flexplot with function in formula",
    flexplot(weight.loss ~ sqrt(motivation), 
             data = exercise_data))  
  
})

  

  
test_that("jittered density plots and options work", {
  options(warn = -1)
  set.seed(1212)
  expect_flexplot_snapshot("testflexplot_bivariate/mean-plot.svg", "mean plot", flexplot(weight.loss ~ therapy.type, data =
                                                      d))
  formula = weight.loss ~ therapy.type; data =  d
  expect_flexplot_snapshot("testflexplot_bivariate/mean-stdev.svg", "mean stdev",
                              flexplot(weight.loss ~ therapy.type, data = d, spread = "stdev"))
  expect_flexplot_snapshot("testflexplot_bivariate/mean-sterr.svg", "mean sterr",
                              flexplot(weight.loss ~ therapy.type, data = d, spread = "sterr"))
  expect_flexplot_snapshot("testflexplot_bivariate/mean-no-raw.svg", "mean no raw",
                              flexplot(
                                weight.loss ~ therapy.type,
                                data = d,
                                raw.data = FALSE
                              ))
  expect_flexplot_snapshot("testflexplot_bivariate/mean-no-jitter.svg", "mean no jitter",
                              flexplot(weight.loss ~ therapy.type, data = d, jitter = F))
  expect_flexplot_snapshot("testflexplot_bivariate/mean-jitter-true.svg", "mean jitter true",
                              flexplot(weight.loss ~ therapy.type, data = d, jitter = T))
  expect_flexplot_snapshot("testflexplot_bivariate/mean-xjitter.svg", "mean xjitter",
                              flexplot(
                                weight.loss ~ therapy.type,
                                data = d,
                                jitter = c(.3, 0)
                              ))
  expect_flexplot_snapshot("testflexplot_bivariate/mean-one-jitter.svg", "mean one jitter",
                              flexplot(
                                weight.loss ~ therapy.type,
                                data = d,
                                jitter = c(.01)
                              ))
  expect_flexplot_snapshot("testflexplot_bivariate/mean-both-jitter.svg", "mean both jitter",
                              flexplot(
                                weight.loss ~ therapy.type,
                                data = d,
                                jitter = c(.3, .5)
                              ))
  
  expect_flexplot_snapshot("testflexplot_bivariate/boxplot.svg", "boxplot",
                              flexplot(speed~superpower, data=avengers, plot.type="boxplot")) 
  expect_flexplot_snapshot("testflexplot_bivariate/violin-plot.svg", "violin plot",
                              flexplot(speed~superpower, data=avengers, plot.type="violin"))
})

options(warn=0)
