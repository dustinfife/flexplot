test_that("Numeric binary outcomes produce logistic curves", {
  
  set.seed(123)
  
  d = data.frame(
    x=rnorm(200)
  )
  
  d$y = rbinom(200, 1, plogis(d$x))
  
  p = flexplot(y~x, data=d, method="logistic")
  
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("Factor binary outcomes produce logistic curves", {
  
  set.seed(123)
  
  d = data.frame(
    x=rnorm(200)
  )
  
  d$y = factor(
    rbinom(200, 1, plogis(d$x)),
    levels=c(0, 1),
    labels=c("No", "Yes")
  )
  
  p = flexplot(y~x, data=d, method="logistic")
  
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_equal(p$scales$get_scales("y")$labels, c("No", "Yes"))
})

test_that("Logistic plots support conditioning variables", {
  
  set.seed(123)
  
  d = data.frame(
    x=rnorm(200),
    group=factor(rep(c("A", "B"), each=100))
  )
  
  d$y = rbinom(200, 1, plogis(d$x))
  
  p = flexplot(y~x | group, data=d, method="logistic")
  
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})