context("visualize function on mixed models)")
options(warn=-1)

test_that("make_formula_mixed works", {
  expect_equal(as.character(make_formula_mixed(c("a", "b", "c"), "c", "q", NULL))[3], "a + c | b")
  expect_equal(as.character(make_formula_mixed(c("a"), "a", "q", NULL))[3], "a")
  expect_equal(as.character(make_formula_mixed(c("a", "b", "c"), "c", "q", a~b))[3], "b")
})

test_that("find_paneled_variables works", {
  expect_equal(find_paneled_variables(y~a + b | x), "x")
  expect_equal(find_paneled_variables(y~a + b + x), NULL)
  expect_equal(find_paneled_variables(y~a | a + x), c("a", "x"))
})

test_that("randomly_sample_clusters works", {
  expect_true(length(unique(randomly_sample_clusters(math, "School", 1)$School))==1)
  expect_true(length(unique(randomly_sample_clusters(math, "School", 10)$School))==10)
})

set.seed(1212)
test_that("visualize mixed models", {
  #### mixed models
  data(math)
  math = math[1:1000,]
  model = lme4::lmer(MathAch~ SES + Sex + (SES|School), data=math)
  set.seed(1212)
  a = visualize(model, formula = MathAch~ SES | Sex + School, plot="model")
  b = visualize(model, formula = MathAch~ SES + School| Sex)
  c = visualize(model, formula = MathAch~ Sex | SES+ School, plot="model")
  
  mod = lme4::lmer(MathAch~1 + (1|School), data=math)
  a1 = visualize(mod, plot="model")
  mod1 = lme4::lmer(MathAch~SES + (SES|School), data=math)
  a2 = visualize(mod1, plot="model")
  a3 = visualize(model, plot = "model",
                 formula = MathAch ~  Sex + School| SES, 
                 sample = 30)
  vdiffr::expect_doppelganger("mixed row panels",a)
  vdiffr::expect_doppelganger("mixed diff lines",b)
  vdiffr::expect_doppelganger("mixed small sample",c)
  vdiffr::expect_doppelganger("mixed anova with no formula",a1)
  vdiffr::expect_doppelganger("mixed no formula one covariate", a2)
  vdiffr::expect_doppelganger("mixed old error", a3)
            
})


test_that("visualize mixed models with alcuse", {
  #### mixed models
  data(alcuse)
  mod1 = lme4::lmer(ALCUSE~1 + (1|ID), data=alcuse)
  #a5 = visualize(mod1, plot="model")  
  mod2 = lme4::lmer(ALCUSE~AGE_14 + (1|ID), data=alcuse)
  a4 = visualize(mod2, plot="model")  
  
  vdiffr::expect_doppelganger("mixed when x axis has <5 levels", a4)
  #vdiffr::expect_doppelganger("random effects anova", a5)
  
})
options(warn=0)