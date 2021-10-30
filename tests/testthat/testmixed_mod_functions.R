context("testing mixed model functions")

# collect objects
require(lme4)
data("math")
d = math
ses_mixed = lmer(MathAch~SES + (SES|School), data=d)
ses_lm = lm(MathAch~SES, data=d)
ses_poly = lmer(MathAch~SES + (SES^2) + (SES|School), data=d)

test_that("subset_random_model works", {
  testthat::expect_true(length(levels(subset_random_model(ses_mixed, MathAch~SES , d, samp.size=5)$School))==5)
  testthat::expect_false(length(levels(subset_random_model(ses_lm, MathAch~SES ,d, samp.size=5)$School))==5)
})

test_that("extract_random_term works", {
  expect_identical(extract_random_term(ses_mixed), "School")
  expect_identical(extract_random_term(ses_poly), "School")
})

test_that("gsub_data_first works", {
  expect_equal(gsub_data_first(c("a", "b", "c"), "b", "c"), c("a", "c", "c"))
})

test_that("remove_term_from_formula works", {
  expect_equal(remove_term_from_formula(y~a + b | c + d, "b", F), "y~a|c+d")
  expect_equal(remove_term_from_formula(y~a + b | c + d, "c", F), "y~a+b|d")
  expect_equal(remove_term_from_formula(y~a + b | c + d, "d", F), "y~a+b|c")
  expect_equal(remove_term_from_formula(y~a + b | c + d, "d", T), y~a+b|c)
})

test_that("get_row_col works", {
  p = flexplot(speed~agility | superpower + ptsd, data=avengers)
  expect_equal(get_row_col(p), 6)
  expect_equal(get_row_col(flexplot(speed~agility | superpower, data=avengers)), 2)
  expect_equal(get_row_col(flexplot(speed~superpower | agility + ptsd, data=avengers)), 9)
  expect_equal(get_row_col(flexplot(speed~superpower | agility + ptsd, data=avengers, bins=4)), 16)
})

test_that("stratified_sample_re works", {
  a = stratified_sample_re(weight.loss~health | motivation + therapy.type, data=exercise_data, re="satisfaction", samp.size=6)
  expect_true(length(a)==6)
  expect_true(all(!duplicated(a)))
  b = stratified_sample_re(MathAch~Sex | SES + School, data=math, re="School", samp.size =8)
  expect_true(length(b)==8)
  expect_true(all(!duplicated(b)))
  c = stratified_sample_re(MathAch~Sex | SES + School, data=math, re="School", samp.size =200)
  expect_true(length(c)!=200)
  expect_true(all(!duplicated(c)))
  stratified_sample_re(MathAch~ SES + School| Sex, data=math, re="School", 11)
})

test_that("find_paneled_variables works", {
  expect_equal(find_paneled_variables(y~a + b | x), "x")
  expect_equal(find_paneled_variables(y~a + b + x), NULL)
  expect_equal(find_paneled_variables(y~a | a + x), c("a", "x"))
})



test_that("make_formula_mixed works", {
  expect_equal(as.character(make_formula_mixed(c("a", "b", "c"), "c", "q", NULL))[3], "a + c | b")
  expect_equal(as.character(make_formula_mixed(c("a"), "a", "q", NULL))[3], "a")
  expect_equal(as.character(make_formula_mixed(c("a", "b", "c"), "c", "q", a~b))[3], "b")
})

test_that("are_re_plotted works", {
  expect_true(are_re_plotted(y~a + b | c, "c"))
  expect_false(are_re_plotted(y~a + b | c, "d"))
  expect_false(are_re_plotted(y~a + I(b^2) | c, "d"))  
})
# 
# test_that("hidden functions for lme4", {
#   
#   
#   # when there's a polynomial in lmers
#   
#   
#   
#   
#   ## make sure the two models in compare.fits for lme4 are both lme4 objects and have the same random terms
#   object3 = lmer(MathAch~SES + (SES|MEANSES), data=d)
#   testthat::expect_null(test_same_class(object, object))
#   testthat::expect_error(test_same_class(object, object2))
#   testthat::expect_error(test_same_class(object, object3))
# })
# 
# 
