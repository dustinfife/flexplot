test_that("icc works", {
  data(math)
  mod = lmer(MathAch~1 + (1|School), data=math)
  expect_equal(icc(mod)$icc, .18, tol=.01)
})