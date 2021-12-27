test_that("variables_to_be_ghosted works", {
  expect_equal(variables_to_be_ghosted(y~a+b|z), c("b", "z"))
  expect_equal(variables_to_be_ghosted(y~a|z), "z")
  expect_null(variables_to_be_ghosted(y~a))
})

test_that("fill_empty_ghost_reference works", {
  expect_equal(fill_empty_ghost_reference(small, "x"), .3199, tol=.01)
  expect_equal(fill_empty_ghost_reference(small, "a"), "a")
  expect_equal(fill_empty_ghost_reference(small, "a", "b"), "b")
  expect_equal(fill_empty_ghost_reference(small, "x", .2), .2)
})

test_that("fill_ghost_reference works", {
  fill_ghost_reference(small, "x")
  fill_ghost_reference(small, "b")
  fill_ghost_reference(small, "x", value=.4)
})

test_that('get_ghost_reference_value works', {
  expect_null(get_ghost_reference_value(NULL, "x"))
  expect_null(get_ghost_reference_value(list("b" = "x"), "x"))
  expect_equal(get_ghost_reference_value(list("x" = .2, "y" = .3), "x"), .2)
})

test_that("create_ghost_reference works", {
  expect_equal(create_ghost_reference(y~a+b|z, small), "xxxx")
  expect_equal(create_ghost_reference(y~a+b|z, small, ghost.line="gray")[1]$b, "y")
  expect_equal(create_ghost_reference(y~a+b, small, ghost.line="gray", ghost.reference = list(b="x"))$b, "x")
  expect_equal(create_ghost_reference(y~a+b|z+x, small, ghost.line="gray", ghost.reference = list(b="x"))$b, "x")
  expect_equal(create_ghost_reference(y~a+x, small, ghost.line="gray", ghost.reference = list(x=.33))$x_binned %>%
                 as.character, "(-0.23)-0.64")
})



