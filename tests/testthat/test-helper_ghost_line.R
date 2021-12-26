test_that("variables_to_be_ghosted works", {
  expect_equal(variables_to_be_ghosted(y~a+b|z), c("b", "z"))
  expect_equal(variables_to_be_ghosted(y~a|z), "z")
  expect_null(variables_to_be_ghosted(y~a))
})

test_that("create_ghost_reference works", {
  expect_equal(create_ghost_reference(y~a+b|z, small), "xxxx")
  expect_equal(create_ghost_reference(y~a+b|z, small, ghost.line="gray")[1]$b, "y")
  create_ghost_reference(y~a+b, small, ghost.line="gray", ghost.reference = list(b="x"))
  
  #expect_equal([1]$b, "y")
})