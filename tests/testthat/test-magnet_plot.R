test_that("magnet_plot works", {
  vdiffr::expect_doppelganger("magnet plot with numeric binary", magnet_plot(y_bin~a, data=small))
  expect_error(magnet_plot(b~a, data=small), "Your outcome variable must have exactly 2 levels")
  expect_message(magnet_plot(a~b, data=small), "your outcome variable levels")
  vdiffr::expect_doppelganger("magnet plot with categorical binary", magnet_plot(a~b, data=small))
  expect_error(magnet_plot(a~y, data=small))
})

test_that("check_magnet_errors works", {
  expect_equal(small, check_magnet_errors(small, "y_bin", "a"))
  expect_error(check_magnet_errors(small, "b", "a"))
  expect_equal(unique(check_magnet_errors(small, "a", "b")$a), c(0,1)) %>% suppressMessages()
  expect_message(check_magnet_errors(small, "a", "b"), "your outcome variable levels")
})


