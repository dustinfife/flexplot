# =============================================================================
# UNIT TESTS FOR LOGISTIC OVERLAY HELPER FUNCTIONS
# File: tests/testthat/test-logistic_overlay_helpers.R
# =============================================================================

# Test data setup - using flexplot's small datase
test_data = small

# Add bins for testing (simulating what would happen in logistic_overlay)
bin_breaks = pretty(test_data$x, n = 4)
test_data$bin = cut(test_data$x, breaks = bin_breaks, include.lowest = TRUE)

# Mock flexplot object for testing
mock_plot = list(
  data = test_data,
  mapping = aes(x = x, y = y_bin, colour = a, shape = a)
)
class(mock_plot) = "ggplot"

# =============================================================================
# Tests for build_inherited_aesthetics()
# =============================================================================

test_that("build_inherited_aesthetics creates basic x,y mapping", {
  simple_plot = list(mapping = aes(x = x, y = y))
  
  result = build_inherited_aesthetics(simple_plot, test_data)
  
  expect_true("x" %in% names(result))
  expect_true("y" %in% names(result))
  expect_equal(length(result), 2)
})

test_that("build_inherited_aesthetics inherits colour mapping", {
  plot_with_color = list(mapping = aes(x = x, y = y_bin, colour = a))
  
  result = build_inherited_aesthetics(plot_with_color, test_data)
  
  expect_true("colour" %in% names(result))
  expect_equal(length(result), 3)
})

test_that("build_inherited_aesthetics ignores missing variables", {
  plot_with_missing = list(mapping = aes(x = x, y = y_bin, colour = missing_var))
  
  result = build_inherited_aesthetics(plot_with_missing, test_data)
  
  # Should only have x and y, not colour since missing_var doesn't exist
  expect_equal(length(result), 2)
  expect_false("colour" %in% names(result))
})

test_that("build_inherited_aesthetics handles custom x,y names", {
  simple_plot = list(mapping = aes(x = x, y = y_bin))
  
  result = build_inherited_aesthetics(simple_plot, test_data, 
                                       base_x = "custom_x", base_y = "custom_y")
  
  expect_equal(rlang::as_name(result$x), "custom_x")
  expect_equal(rlang::as_name(result$y), "custom_y")
})

# =============================================================================
# Tests for create_logistic_summary()
# =============================================================================

test_that("create_logistic_summary calculates proportions correctly", {
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  bin_width = diff(bin_breaks)[1]
  
  result = create_logistic_summary(
    data = test_data,
    group_vars = "bin",
    outcome_var = "y_bin",
    bin_centers = bin_centers,
    bin_width = bin_width
  )
  
  expect_true(nrow(result) > 0)
  expect_true("proportion" %in% names(result))
  expect_true("count" %in% names(result))
  
  # Check that proportions are valid
  expect_true(all(result$proportion >= 0 & result$proportion <= 1, na.rm = TRUE))
})

test_that("create_logistic_summary handles character outcomes", {
  char_data = test_data
  char_data$y_char = ifelse(char_data$y_bin == 1, "yes", "no")
  
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  bin_width = diff(bin_breaks)[1]
  
  result = create_logistic_summary(
    data = char_data,
    group_vars = "bin",
    outcome_var = "y_char",
    bin_centers = bin_centers,
    bin_width = bin_width
  )
  
  expect_true(all(result$proportion >= 0 & result$proportion <= 1, na.rm = TRUE))
})

test_that("create_logistic_summary handles factor outcomes", {
  factor_data = test_data
  factor_data$y_factor = factor(factor_data$y_bin, levels = c(0, 1), labels = c("no", "yes"))
  
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  bin_width = diff(bin_breaks)[1]
  
  result = create_logistic_summary(
    data = factor_data,
    group_vars = "bin",
    outcome_var = "y_factor",
    bin_centers = bin_centers,
    bin_width = bin_width
  )
  
  expect_true(all(result$proportion >= 0 & result$proportion <= 1, na.rm = TRUE))
})

test_that("create_logistic_summary calculates bin_mid correctly", {
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  bin_width = diff(bin_breaks)[1]
  
  result = create_logistic_summary(
    data = test_data,
    group_vars = "bin",
    outcome_var = "y_bin",
    bin_centers = bin_centers,
    bin_width = bin_width
  )
  
  # bin_mid should be calculated from bin_centers
  expect_true(all(!is.na(result$bin_mid)))
  expect_true(all(is.finite(result$bin_mid)))
})

test_that("create_logistic_summary calculates rectangle bounds", {
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  bin_width = diff(bin_breaks)[1]
  
  result = create_logistic_summary(
    data = test_data,
    group_vars = "bin",
    outcome_var = "y_bin",
    bin_centers = bin_centers,
    bin_width = bin_width
  )
  
  expect_true(all(result$xmin < result$xmax, na.rm = TRUE))
  expect_true(all(result$xmin == result$bin_mid - bin_width * 0.4, na.rm = TRUE))
  expect_true(all(result$xmax == result$bin_mid + bin_width * 0.4, na.rm = TRUE))
})

test_that("create_logistic_summary handles multiple grouping variables", {
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  bin_width = diff(bin_breaks)[1]
  
  result = create_logistic_summary(
    data = test_data,
    group_vars = c("bin", "a"),
    outcome_var = "y_bin",
    bin_centers = bin_centers,
    bin_width = bin_width
  )
  
  # Should have groups split by both bin and factor 'a'
  expect_true("a" %in% names(result))
  unique_groups = result %>% distinct(bin, a) %>% nrow()
  expect_gt(unique_groups, length(unique(result$bin)))
})

test_that("create_logistic_summary fails gracefully with missing data", {
  expect_error(
    create_logistic_summary(outcome_var = "y"),
    "Must provide either 'plot' or 'data'"
  )
  
  expect_error(
    create_logistic_summary(data = test_data),
    "Must provide either 'plot' or 'data'"
  )
})

test_that("create_logistic_summary scales size and alpha appropriately", {
  bin_centers = (bin_breaks[-1] + bin_breaks[-length(bin_breaks)]) / 2
  bin_width = diff(bin_breaks)[1]
  
  result = create_logistic_summary(
    data = test_data,
    group_vars = "bin",
    outcome_var = "y_bin",
    bin_centers = bin_centers,
    bin_width = bin_width
  )
  
  expect_true(all(result$alpha >= 0.2 & result$alpha <= 1, na.rm = TRUE))
})


# 

# tests for convert_formula_to_logistic_bins
# test_that("convert_formula_to_logistic_bins works", {
#   convert_formula_to_logistic_bins(died~agility + superpower, data=avengers)
# })