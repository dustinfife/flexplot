test_that("CAS formula extraction does not use deprecated lme4 wrappers", {
  m = lme4::lmer(y ~ x + (x | id), small_mixed)
  expect_no_warning({ adjusted = compute_cas_from_model(m, adjust_re="intercept") })
  expect_equal(adjusted$y_adj,
               small_mixed$y - unname(predict(m, re.form=~ (1 | id)) - predict(m, re.form=NA)))
  expect_no_warning({ p = cluster_adjusted_scatter(y ~ x, m, jitter=FALSE) })
  expect_no_warning(ggplot2::ggplot_build(p))
})

test_that("CAS subtracts each selected random contribution exactly once", {
  for (random in c("(1 | id)", "(x | id)")) {
    m = lme4::lmer(make.formula("y", "x", random), small_mixed)
    fixed = as.numeric(predict(m, re.form=NA))
    conditional = as.numeric(predict(m, re.form=NULL))
    intercept = as.numeric(predict(m, re.form=~ (1 | id)))
    expected = list(all=small_mixed$y - (conditional - fixed),
                    intercept=small_mixed$y - (intercept - fixed), none=small_mixed$y)
    for (mode in names(expected)) {
      d = compute_cas_from_model(m, adjust_re=mode)
      expect_equal(d$y_adj, expected[[mode]], tolerance=1e-10)
      expect_equal(d$pred_fixed, fixed, tolerance=1e-10)
      expect_equal(unname(cas_adjustment(small_mixed, m, adjust=mode)), expected[[mode]], tolerance=1e-10)
    }
    expect_equal(compute_cas_from_model(m)$y_adj, expected$intercept, tolerance=1e-10)
  }
  m = lme4::lmer(y ~ x + (x | id), small_mixed)
  changed = small_mixed[rev(seq_len(nrow(small_mixed))), ]
  rownames(changed) = seq_len(nrow(changed))
  expect_error(cas_adjustment(changed, m), "Data do not match")
  expected = small_mixed$y - as.numeric(predict(m, re.form=~ (0 + x | id)) - predict(m, re.form=NA))
  expect_equal(compute_cas_from_model(m, adjust_re=~ (0 + x | id))$y_adj, expected)
  expect_equal(cas_adjustment(small_mixed, adjust="none", outcome_name="y"), small_mixed$y)
  expect_equal(cas_adjustment(small_mixed, adjust="all", outcome_name="y",
                 pred_fixed=unname(predict(m, re.form=NA)), pred_cond=unname(predict(m))),
               compute_cas_from_model(m, adjust_re="all")$y_adj)
  d = small_mixed
  d$cohort = factor(rep(1:2, length.out=nrow(d)))
  compound = lme4::lmer(y ~ x + (1 | id:cohort), d)
  expected = d$y - unname(predict(compound, re.form=~ (1 | id:cohort)) - predict(compound, re.form=NA))
  expect_equal(compute_cas_from_model(compound, adjust_re="intercept")$y_adj, expected)
})

test_that("CAS plots adjusted observations and the original fixed-effects curve", {
  for (random in c("(1 | id)", "(x | id)")) {
    m = lme4::lmer(make.formula("y", "x", random), small_mixed)
    for (mode in c("all", "intercept", "none")) {
      p = cluster_adjusted_scatter(y ~ x, m, adjust_re=mode, jitter=FALSE)
      layers = ggplot2::ggplot_build(p)$data
      expected = compute_cas_from_model(m, adjust_re=mode)
      expect_equal(layers[[1]]$y, expected$y_adj[match(layers[[1]]$x, expected$x)])
      order_x = order(small_mixed$x)
      expect_equal(layers[[2]]$x, small_mixed$x[order_x])
      expect_equal(layers[[2]]$y, unname(predict(m, re.form=NA))[order_x], tolerance=1e-10)
      expect_false(any(vapply(p$layers, function(layer) inherits(layer$geom, "GeomSmooth"), logical(1))))
    }
  }
  m = lme4::lmer(y ~ x + (x | id), small_mixed)
  full = cluster_adjusted_scatter(y ~ x, m, jitter=FALSE)
  sampled = cluster_adjusted_scatter(y ~ x, m, sample=3, jitter=FALSE)
  expect_equal(nrow(ggplot2::ggplot_build(sampled)$data[[1]]), 3)
  expect_true(inherits(tail(sampled$layers, 1)[[1]]$geom, "GeomLine"))
  expect_equal(tail(ggplot2::ggplot_build(sampled)$data, 1)[[1]], ggplot2::ggplot_build(full)$data[[2]])
  smoothed = cluster_adjusted_scatter(y ~ x, m, smooth=TRUE, jitter=FALSE)
  expect_equal(smoothed$layers[[2]]$data, full$layers[[2]]$data)
  expect_true(inherits(smoothed$layers[[3]]$geom, "GeomSmooth"))
  expect_equal(cluster_adjusted_scatter(y ~ x, m, return_data=TRUE)$y_adj,
               compute_cas_from_model(m, adjust_re="all")$y_adj)
})

test_that("CAS handles omitted fitting rows and reordered external data", {
  d = small_mixed
  rownames(d) = paste0("case_", seq_len(nrow(d)))
  d$x[2] = NA
  d$y[6] = NA
  d$z[9] = NA
  m = lme4::lmer(y ~ x + z + (x | id), d, na.action=na.exclude)
  fitted = model.frame(m)
  ids = rownames(fitted)
  expected = fitted$y - unname(predict(m)[ids] - predict(m, re.form=NA)[ids])
  adjusted = compute_cas_from_model(m, adjust_re="all")
  expect_identical(rownames(adjusted), ids)
  expect_equal(adjusted$y_adj, expected, tolerance=1e-10)
  shuffled = d[rev(seq_len(nrow(d))), ]
  external = cas_adjustment(shuffled, m, adjust="all")
  expect_equal(cas_adjustment(shuffled, m, adjust="none"), shuffled$y)
  expect_equal(unname(external[ids]), expected, tolerance=1e-10)
  expect_true(all(is.na(external[setdiff(rownames(d), ids)])))
  reset = shuffled
  rownames(reset) = seq_len(nrow(reset))
  expect_error(cas_adjustment(reset, m), "align data")
  p = cluster_adjusted_scatter(y ~ x, m, data=shuffled, jitter=FALSE)
  expect_equal(nrow(ggplot2::ggplot_build(p)$data[[1]]), nrow(fitted))
  expect_equal(p$data$y_adj, expected, tolerance=1e-10)
})

test_that("CAS fixed markers follow categorical display positions and factor ordering", {
  for (ordered in c(FALSE, TRUE)) {
    d = small_mixed
    d$x = if (ordered) factor(d$a, levels=c("yes", "no"), ordered=TRUE) else as.numeric(d$a == "yes")
    m = lme4::lmer(y ~ x + (1 | id), d)
    p = cluster_adjusted_scatter(y ~ x, m, jitter=FALSE)
    built = ggplot2::ggplot_build(p)$data
    expect_identical(levels(p$layers[[2]]$data$x), levels(p$data$x))
    expect_true(inherits(p$layers[[3]]$geom, "GeomPoint"))
    fixed = as.numeric(predict(m, re.form=NA))
    expected = fixed[match(levels(p$data$x), as.character(d$x))]
    expect_equal(as.numeric(built[[2]]$x), seq_along(expected))
    expect_equal(built[[2]]$y, expected)
    expect_equal(built[[3]]$y, expected[match(as.numeric(built[[3]]$x), seq_along(expected))])
  }
})

test_that("CAS uses observation-weighted marginal standardization without refitting", {
  d = small_mixed
  # Unequal cluster sizes and different covariate means distinguish observation
  # weighting from equal-cluster weighting.
  d$z = d$z + match(d$id, unique(d$id))
  d = d[-which(d$id == unique(d$id)[1])[1:3], ]
  m = lme4::lmer(y ~ x*z + I(z^2) + (1 | id), d)
  beta = lme4::fixef(m)
  expected_x = sort(unique(d$x))
  expected_y = beta["(Intercept)"] + beta["x"] * expected_x +
    (beta["z"] + beta["x:z"] * expected_x) * mean(d$z) + beta["I(z^2)"] * mean(d$z^2)
  equal_cluster_mean = mean(tapply(d$z, d$id, mean))
  expect_gt(abs(mean(d$z) - equal_cluster_mean), 1e-4)
  p = cluster_adjusted_scatter(y ~ x, m, jitter=FALSE)
  line = ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(line$x, expected_x)
  expect_equal(line$y, unname(expected_y), tolerance=1e-10)
  reference_y = beta["(Intercept)"] + beta["x"] * expected_x +
    (beta["z"] + beta["x:z"] * expected_x) * mean(d$z) + beta["I(z^2)"] * mean(d$z)^2
  expect_gt(max(abs(line$y - reference_y)), 1e-4)
  equal_cluster_y = beta["(Intercept)"] + beta["x"] * expected_x +
    (beta["z"] + beta["x:z"] * expected_x) * equal_cluster_mean +
    beta["I(z^2)"] * mean(tapply(d$z^2, d$id, mean))
  expect_gt(max(abs(line$y - equal_cluster_y)), 1e-4)
  local_mocked_bindings(compare_fits=function(...) stop("Unexpected comparison/refit"),
                         lm=function(...) stop("Unexpected regression refit"), .package="flexplot")
  expect_equal(cluster_adjusted_scatter(y ~ x, m, jitter=FALSE)$layers[[2]]$data,
               p$layers[[2]]$data)
  m_factor = lme4::lmer(y ~ x*a + (1 | id), small_mixed)
  p_factor = cluster_adjusted_scatter(y ~ x | a, m_factor, jitter=FALSE)
  built = ggplot2::ggplot_build(p_factor)
  curve = built$data[[2]]
  grid = sort(unique(small_mixed$x))
  beta = lme4::fixef(m_factor)
  expect_equal(length(unique(curve$PANEL)), 2)
  for (panel in unique(curve$PANEL)) {
    category = built$layout$layout$a[built$layout$layout$PANEL == panel]
    is_yes = as.character(category) == "yes"
    expected = beta["(Intercept)"] + beta["x"] * grid +
      is_yes * (beta["ayes"] + beta["x:ayes"] * grid)
    expect_equal(curve$y[curve$PANEL == panel], unname(expected), tolerance=1e-10)
  }
})

test_that("CAS rejects unsupported model structures explicitly", {
  multiple = suppressMessages(lme4::lmer(y ~ x + (1 | id) + (1 | b), small_mixed))
  expect_error(compute_cas_from_model(multiple), "exactly one.*grouping factor")
  expect_error(cluster_adjusted_scatter(y ~ x, multiple), "exactly one.*grouping factor")
  slopes = lme4::lmer(y ~ x + (0 + x | id), small_mixed)
  expect_error(compute_cas_from_model(slopes, adjust_re="intercept"), "random-intercept")
  expect_equal(compute_cas_from_model(slopes, adjust_re="none")$y_adj, small_mixed$y)
  expect_error(cluster_adjusted_scatter(y ~ x, lm(y ~ x, small_mixed)), "lmerMod")
  m = lme4::lmer(y ~ x + (x | id), small_mixed)
  expect_error(cluster_adjusted_scatter(z ~ x, m), "outcome.*response")
  expect_error(cluster_adjusted_scatter(y ~ x + missing, m), "model frame")
  reserved = small_mixed
  reserved$pred_fixed = reserved$z
  collision = lme4::lmer(y ~ x + pred_fixed + (1 | id), reserved)
  expect_error(compute_cas_from_model(collision), "metadata would overwrite")
})

test_that("CAS model construction preserves literal names and validates random text", {
  d = small_mixed
  x = "x\\` + I({cas_probe(); y}) #"
  outcome = "outcome with spaces"
  group = "group`\\name"
  names(d)[match(c("x", "y", "id"), names(d))] = c(x, outcome, group)
  f = jamovi_formula(outcome, x)
  random = paste0("(1 | ", escape_spaces_in_formulas(group), ")")
  cas_probe = function() stop("Column name executed")
  p = cluster_adjusted_scatter(f, data=d, random=random, jitter=FALSE)
  m = lme4::lmer(y ~ x + (1 | id), small_mixed)
  expected = cluster_adjusted_scatter(y ~ x, m, jitter=FALSE)
  expect_equal(ggplot2::ggplot_build(p)$data, ggplot2::ggplot_build(expected)$data)
  expect_error(cluster_adjusted_scatter(f, data=d, random="1 = cas_probe((1 | id))"),
               "Unsupported random-effects string")
  fitted = lme4::lmer(y ~ x*a + (1 | id), small_mixed)
  automatic = cluster_adjusted_scatter(y ~ x | a, data=small_mixed, random="(1 | id)", jitter=FALSE)
  supplied = cluster_adjusted_scatter(y ~ x | a, fitted, jitter=FALSE)
  expect_equal(ggplot2::ggplot_build(automatic)$data, ggplot2::ggplot_build(supplied)$data)
})
