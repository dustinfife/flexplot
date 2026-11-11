test_that("mixed-model ICC and design effects preserve their existing calculations", {
  missing = small_mixed
  missing$y[1] = NA_real_
  missing$x[2] = NA_real_
  samples = list(small_mixed, small_mixed[-c(1,2,3), ], missing)
  for (data in samples) {
    model = lme4::lmer(y ~ x + (1 | id), data=data, REML=FALSE)
    variance = as.numeric(lme4::VarCorr(model)$id[1,1])
    expected_icc = variance / (variance + stats::sigma(model)^2)
    clusters = nrow(lme4::ranef(model)$id)
    # The current implementation counts coefficient entries across clusters,
    # including fixed slopes. Preserve that convention in this cleanup.
    counted_units = clusters * length(lme4::fixef(model))
    expected_design = 1 + expected_icc*(stats::nobs(model)/counted_units - 1)
    result = icc(model)
    expect_identical(names(result), c("icc", "design.effect"))
    expect_type(result$icc, "double")
    expect_type(result$design.effect, "double")
    expect_equal(result$icc, expected_icc, tolerance=1e-12)
    expect_equal(result$design.effect, expected_design, tolerance=1e-12)
  }
  expect_identical(names(formals(icc)), "model")
  expect_true("icc" %in% getNamespaceExports("flexplot"))
  expect_identical(getExportedValue("flexplot", "icc"), icc)
})

test_that("linear mixed-model estimates retain baseline ICC and original fit components", {
  model = lme4::lmer(y ~ x + (1 | id), data=small_mixed, REML=FALSE)
  baseline = lme4::lmer(y ~ 1 + (1 | id), data=small_mixed, REML=FALSE)
  variance = as.numeric(lme4::VarCorr(baseline)$id[1,1])
  expected_icc = variance / (variance + stats::sigma(baseline)^2)
  clusters = nrow(lme4::ranef(baseline)$id)
  expected = c(icc=expected_icc,
                design.effect=1 + expected_icc*(stats::nobs(baseline)/clusters - 1))
  result = estimates(model)
  expect_s3_class(result, "lmer_estimates")
  expect_identical(names(result), c("fixed", "r.squared", "rand", "icc"))
  expect_equal(result$fixed, lme4::fixef(model))
  expect_equal(as.data.frame(result$rand), as.data.frame(lme4::VarCorr(model)))
  expect_equal(result$icc, expected, tolerance=1e-12)
})

test_that("generalized mixed-model estimates preserve the existing component-ratio ICC", {
  data = small_mixed
  data$binary = as.integer(data$y > median(data$y))
  model = lme4::glmer(binary ~ x + (1 | id), data=data, family=binomial)
  baseline = lme4::glmer(binary ~ 1 + (1 | id), data=data, family=binomial)
  # Preserve the existing VarCorr ratio without adding latent observation variance.
  variances = as.data.frame(lme4::VarCorr(baseline))$vcov
  expected_icc = variances[1]/sum(variances)
  clusters = nrow(lme4::ranef(baseline)$id)
  expected = c(icc=expected_icc,
                design.effect=1 + expected_icc*(stats::nobs(baseline)/clusters - 1))
  expect_equal(unlist(icc(baseline)), expected, tolerance=1e-12)
  result = estimates(model)
  expect_s3_class(result, "lmer_estimates")
  expect_equal(result$fixed, lme4::fixef(model))
  expect_equal(result$icc, expected, tolerance=1e-12)
  expect_null(result$r.squared)
})
