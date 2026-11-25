# Rename symbols in an already trusted formula, never parse dataset names.
lm_names_formula = function(formula, mapping) {
  replace = function(expr) {
    if (is.symbol(expr) && as.character(expr) %in% names(mapping)) return(as.name(mapping[[as.character(expr)]]))
    if (is.call(expr)) return(as.call(lapply(as.list(expr), replace)))
    expr
  }
  structure(replace(formula), class="formula", .Environment=environment(formula))
}

expect_lm_names_statistics = function(actual, expected) {
  expect_equal(actual$r.squared, expected$r.squared)
  expect_equal(unname(actual$semi.p), unname(expected$semi.p), tolerance=1e-10)
  expect_equal(actual$correlation, expected$correlation)
  expect_equal(actual$sigma, expected$sigma)
  for (table in c("numbers.summary", "factor.summary", "difference.matrix", "model.comparison")) {
    if (!is.data.frame(expected[[table]])) {
      expect_equal(actual[[table]], expected[[table]])
      next
    }
    expect_identical(names(actual[[table]]), names(expected[[table]]))
    columns = vapply(expected[[table]], is.numeric, logical(1))
    expect_equal(actual[[table]][columns], expected[[table]][columns], tolerance=1e-10)
    if (table == "difference.matrix") {
      expect_equal(actual[[table]]$comparison, expected[[table]]$comparison)
      expect_equal(unname(attr(actual[[table]], "adjustments")),
        unname(attr(expected[[table]], "adjustments")))
    }
  }
}

test_that("literal numeric and categorical names preserve all supplementary LM statistics", {
  captured = new.env(parent=emptyenv())
  probe = function() {captured$executed=TRUE; stop("NAME_EXECUTED")}
  rlang::local_bindings(lm_names_probe=probe, .env=globalenv())
  names = c("has space", "a+b", "tick`name`", "back\\slash", "log(y)", "1", "a:b",
    "x\\`+I({lm_names_probe();y}) #")
  for (variable in c("x", "b")) {
    for (name in names) {
      captured$executed = FALSE
      data = small
      data$y[2] = NA
      data$z[7] = NA
      contrasts(data$b) = contr.helmert(3)
      mapping = stats::setNames(name, variable)
      formula = if (variable == "x") y ~ x + z + b else y ~ b + x + z
      reference = lm(formula, data)
      expected = estimates(reference)
      names(data)[names(data) == variable] = name
      model = lm(lm_names_formula(formula, mapping), data)
      before = coef(model)
      expect_no_error({actual = estimates(model)})
      expect_lm_names_statistics(actual, expected)
      expect_identical(coef(model), before)
      expect_true(name %in% c(actual$numbers, actual$factors))
      expect_false(captured$executed)
    }
  }
})

test_that("unusual names preserve interaction and polynomial model spaces", {
  data = small
  data$y[3] = NA
  contrasts(data$b) = contr.sum(3)
  formulas = list(y ~ x*z+b, y ~ x*b+z, y ~ a*b+x,
    y ~ z+x+I(x^2), y ~ poly(x,2)+z)
  mapping = c(x="has space", z="log(y)", a="tick`name`", b="a+b")
  renamed = data
  names(renamed)[match(names(mapping), names(renamed))] = unname(mapping)
  for (formula in formulas) {
    model = lm(lm_names_formula(formula, mapping), renamed)
    reference = lm(formula, data)
    expect_lm_names_statistics(estimates(model, mc=FALSE), estimates(reference, mc=FALSE))
    expect_equal(unname(coef(model)), unname(coef(reference)), tolerance=1e-12)
    expect_equal(unname(vcov(model)), unname(vcov(reference)), tolerance=1e-12)
  }
})

test_that("one-way nonstandard factor names retain unadjusted t uncertainty", {
  data = small
  names(data)[names(data)=="b"] = "a+b"
  model = lm(y ~ `a+b`, data)
  result = estimates(model, mc=FALSE)$difference.matrix
  reference = estimates(lm(y ~ b,small),mc=FALSE)$difference.matrix
  expect_equal(result$difference,reference$difference,tolerance=1e-12)
  expect_equal(result$lower,reference$lower,tolerance=1e-12)
  expect_equal(result$upper,reference$upper,tolerance=1e-12)
  expect_identical(attr(result,"adjustments")[["a+b"]],"Pointwise t")
})

test_that("actual jamovi analyses replace supplementary rows for nonstandard names", {
  skip_if_not_installed("jmvcore")
  data = small
  names(data)[match(c("x","b"),names(data))] = c("has space","a+b")
  options = flexplot:::glmbasicOptions$new(out="y", preds=c("has space","a+b"), graphic=FALSE)
  analysis = flexplot:::glmbasicClass$new(options=options, data=NULL)
  reader = jamovi_reporting_read
  environment(reader) = list2env(list(data=data), parent=environment(reader))
  analysis$.setReadDatasetSource(reader)
  analysis$.setReadDatasetHeaderSource(reader)
  expect_true(analysis$run())
  for (selected in list(list("has space","a+b"),list("has space"),
                        list("has space","a+b",c("has space","a+b")),list("has space","a+b"))) {
    expect_true(jamovi_reporting_update(analysis, list(modelTerms=selected)))
    formula = flexplot:::jamovi_glm_formula("y", c("has space","a+b"), selected)
    model = lm(formula, data)
    expected = estimates(model, mc=FALSE)
    expect_null(analysis$results$glmcat$notes$summaries)
    jamovi_expect_lm_coefficients(analysis, model)
    rows = analysis$results$rsq$asDF
    expect_equal(rows$Estimate[match(names(expected$semi.p),rows$var)], as.numeric(expected$semi.p))
    expect_equal(analysis$results$diff$rowCount,
      if (is.data.frame(expected$difference.matrix)) nrow(expected$difference.matrix) else 0L)
    expect_equal(analysis$results$factorMeans$rowCount,
      if (is.data.frame(expected$factor.summary)) nrow(expected$factor.summary) else 0L)
    reopened = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
      out="y", preds=c("has space","a+b"), modelTerms=selected, graphic=FALSE), data=data)
    expect_true(reopened$run())
    expect_equal(reopened$results$rsq$asDF, analysis$results$rsq$asDF)
    expect_identical(reopened$results$diff$rowCount, analysis$results$diff$rowCount)
    if (analysis$results$diff$rowCount > 0L) {
      expect_equal(reopened$results$diff$asDF, analysis$results$diff$asDF)
    }
  }
})
