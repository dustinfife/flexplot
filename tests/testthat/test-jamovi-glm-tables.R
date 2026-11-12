jamovi_glm_analysis = function(data, predictors) {
  options = flexplot:::glmbasicOptions$new(out="y", preds=predictors,
    graphic=FALSE, graphicassump=FALSE, estimates=TRUE)
  flexplot:::glmbasicClass$new(options=options, data=data)
}

test_that("complete jamovi GLM runs report numeric coefficients and existing Wald intervals", {
  skip_if_not_installed("jmvcore")
  for (missing in c(FALSE, TRUE)) {
    data = small
    if (missing) {
      data$y[3] = NA
      data$x[4] = NA
      data$a[5] = NA # Unused by this model; must not exclude another observation.
    }
    data$unused = NA_real_
    model = lm(y ~ x + z, data=data)
    analysis = jamovi_glm_analysis(data, c("x", "z"))
    expect_no_warning({ completed = analysis$run() })
    expect_true(completed)
    table = analysis$results$glmcat$asDF
    expect_identical(names(table), c("var", "levels", "means", "lower", "upper"))
    expect_identical(table$var, c("Numeric Variables (Estimates reported are slopes/intercepts)",
                                  "Intercept", "Slope: x", "Slope: z"))
    expect_true(all(is.na(table[1, c("levels", "means", "lower", "upper")])))
    coefficients = summary(model)$coefficients
    expect_equal(as.numeric(table$means[-1]), unname(coef(model)), tolerance=1e-10)
    # The numeric summary already uses 1.96, not a t critical value. Preserve it.
    expect_equal(as.numeric(table$lower[-1]), unname(round(coef(model)-1.96*coefficients[, 2], 2)))
    expect_equal(as.numeric(table$upper[-1]), unname(round(coef(model)+1.96*coefficients[, 2], 2)))
    unrounded = estimates.lm(model, mc=FALSE)$numbers.summary
    expect_equal((unrounded$upper-unrounded$lower)/(2*1.96), unname(coefficients[, 2]), tolerance=1e-10)
    expect_equal(nrow(analysis$results$diff$asDF), 0)
    rsq = analysis$results$rsq$asDF
    expect_equal(rsq$Estimate[rsq$var == "model"], round(summary(model)$r.squared, 4))
    # Preserve the existing sequential sums-of-squares effect-size output.
    anova = stats::anova(model)
    expected = anova$`Sum Sq`[1:2]/sum(anova$`Sum Sq`)
    expect_equal(rsq$Estimate[match(c("x", "z"), rsq$var)], expected, tolerance=1e-10)
    expect_identical(all.vars(analysis$results$plot$state$formula), c("y", "x", "z"))
    expect_identical(names(analysis$results$plot$state$data), c("y", "x", "z"))
    for (variable in c("y", "x", "z")) {
      expect_equal(analysis$results$plot$state$data[[variable]], data[[variable]])
    }
  }
})

test_that("complete jamovi GLM runs preserve factor means and covariate-adjusted tables", {
  skip_if_not_installed("jmvcore")
  for (adjusted in c(FALSE, TRUE)) {
    for (missing in c(FALSE, TRUE)) {
      data = small
      data$a = factor(data$a, levels=rev(levels(data$a)))
      if (missing) {
        data$y[3] = NA
        data$a[5] = NA
        data$x[4] = NA # Relevant only when adjusted=TRUE.
      }
      data$unused = NA_real_
      model = lm(if (adjusted) y ~ x + a else y ~ a, data=data)
      analysis = jamovi_glm_analysis(data, if (adjusted) c("x", "a") else "a")
      expect_no_warning({ completed = analysis$run() })
      expect_true(completed)
      table = analysis$results$glmcat$asDF
      expect_identical(names(table), c("var", "levels", "means", "lower", "upper"))
      expect_identical(table$var[1], "Factors (Estimates reported are means)")
      levels = levels(model$model$a)
      profile = data.frame(a=factor(levels, levels=levels))
      if (adjusted) profile$x = mean(model$model$x)
      predicted = predict(model, profile, se.fit=TRUE)
      expected = predict(model, profile, interval="confidence")
      expect_identical(table$levels[2:3], levels)
      expect_equal(as.numeric(table$means[2:3]), unname(round(expected[, "fit"], 2)))
      expect_equal(as.numeric(table$lower[2:3]), unname(round(expected[, "lwr"], 2)))
      expect_equal(as.numeric(table$upper[2:3]), unname(round(expected[, "upr"], 2)))
      unrounded = estimates.lm(model, mc=FALSE)$factor.summary
      expect_equal((unrounded$upper-unrounded$lower)/(2*qt(.975, df.residual(model))),
                     unname(predicted$se.fit), tolerance=1e-10)
      if (!adjusted) {
        observed = tapply(model$model$y, model$model$a, mean)
        expect_equal(as.numeric(table$means[2:3]), as.numeric(round(observed[levels], 2)))
      }

      differences = analysis$results$diff$asDF
      expect_identical(names(differences), c("variables", "comparison", "diff", "lower", "upper", "cohensd"))
      expect_equal(nrow(differences), 1)
      difference = unname(expected[2, "fit"]-expected[1, "fit"])
      counts = table(model$model$a)
      # Preserve the existing Tukey-scaled difference interval calculation.
      width = qtukey(.95, 2, df.residual(model))*sigma(model)*sqrt(sum(1/counts))
      expect_identical(differences$comparison, paste(levels[2], levels[1], sep="-"))
      expect_equal(differences$diff, round(difference, 2))
      expect_equal(differences$lower, round(difference-width, 2))
      expect_equal(differences$upper, round(difference+width, 2))
      expect_equal(differences$cohensd, round(difference/sigma(model), 2))

      if (adjusted) {
        expect_equal(nrow(table), 6)
        expect_identical(table$var[4:6], c("Numeric Variables (Estimates reported are slopes/intercepts)",
                                            "Intercept", "Slope: x"))
        expect_true(all(is.na(table[4, c("levels", "means", "lower", "upper")])))
        numeric = summary(model)$coefficients[c("(Intercept)", "x"), ]
        expect_equal(as.numeric(table$means[5:6]), unname(numeric[, 1]), tolerance=1e-10)
        expect_equal(as.numeric(table$lower[5:6]), unname(round(numeric[, 1]-1.96*numeric[, 2], 2)))
        expect_equal(as.numeric(table$upper[5:6]), unname(round(numeric[, 1]+1.96*numeric[, 2], 2)))
      } else {
        expect_equal(nrow(table), 3)
        expect_false(any(grepl("Numeric Variables", table$var), na.rm=TRUE))
      }
      expect_equal(analysis$results$rsq$asDF$Estimate[1], round(summary(model)$r.squared, 4))
    }
  }
})
