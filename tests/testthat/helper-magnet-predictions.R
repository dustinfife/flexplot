# Independent prediction oracle: base-R stratum selection and a fresh local GLM,
# including intercept-only fits when a stratum has only one observed x category.
magnet_within_predictions = function(data, prediction, strata=character()) {
  data = data[complete.cases(data[c("a", "y_bin", strata)]), , drop=FALSE]
  keys = if (length(strata)) unique(prediction[strata]) else data.frame(all=1)
  expected = rep(NA_real_, nrow(prediction))
  for (i in seq_len(nrow(keys))) {
    source = rep(TRUE, nrow(data))
    target = rep(TRUE, nrow(prediction))
    for (variable in strata) {
      source = source & as.character(data[[variable]]) == as.character(keys[[variable]][i])
      target = target & as.character(prediction[[variable]]) == as.character(keys[[variable]][i])
    }
    local = droplevels(data[source, , drop=FALSE])
    formula = if (length(unique(local$a)) > 1) y_bin ~ a else y_bin ~ 1
    fitted = stats::glm(formula, local, family=stats::binomial())
    expected[target] = unname(stats::predict(fitted, prediction[target, , drop=FALSE], type="response"))
  }
  expected
}
