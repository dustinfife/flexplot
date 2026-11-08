test_that("magnet group and panel fits can display opposite categorical relationships", {
  d = small[rep(seq_len(nrow(small)), each=8), ]
  d$a = ordered(d$a, levels=c("b", "a"))
  d$b = factor(d$b, levels=c("z", "x", "y"))
  d$panel = ordered(ifelse(d$z > median(small$z), "high", "low"), levels=c("high", "low"))
  cell = interaction(d$a, d$b, d$panel)
  index = ave(seq_len(nrow(d)), cell, FUN=seq_along)
  size = ave(seq_len(nrow(d)), cell, FUN=length)
  probability = ifelse((d$a == "a") == (d$panel == "high"), .8, .2)
  d$y_bin = as.integer(index <= round(size * probability))
  p = flexplot(y_bin ~ a + b | panel, d, jitter=FALSE)
  fitted = p$layers[[3]]$data
  expect_equal(fitted$pred_prob, magnet_within_predictions(d, fitted, c("b", "panel")),
                 tolerance=1e-7, scale=1)
  proportions = aggregate(y_bin ~ a + b + panel, d, mean)
  rows = match(paste(fitted$a, fitted$b, fitted$panel),
                 paste(proportions$a, proportions$b, proportions$panel))
  expect_equal(fitted$pred_prob, proportions$y_bin[rows], tolerance=1e-7, scale=1)
  pooled = glm(y_bin ~ a + b + panel, d, family=binomial())
  expect_gt(max(abs(fitted$pred_prob - predict(pooled, fitted, type="response"))), .1)
  sampled = withr::with_seed(82, flexplot(y_bin ~ a + b | panel, d,
                                         jitter=FALSE, sample=1, ghost.line="red",
                                         ghost.reference=list(panel="high")))
  expect_equal(sampled$layers[[3]]$data, fitted)
  expect_equal(sampled$layers[[4]]$data, p$layers[[4]]$data)
  expect_equal(nrow(sampled$layers[[2]]$data), 1)
  expect_identical(levels(fitted$a), levels(d$a))
  expect_true(is.ordered(fitted$a))
  built = ggplot2::ggplot_build(sampled)
  line = which(vapply(sampled$layers, function(layer) {
    inherits(layer$geom, "GeomLine") && identical(layer$aes_params$colour, "red")
  }, logical(1)))
  reference = fitted[fitted$panel == "high", ]
  for (panel in unique(built$data[[line]]$PANEL)) {
    ghost = built$data[[line]][built$data[[line]]$PANEL == panel, ]
    expect_equal(sort(ghost$y), sort(reference$pred_prob))
    expect_equal(length(unique(ghost$group)), nlevels(d$b))
  }
  expect_no_warning(ggplot2::ggplotGrob(sampled))
})

test_that("sparse local magnet fits retain observed categories and finite boundary probabilities", {
  d = data.frame(
    a=ordered(c("A", "A", "B", "B", "A", "A", "B", "C", "A", "C", "A", "B", "C"),
                levels=c("C", "A", "B", "unused")),
    b=factor(rep(c("mixed", "one_x", "zeros", "ones", "separated", "singleton"),
                   c(4, 2, 2, 2, 2, 1))),
    panel=factor(c(rep("first", 6), rep("second", 7))),
    y_bin=c(0, 1, 0, 1, 0, 1, 0, 0, 1, 1, 0, 1, 1)
  )
  d = rbind(d, d[c(1, 2), ])
  d$y_bin[nrow(d) - 1] = NA
  d$panel[nrow(d)] = NA
  eligible = droplevels(d[complete.cases(d), ])
  expect_no_warning({ p = flexplot(y_bin ~ a + b | panel, d, jitter=FALSE) })
  fitted = p$layers[[3]]$data
  expect_equal(fitted$pred_prob, magnet_within_predictions(eligible, fitted, c("b", "panel")),
                 tolerance=1e-7, scale=1)
  observed = unique(eligible[c("a", "b", "panel")])
  expect_setequal(paste(fitted$a, fitted$b, fitted$panel), paste(observed$a, observed$b, observed$panel))
  expect_true(all(is.finite(fitted$pred_prob) & fitted$pred_prob >= 0 & fitted$pred_prob <= 1))
  expect_equal(fitted$pred_prob[fitted$b == "zeros"], rep(0, 2))
  expect_equal(fitted$pred_prob[fitted$b == "ones"], rep(1, 2))
  expect_equal(fitted$pred_prob[fitted$b == "one_x"], .5)
  expect_equal(fitted$pred_prob[fitted$b == "singleton"], 1)
  expect_equal(nrow(p$layers[[2]]$data), nrow(eligible))
  expect_equal(sum(p$layers[[4]]$data$n_total), nrow(eligible))
  expect_identical(levels(fitted$a), levels(d$a))
  built = ggplot2::ggplot_build(p)
  expect_identical(built$layout$panel_scales_x[[1]]$get_limits(), levels(eligible$a))
  expect_no_warning(ggplot2::ggplotGrob(p))
})
