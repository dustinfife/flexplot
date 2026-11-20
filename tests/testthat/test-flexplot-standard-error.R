test_that("standard-error whiskers use nonmissing group counts, including sparse cells", {
  values = list(singleton=7, small=c(2, 4), constant=rep(8, 4),
                large=seq(-5, 5, length.out=101))
  data = data.frame(y=unlist(values, use.names=FALSE),
    x=factor(rep(names(values), lengths(values)), levels=rev(names(values))))
  data = rbind(data, data.frame(y=NA_real_, x="small"),
    data.frame(y=99, x=NA_character_))
  expected = lapply(values, function(y) c(mean=mean(y), se=sd(y)/sqrt(length(y))))
  for (sample in c(Inf, 1)) {
    plot = withr::with_seed(64, flexplot(y ~ x, data, spread="sterr",
      jitter=FALSE, sample=sample))
    built = ggplot2::ggplot_build(plot)
    categories = built$layout$panel_scales_x[[1]]$get_limits()
    points = built$data[[2]]
    errors = built$data[[3]]
    rows = categories[round(points$x)]
    means = vapply(expected[rows], function(z) z[["mean"]], numeric(1))
    ses = vapply(expected[rows], function(z) z[["se"]], numeric(1))
    expect_equal(points$y, unname(means))
    expect_equal(errors$ymin, unname(means-ses))
    expect_equal(errors$ymax, unname(means+ses))
    expect_true(is.na(errors$ymin[rows=="singleton"]))
    expect_equal(errors$ymin[rows=="constant"], 8)
    expect_equal(errors$ymax[rows=="constant"], 8)
    # Undefined whiskers are omitted at drawing time; singleton means remain.
    expect_no_warning(ggplot2::ggplotGrob(plot))
  }
})
