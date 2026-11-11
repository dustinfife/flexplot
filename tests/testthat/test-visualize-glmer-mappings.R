glmer_mapping_fixture = function() {
 data = expand.grid(x=seq(-1.5,1.5,length.out=9),
                    id=levels(factor(small_mixed$id)), g=c('B','A'), replicate=seq_len(8))
 data$id = factor(data$id)
 data$g = ordered(data$g,levels=c('B','A'))
 cluster = as.integer(data$id)
 eta = .2 + .4*data$x + .25*(data$g=='A') + .7*sin(cluster) + .6*cos(cluster)*data$x
 binary = data
 binary$y = as.integer((data$replicate-.5)/8 < plogis(eta))
 count = data
 lambda = exp(.7 + .3*data$x + .25*(data$g=='A') + .5*sin(cluster) + .3*cos(cluster)*data$x)
 count$y = qpois((data$replicate-.5)/8,lambda)
 low_binary = binary
 low_binary$x = rep(c(3,1,2),length.out=nrow(low_binary))
 alias = count
 alias$outcome = alias$y
 alias$terms = alias$g
 low = count
 low$x = rep(c(3,1,2),length.out=nrow(low))
 list(binary=binary,count=count,low=low,
      binomial_intercept=lme4::glmer(y~x+g+(1|id),data=binary,family=binomial),
      binomial_slope=lme4::glmer(y~x+g+(x|id),data=binary,family=binomial),
      poisson_intercept=lme4::glmer(y~x+g+(1|id),data=count,family=poisson),
      poisson_slope=lme4::glmer(y~x+g+(x|id),data=count,family=poisson),
      binomial_low=lme4::glmer(y~x+g+(1|id),data=low_binary,family=binomial),
      alias=lme4::glmer(outcome~x+terms+(1|id),data=alias,family=poisson),
      poisson_low=lme4::glmer(y~x+g+(1|id),data=low,family=poisson))
}

glmer_mapping_cases = function(fixture, include_partial=FALSE) {
  cases = list()
  for (model in c("binomial_intercept", "binomial_slope", "poisson_intercept", "poisson_slope")) {
    cases[[paste0(model, "_numeric")]] = list(model=fixture[[model]], formula=y ~ x + id | g,
                                               type="numeric", category="g", outcome="y")
    cases[[paste0(model, "_categorical")]] = list(model=fixture[[model]], formula=y ~ g + id,
                                                   type="categorical", category="g", outcome="y")
  }
  for (model in c("binomial_low", "poisson_low")) {
    cases[[model]] = list(model=fixture[[model]], formula=y ~ x + id | g,
                           type="numeric", category="g", outcome="y")
  }
  cases$poisson_numeric_conditioning = list(model=fixture$poisson_intercept,
    formula=y ~ g + id | x, type="categorical", category="g", outcome="y")
  cases$alias_numeric = list(model=fixture$alias, formula=outcome ~ x + id | terms,
                              type="numeric", category="terms", outcome="outcome")
  cases$alias_categorical = list(model=fixture$alias, formula=outcome ~ terms + id,
                                  type="categorical", category="terms", outcome="outcome")
  if (include_partial) {
    # Record these as baseline controls, not assertions that incomplete support
    # is statistically correct. The binomial overlay lacks its numeric facet ID.
    cases$existing_binomial_numeric_facet_gap = list(model=fixture$binomial_intercept,
      formula=y ~ g + id | x, type="partial", category="g", outcome="y")
    for (model in c("binomial_intercept", "poisson_intercept")) {
      cases[[paste0(model, "_control")]] = list(model=fixture[[model]], formula=y ~ x | id,
                                                type="control", category="g", outcome="y")
    }
  }
  cases
}

glmer_mapping_plot = function(case) {
  suppressMessages(withr::with_seed(417,
    visualize(case$model, formula=case$formula, plot="model", sample=3, jitter=FALSE)))
}

glmer_mapping_coordinates = function(x, y, panel, colour) {
  result = data.frame(x=as.numeric(x), y=as.numeric(y), PANEL=as.character(panel),
                       colour=as.character(colour))
  result = result[do.call(order, result), , drop=FALSE]
  rownames(result) = NULL
  result
}

glmer_mapping_expected = function(model, source, plot, category, random) {
  frame = stats::model.frame(model)
  n = nrow(source)
  if ("x" %in% names(source)) {
    x = if (is.factor(source$x)) as.numeric(as.character(source$x)) else source$x
  } else if ("x_binned" %in% names(source)) {
    medians = tapply(plot$data$x, plot$data$x_binned, median)
    x = unname(medians[as.character(source$x_binned)])
  } else {
    # The fixture's x grid is balanced in every cluster/category. Thus these
    # equally weighted coordinates independently reproduce full-data averaging.
    x = sort(unique(frame$x))
  }
  average = !any(c("x", "x_binned") %in% names(source))
  rows = if (average) rep(seq_len(n), each=length(x)) else seq_len(n)
  profile = frame[rep(1, length(rows)), , drop=FALSE]
  profile$x = if (average) rep(x, n) else x
  profile[[category]] = ordered(as.character(source[[category]][rows]), levels=levels(frame[[category]]))
  id = if (random) as.character(source$id[rows]) else rep(levels(frame$id)[1], length(rows))
  profile$id = factor(id, levels=levels(frame$id))
  prediction = unname(predict(model, profile, type="response", re.form=if (random) NULL else NA))
  if (average) return(rowMeans(matrix(prediction, nrow=n, byrow=TRUE)))
  prediction
}

glmer_mapping_grob_labels = function(grob) {
  labels = if (is.null(grob$label)) character() else as.character(grob$label)
  for (child in grob$grobs) labels = c(labels, glmer_mapping_grob_labels(child))
  for (child in grob$children) labels = c(labels, glmer_mapping_grob_labels(child))
  labels
}

glmer_mapping_legend_labels = function(plot) {
  grob = ggplot2::ggplotGrob(plot)
  unlist(lapply(grob$grobs[grepl("^guide-box", grob$layout$name)], glmer_mapping_grob_labels), use.names=FALSE)
}

test_that("GLMM numeric mappings retain original-model probability and count curves", {
  cases = glmer_mapping_cases(glmer_mapping_fixture())
  for (case in cases[vapply(cases, function(x) x$type == "numeric", logical(1))]) {
    plot = glmer_mapping_plot(case)
    built = ggplot2::ggplot_build(plot)
    indices = (length(plot$layers)-1L):length(plot$layers)
    expect_true(all(vapply(plot$layers[indices], function(x) inherits(x$geom, "GeomLine"), logical(1))))
    for (i in indices) {
      random = i == max(indices)
      source = plot$layers[[i]]$data
      actual = built$data[[i]]
      expected = glmer_mapping_expected(case$model, source, plot, case$category, random)
      expect_equal(source[[if (random) case$outcome else "prediction"]], expected, tolerance=1e-10)
      panel = match(as.character(source[[case$category]]), as.character(built$layout$layout[[case$category]]))
      x = if (is.factor(source$x)) match(as.character(source$x), built$layout$panel_scales_x[[1]]$get_limits()) else source$x
      colours = if (random) built$plot$scales$get_scales("colour")$map(source$id) else rep("black", nrow(source))
      expect_equal(glmer_mapping_coordinates(actual$x, actual$y, actual$PANEL, actual$colour),
                     glmer_mapping_coordinates(x, expected, panel, colours), tolerance=1e-10)
      paths = unique(actual[c("PANEL", "group")])
      expect_true(all(table(paths$PANEL) == if (random) 3 else 1))
    }
    expect_equal(unique(built$data[[indices[1]]]$linewidth), 2)
    expect_equal(unique(built$data[[indices[1]]]$linetype), 1)
    expect_identical(as.character(built$layout$layout[[case$category]]), c("B", "A"))
    labels = glmer_mapping_legend_labels(plot)
    expect_true(all(as.character(unique(plot$data$id)) %in% labels))
    expect_true("id" %in% labels)
    expect_false(any(c("fixed effects", "random effects") %in% labels))
  }
})

test_that("GLMM categorical mappings retain fitted points, lines, ordering and facets", {
  cases = glmer_mapping_cases(glmer_mapping_fixture())
  for (case in cases[vapply(cases, function(x) x$type == "categorical", logical(1))]) {
    plot = glmer_mapping_plot(case)
    built = ggplot2::ggplot_build(plot)
    indices = (length(plot$layers)-3L):length(plot$layers)
    expect_equal(unname(vapply(plot$layers[indices], function(x) class(x$geom)[1], character(1))),
                   c("GeomPoint", "GeomLine", "GeomPoint", "GeomLine"))
    for (i in indices) {
      random = i >= indices[3]
      source = plot$layers[[i]]$data
      actual = built$data[[i]]
      expected = glmer_mapping_expected(case$model, source, plot, case$category, random)
      expect_equal(source[[case$outcome]], expected, tolerance=1e-10)
      panel = if ("x_binned" %in% names(source))
        match(source$x_binned, built$layout$layout$x_binned) else rep(1, nrow(source))
      x = match(as.character(source[[case$category]]), built$layout$panel_scales_x[[1]]$get_limits())
      # Magnet bases map colour per layer, not globally: their existing model
      # overlays therefore retain default black rather than inheriting id colours.
      colours = if (random && !is.null(plot$mapping$colour))
        built$plot$scales$get_scales("colour")$map(source$id) else rep("black", nrow(source))
      expect_equal(glmer_mapping_coordinates(actual$x, actual$y, actual$PANEL, actual$colour),
                     glmer_mapping_coordinates(x, expected, panel, colours), tolerance=1e-10)
      if (inherits(plot$layers[[i]]$geom, "GeomLine")) {
        paths = unique(actual[c("PANEL", "group")])
        expect_true(all(table(paths$PANEL) == if (random) 3 else 1))
        expect_equal(unique(actual$linetype), if (random) 2 else 1)
        expect_equal(unique(actual$linewidth), if (random) .5 else 2)
      } else {
        expect_equal(unique(actual$size), if (random) .5 else 3)
      }
    }
    expect_identical(built$layout$panel_scales_x[[1]]$get_limits(), c("B", "A"))
    if ("x_binned" %in% names(built$layout$layout)) expect_equal(nrow(built$layout$layout), 3)
    labels = glmer_mapping_legend_labels(plot)
    expect_true(all(as.character(unique(plot$data$id)) %in% labels))
    expect_true("id" %in% labels)
  }
})
