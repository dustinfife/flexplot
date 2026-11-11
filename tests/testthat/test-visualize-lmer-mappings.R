lmer_mapping_fixture = function() {
  data = small_mixed
  data$id = factor(data$id)
  data$g = ordered(rep(c("B", "A"), length.out=nrow(data)), levels=c("B", "A"))
  low = data
  low$x = rep(c(2,1,3), length.out=nrow(low))
  list(data=data,
       intercept=lme4::lmer(y ~ x + g + (1 | id), data=data, REML=FALSE),
       slope=lme4::lmer(y ~ x + g + (x | id), data=data, REML=FALSE),
       categorical=lme4::lmer(y ~ g + (1 | id), data=data, REML=FALSE),
       low=lme4::lmer(y ~ x + (1 | id), data=low, REML=FALSE))
}

lmer_mapping_coordinates = function(x, y, panel, colour=NULL, linetype=NULL) {
  result = data.frame(x=as.numeric(x), y=as.numeric(y), PANEL=as.character(panel))
  if (!is.null(colour)) result$colour = as.character(colour)
  if (!is.null(linetype)) result$linetype = as.character(linetype)
  result = result[do.call(order, result), , drop=FALSE]
  rownames(result) = NULL
  result
}

lmer_mapping_grob_labels = function(grob) {
  labels = if (is.null(grob$label)) character() else as.character(grob$label)
  for (child in grob$grobs) labels = c(labels, lmer_mapping_grob_labels(child))
  for (child in grob$children) labels = c(labels, lmer_mapping_grob_labels(child))
  labels
}

lmer_mapping_legend_labels = function(plot) {
  grob = ggplot2::ggplotGrob(plot)
  guides = grob$grobs[grepl("^guide-box", grob$layout$name)]
  unlist(lapply(guides, lmer_mapping_grob_labels), use.names=FALSE)
}

lmer_mapping_examples = function(fixture) {
  plots = list()
  formulas = list(cluster=y ~ x + id, grouped=y ~ x + g | id, panels=y ~ x | id)
  for (model in c("intercept", "slope")) {
    for (name in names(formulas)) {
      plots[[paste(model, name, sep="_")]] = withr::with_seed(417,
        visualize(fixture[[model]], formula=formulas[[name]], plot="model", sample=3, jitter=FALSE))
    }
  }
  plots$categorical_cluster = withr::with_seed(417,
    visualize(fixture$categorical, formula=y ~ g + id, plot="model", sample=3, jitter=FALSE))
  plots$categorical_panels = withr::with_seed(417,
    visualize(fixture$categorical, formula=y ~ g | id, plot="model", sample=3, jitter=FALSE))
  plots$low_cardinality = withr::with_seed(417,
    visualize(fixture$low, formula=y ~ x + id, plot="model", sample=3, jitter=FALSE))
  plots
}

test_that("mixed numeric geometry helpers preserve predictions and all three mapping branches", {
  fixture = lmer_mapping_fixture()
  formulas = list(cluster=y ~ x + id, grouped=y ~ x + g | id, panels=y ~ x | id)
  for (model in c("intercept", "slope")) {
    for (name in names(formulas)) {
      grid = expand.grid(x=seq(-1, 1, length.out=5),
                         id=levels(fixture$data$id)[1:3],
                         g=if (name == "grouped") levels(fixture$data$g) else "B")
      grid$id = factor(grid$id, levels=levels(fixture$data$id))
      grid$g = ordered(grid$g, levels=levels(fixture$data$g))
      random = grid
      random$y = unname(predict(fixture[[model]], grid, re.form=NULL))
      fixed = grid
      fixed$prediction = unname(predict(fixture[[model]], grid, re.form=NA))
      mapping = switch(name,
        cluster=ggplot2::aes(x=x, y=y, colour=id),
        grouped=ggplot2::aes(x=x, y=y, colour=g, linetype=g),
        panels=ggplot2::aes(x=x, y=y))
      plot = ggplot2::ggplot(fixture$data, mapping) + ggplot2::geom_point()
      if (name != "cluster") plot = plot + ggplot2::facet_wrap(ggplot2::vars(id))
      plot = plot + add_random_geom(formulas[[name]], "id", random) +
        add_fixed_geom(formulas[[name]], "id", fixed)
      built = ggplot2::ggplot_build(plot)
      expect_equal(unname(vapply(plot$layers, function(x) class(x$geom)[1], character(1))),
                     c("GeomPoint", "GeomLine", "GeomLine"))
      panels = if (name == "cluster") rep(1, nrow(grid)) else
        match(as.character(grid$id), as.character(built$layout$layout$id))
      colour_scale = built$plot$scales$get_scales("colour")
      colours = switch(name, cluster=colour_scale$map(grid$id),
                         grouped=colour_scale$map(grid$g), panels=rep("black", nrow(grid)))
      linetypes = if (name == "grouped") built$plot$scales$get_scales("linetype")$map(grid$g) else
        rep(1, nrow(grid))
      for (index in 2:3) {
        actual = built$data[[index]]
        expected_y = if (index == 2) random$y else fixed$prediction
        expected_colours = if (index == 3 && name != "grouped") rep("black", nrow(grid)) else colours
        expect_equal(lmer_mapping_coordinates(actual$x, actual$y, actual$PANEL, actual$colour, actual$linetype),
                     lmer_mapping_coordinates(grid$x, expected_y, panels, expected_colours, linetypes))
        groups = unique(actual[c("PANEL", "group")])
        expected_groups = if (name == "grouped") 2L else if (index == 2 && name == "cluster") 3L else 1L
        counts = table(groups$PANEL)
        expect_true(all(counts[counts > 0] == expected_groups))
        expect_setequal(names(counts[counts > 0]), as.character(unique(panels)))
      }
      expect_equal(unique(built$data[[2]]$alpha), .5)
      expect_equal(unique(built$data[[3]]$linewidth), 2)
      labels = lmer_mapping_legend_labels(plot)
      if (name == "panels") {
        expect_length(labels, 0)
      } else {
        expected_levels = if (name == "grouped") c("B", "A") else levels(fixture$data$id)
        expect_true(all(expected_levels %in% labels))
        expect_true((if (name == "grouped") "g" else "id") %in% labels)
      }
    }
  }
  expect_null(add_fixed_geom(y ~ x + id, "id", data.frame()))
})

test_that("programmatic mappings select data columns despite local-name collisions and backticks", {
  for (group in c("variable", "group label")) {
    data = data.frame(terms=rep(c(-1, 0, 1), 2), outcome=seq_len(6), prediction=seq_len(6)+.25,
                        id=factor(rep("panel", 6)))
    data[[group]] = ordered(rep(c("B", "A"), each=3), levels=c("B", "A"))
    formula = make.formula("outcome", list(as.name("terms"), as.name(group)))
    formula[[3]] = call("|", formula[[3]], as.name("id"))
    plot = ggplot2::ggplot(data, ggplot2::aes(x=terms, y=outcome)) +
      add_random_geom(formula, "id", data) + add_fixed_geom(formula, "id", data) +
      ggplot2::facet_wrap(ggplot2::vars(id))
    built = ggplot2::ggplot_build(plot)
    for (i in 1:2) {
      actual = built$data[[i]]
      values = if (i == 1) data$outcome else data$prediction
      colours = built$plot$scales$get_scales("colour")$map(data[[group]])
      expect_equal(lmer_mapping_coordinates(actual$x, actual$y, actual$PANEL, actual$colour),
                   lmer_mapping_coordinates(data$terms, values, rep(1,6), colours))
      expect_length(unique(actual$group), 2)
    }
    expect_true(all(c("B", "A", group) %in% lmer_mapping_legend_labels(plot)))
  }
})

test_that("public lmer visualizations retain model-layer coordinates, facets and ordered categories", {
  fixture = lmer_mapping_fixture()
  plots = lmer_mapping_examples(fixture)
  for (name in names(plots)) {
    plot = plots[[name]]
    built = ggplot2::ggplot_build(plot)
    categorical = startsWith(name, "categorical")
    expect_equal(unname(vapply(plot$layers, function(x) class(x$geom)[1], character(1))),
                   if (categorical) c("GeomPoint", "GeomPoint", "GeomLine", "GeomPoint", "GeomLine") else
                     c("GeomPoint", "GeomLine", "GeomLine"))
    x = if (categorical) "g" else "x"
    for (i in seq.int(2, length(plot$layers))) {
      source = plot$layers[[i]]$data
      actual = built$data[[i]]
      outcome = if ("prediction" %in% names(source)) "prediction" else "y"
      source_x = if (is.factor(source[[x]])) match(as.character(source[[x]]),
                      built$layout$panel_scales_x[[1]]$get_limits()) else source[[x]]
      panels = if ("id" %in% names(built$layout$layout))
        match(as.character(source$id), as.character(built$layout$layout$id)) else rep(1,nrow(source))
      expect_equal(lmer_mapping_coordinates(actual$x, actual$y, actual$PANEL),
                     lmer_mapping_coordinates(source_x, source[[outcome]], panels))
    }
    if (categorical) expect_identical(built$layout$panel_scales_x[[1]]$get_limits(), c("B", "A"))
    if ("id" %in% names(built$layout$layout)) {
      selected = levels(fixture$data$id)[levels(fixture$data$id) %in% plot$data$id]
      expect_identical(as.character(built$layout$layout$id), selected)
    }
    expect_s3_class(ggplot2::ggplotGrob(plot), "gtable")
  }
})
