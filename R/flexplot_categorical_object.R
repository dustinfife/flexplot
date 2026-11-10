# Native ggplot renderer for categorical predictors and a numeric outcome.
    .5
      flexplot_categorical_summary = function(z, spread="quartiles") {
  if (identical(spread, "quartiles")) {
    return(data.frame(
      y=stats::median(z),
      ymin=stats::quantile(z, .25, na.rm=TRUE),
      ymax=stats::quantile(z, .75, na.rm=TRUE)
    ))
  }

  center = mean(z)
  interval = stats::sd(z)
  if (identical(spread, "sterr")) {
    interval = 1.96 * interval / sqrt(length(z) - 1)
  }

  data.frame(y=center, ymin=center - interval, ymax=center + interval)
}

flexplot_categorical_summary_layers = function(spread="quartiles", group=NULL) {
  dodge = if (identical(spread, "quartiles")) .4 else .5
  layers = list(
    ggplot2::stat_summary(fun.data=flexplot_categorical_summary,
                          fun.args=list(spread=spread), geom="point", size=3,
                          position=ggplot2::position_dodge(width=dodge)),
    ggplot2::stat_summary(fun.data=flexplot_categorical_summary,
                          fun.args=list(spread=spread), geom="errorbar",
                          linewidth=1.25, width=.2,
                          position=ggplot2::position_dodge(width=dodge))
  )

  if (is.null(group)) {
    layers[[1]]$aes_params$colour = "#bf0303"
    layers[[2]]$aes_params$colour = "#bf0303"
    return(layers)
  }

  line_dodge = if (identical(spread, "stdev")) .5 else .4
  c(layers, list(ggplot2::stat_summary(
    mapping=ggplot2::aes(group=.data[[group]]),
    fun.data=flexplot_categorical_summary,
    fun.args=list(spread=spread), geom="line",
    position=ggplot2::position_dodge(width=line_dodge)
  )))
}

flexplot_categorical_group_position = function(self, data, params, scales) {
  index = match(as.character(data$colour), self$group_levels)
  offset = .5 * ((index - .5) / length(self$group_levels) - .5)
  for (coordinate in intersect(c("x", "xmin", "xmax"), names(data))) {
    data[[coordinate]] = data[[coordinate]] + offset
  }
  data
}

flexplot_categorical_line_position = function(self, data, params, scales) {
  if (nrow(data) < 2) return(data[FALSE, , drop=FALSE])
  data = ggplot2::ggproto_parent(ggplot2::PositionDodge, self)$compute_panel(data, params, scales)
  categories = match(scales$x$get_limits()[round(data$x)], self$category_levels)
  runs = integer(nrow(data))
  for (group in unique(data$group)) {
    rows = which(data$group == group)
    rows = rows[order(categories[rows])]
    runs[rows] = cumsum(c(TRUE, diff(categories[rows]) > 1))
  }
  # Split after dodging so segments retain the summary markers' group slots.
  paths = interaction(data$group, runs, drop=TRUE)
  data$group = as.integer(paths)
  data[stats::ave(seq_len(nrow(data)), paths, FUN=length) > 1, , drop=FALSE]
}

flexplot_categorical_object = function(data, outcome, axis, plot.type="histogram",
                                        spread="quartiles", suppress_smooth=FALSE,
                                        alpha=.5, jitter=NULL, sample=Inf,
                                        raw.data=TRUE, given=NA) {
  jitter = match_jitter_categorical(jitter)
  points_data = sample.subset(sample, data)
  x = axis[1]
  group = if (length(axis) > 1) axis[2] else NULL
  n_groups = if (is.null(group)) 0L else length(unique(data[[group]]))

  if (is.null(group)) {
    mapping = ggplot2::aes(x=.data[[x]], y=.data[[outcome]])
    points = geom_jitterd(data=points_data, alpha=raw.alph.func(raw.data, alpha),
                          width=jitter[1], height=jitter[2])
  } else {
    mapping = ggplot2::aes(
      x=.data[[x]], y=.data[[outcome]],
      color=.data[[group]]
    )
    if (n_groups > 6) {
      flexplot_high_cardinality_message()
    } else {
      mapping$shape = mapping$colour
      mapping$linetype = mapping$colour
    }
    
    points = ggplot2::geom_point(
      data=points_data, alpha=raw.alph.func(raw.data, alpha),
      position=position_jitterdodged(
        jitter.width=jitter[1], jitter.height=jitter[2], dodge.width=.5
      )
    )
  }

  p = ggplot2::ggplot(data, mapping) + points + ggplot2::theme_bw() +
    ggplot2::theme(text=ggplot2::element_text(size=14))

  if (n_groups > 6) p = p + ggplot2::guides(colour="none")

  if (identical(plot.type, "line")) {
    if (!is.null(group) && suppress_smooth) return(p)
    line_mapping = if (is.null(group)) ggplot2::aes(group=1) else
      ggplot2::aes(group=.data[[group]])
    position = ggplot2::ggproto(
      NULL, ggplot2::position_dodge(width=if (spread == "quartiles") .4 else .5),
      category_levels=flexplot_sampling_levels(data[[x]]),
      compute_panel=flexplot_categorical_line_position
    )
    line = ggplot2::stat_summary(mapping=line_mapping,
      fun.data=flexplot_categorical_summary, fun.args=list(spread=spread),
      geom="line", position=position)
    if (is.null(group)) return(p + line)
    return(p + flexplot_categorical_summary_layers(spread, group)[1:2] + line)
  }

  if (!is.null(group)) {
    if (plot.type %in% c("boxplot", "violin")) {
      group_levels = flexplot_sampling_levels(data[[group]])
      position = ggplot2::ggproto(
        NULL, ggplot2::Position, group_levels=group_levels,
        compute_panel=flexplot_categorical_group_position
      )
      width = .45 / length(group_levels)
      if (identical(plot.type, "boxplot")) {
        return(p + ggplot2::geom_boxplot(alpha=.1, width=width, position=position,
                                        linetype="solid"))
      }
      # Densities require two observations per displayed x/group/facet cell.
      facets = vapply(given[!is.na(given) & nzchar(given)], resolve_binned_var,
                        character(1), data=data, return.name=TRUE)
      violin_data = data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(c(x, group, facets)))) |>
        dplyr::filter(dplyr::n() > 1) |>
        dplyr::ungroup()
      return(p + ggplot2::geom_violin(data=violin_data, alpha=.1,
                                      width=width, position=position, linetype="solid"))
    }
    if (suppress_smooth) return(p)
    return(p + flexplot_categorical_summary_layers(spread, group))
  }

  if (identical(plot.type, "boxplot")) {
    p = p + ggplot2::geom_boxplot(alpha=.1)
  } else if (identical(plot.type, "violin")) {
    p = p + ggplot2::geom_violin(alpha=.1)
  } else if (!suppress_smooth) {
    p = p + flexplot_categorical_summary_layers(spread)
  }

  p
}
