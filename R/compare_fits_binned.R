# Reuse the row groups retained by the observed-prediction machinery. There is
# no second bin assignment and no additional call to a prediction method.
compare_fits_binned_observations = function(models, data, frames, outcome) {
  success = NULL
  for (model in models) {
    if (!inherits(model, "glm") || model$family$family != "binomial") {
      stop("Binned observations currently require binomial GLMs.", call.=FALSE)
    }
    response = stats::model.response(compare_fits_model_frame(model))
    if (is.factor(response)) {
      coded = as.numeric(as.character(data[[outcome]]) != levels(response)[1])
    } else {
      coded = data[[outcome]]
      if ((!is.numeric(coded) && !is.logical(coded)) || !all(coded %in% c(0, 1))) {
        stop("Binned observations require individual binary outcomes, not proportions or trial counts.", call.=FALSE)
      }
      coded = as.numeric(coded)
    }
    if (!is.null(success) && !identical(success, coded)) {
      stop("Compared binomial models use different success coding.", call.=FALSE)
    }
    success = coded
  }
  groups = attr(frames[[1]], "observation_groups")
  if (length(groups) != nrow(frames[[1]]) ||
      !identical(sort(unlist(groups, use.names=FALSE)), seq_len(nrow(data)))) {
    stop("Observed prediction rows do not identify the eligible observations.", call.=FALSE)
  }
  for (frame in frames) {
    if (!identical(attr(frame, "observation_groups"), groups)) {
      stop("Compared models do not share the same observed prediction groups.", call.=FALSE)
    }
  }
  summary = frames[[1]]
  summary[c("model", "prediction", "se", "prediction.fit", "prediction.lwr", "prediction.upr")] = NULL
  fields = utils::tail(make.unique(c(names(summary), "proportion", "count")), 2)
  summary[[fields[1]]] = numeric(length(groups))
  for (i in seq_along(groups)) summary[[fields[1]]][i] = mean(success[groups[[i]]])
  summary[[fields[2]]] = lengths(groups)
  attr(summary, "observation_groups") = NULL
  attr(summary, "observed_fields") = fields
  list(summary=summary, response=success)
}

compare_fits_binned_layers = function(p, observed, xvar, show_observations) {
  if (show_observations) {
    mapping = build_inherited_aesthetics(p, observed, base_x=xvar,
                                         base_y=attr(observed, "observed_fields")[1])
    mapping$shape = NULL
    p = p + ggplot2::geom_point(data=observed, mapping=mapping, shape=21,
                                fill="white", size=3, stroke=.8,
                                inherit.aes=FALSE, show.legend=FALSE)
  }
  # Keep the existing model color/group mappings and legend identities. Adding
  # markers at the existing line vertices makes the bin means explicit.
  for (i in seq_along(p$layers)) {
    layer = p$layers[[i]]
    if (!inherits(layer$geom, "GeomLine") || !is.data.frame(layer$data) ||
        !all(c("prediction", "model") %in% names(layer$data))) next
    mapping = layer$mapping[intersect(names(layer$mapping), c("x", "y", "colour", "group"))]
    panels = unique(c(names(p$facet$params$rows), names(p$facet$params$cols)))
    group = logistic_overlay_mapping_variable(layer$mapping$group)
    lines = layer$data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(unique(c(panels, group))))) |>
      dplyr::filter(dplyr::n_distinct(.data[[xvar]]) > 1) |>
      dplyr::ungroup()
    p$layers[[i]] = do.call(ggplot2::ggproto, list(NULL, layer, data=lines))
    p = p + ggplot2::geom_point(data=layer$data, mapping=mapping, size=2,
                                inherit.aes=FALSE, show.legend=FALSE)
  }
  p
}
