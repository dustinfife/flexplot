# These names are columns created/read inside dplyr data masks, ggplot mappings,
# or tidyselect expressions. They are not lexical/global inputs to calculations.
# Keep actual unresolved references and JASP host callbacks out of this list so
# that check diagnostics continue to expose them.
utils::globalVariables(c(
  # Mixed-model summaries and magrittr's documented placeholder.
  "Value", ".",
  # Logistic bin summaries and overlay rectangles.
  "bin", "bin_means", "bin_mid", "proportion", "xmin", "xmax", "ymin", "ymax",
  "observed_prop", "n_obs", "p_c", "observed_logit", "x_center", "residual",
  # Magnet stack/count frames and model-probability frames.
  "n_0", "n_1", "n_total", "pred_prob", "y_scaled", "X", "stack_pos",
  # Threshold-aligned logistic plot data and joined threshold columns.
  "group_id", "threshold", "x_shifted", "prob", "type",
  # Tidyselect endpoints of prediction confidence-interval columns.
  "prediction.fit", "prediction.upr"
))
