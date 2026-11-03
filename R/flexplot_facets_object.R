# Object-based counterpart to flexplot_panel_variables().
# This intentionally leaves the existing string renderer unchanged.
flexplot_panel_variables_object = function(given, break.me) {
  if (length(given) == 0L || is.na(given[1])) return(NULL)

  given2 = given
  if (length(break.me) > 0L) {
    binned = given2 %in% break.me
    given2[binned] = paste0(given2[binned], "_binned")
  }

  # Preserve the existing orientation: second conditioning variable in rows,
  # first in columns. A blank first variable denotes rows only.
  if (identical(given[1], "") && (length(given2) < 2 || is.na(given2[2]))) {
    stop("A row facet variable is required after a blank first variable.")
  }
  
  if (identical(given[1], "")) {
    row_var = rlang::sym(given2[2])
    return(ggplot2::facet_grid(
      rows = ggplot2::vars(!!row_var),
      labeller = custom.labeler
    ))
  }

  if (length(given2) > 1L && !is.na(given2[2])) {
    return(ggplot2::facet_grid(
      rows = ggplot2::vars(!!rlang::sym(given2[2])),
      cols = ggplot2::vars(!!rlang::sym(given2[1])),
      labeller = custom.labeler
    ))
  }

  ggplot2::facet_grid(
    cols = ggplot2::vars(!!rlang::sym(given2[1])),
    labeller = custom.labeler
  )
}
