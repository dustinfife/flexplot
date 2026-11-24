# Native cell footnotes only: do not depend on unsupported heading markers.
jamovi_table_footnote = function(table, columns, note, rows=NULL, missing=FALSE) {
  if (!length(note) || !nzchar(note) || table$rowCount==0L) return(invisible(NULL))
  if (is.null(rows)) rows = seq_len(table$rowCount)
  rows = rows[rows>=1L & rows<=table$rowCount]
  for (column in intersect(columns,vapply(table$columns,function(item)item$name,character(1)))) {
    if (!isTRUE(table$getColumn(column)$visible)) next
    for (row in rows) {
      value = table$getCell(column,rowNo=row)$value
      present = length(value) && !is.na(value) && (!is.numeric(value) || is.finite(value))
      if (present || missing) {
        table$addFootnote(col=column,rowNo=row,note=note)
        break
      }
    }
  }
  invisible(NULL)
}

jamovi_table_clear_footnotes = function(table) {
  for (column in table$columns) for (row in seq_len(table$rowCount)) {
    cell = column$getCell(row)
    cell$footnotes = character()
  }
  invisible(NULL)
}

jamovi_bayes_note = function() "Approximate BIC Bayes factor favoring the row model over the other model."

# Presentation-only diagnosis from the fitted design; no alternative test is run.
jamovi_comparison_unavailable = function(model1, model2, test, comparison=NULL) {
  matrices = lapply(list(model1,model2),stats::model.matrix)
  ranks = vapply(matrices,function(matrix)qr(matrix)$rank,integer(1))
  combined = qr(cbind(matrices[[1]],matrices[[2]]))$rank
  reason = if (combined==max(ranks) && ranks[1]==ranks[2]) "equivalent fitted model designs" else
    if (combined>max(ranks)) "nonnested fitted model designs" else
      if (test=="F-test" && !check_nested(model1,model2)) "models fail the existing term-nesting check" else
        if (!is.null(comparison) && (!is.finite(comparison$df) || comparison$df<=0)) "nonpositive or nonfinite likelihood degrees of freedom" else
          if (!is.null(comparison) && !is.finite(comparison$lr)) "a nonfinite likelihood-ratio statistic" else
            if (!is.null(comparison) && comparison$lr < -1e-8) "a negative likelihood-ratio statistic" else
              "the fitted-model comparison did not produce a finite p-value"
  paste0(test," unavailable: ",reason,".")
}
