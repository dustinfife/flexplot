# Read actual native cell notes rather than relying on the text-export cache.
jamovi_test_table_notes = function(table) {
  cells = unlist(lapply(table$columns,function(column) {
    if (!isTRUE(column$visible)) return(character())
    unlist(lapply(seq_len(table$rowCount),function(row)column$getCell(row)$footnotes),use.names=FALSE)
  }),use.names=FALSE)
  unique(c(vapply(table$notes,function(note)note$note,character(1)),cells))
}
