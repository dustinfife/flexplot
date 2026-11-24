test_that("native options and result protobuf restoration replace stale comparison and coefficient notes", {
  jmvcore:::initProtoBuf()
  for(module in c("glmbasic","glmgeneralized","mixedmodel")) {
    values=list(out=if(module=="glmgeneralized")"y_bin" else "y",preds=c("x","a","z"),modelTerms=list("x","a","z"),
      comparisonTerms=list("x","a"),compare=TRUE,includeP=TRUE,graphic=FALSE,comparisonGraphic=FALSE,comparisonX="x")
    data=small
    if(module=="glmgeneralized")values$family="binomial"
    if(module=="mixedmodel"){values$comparisonX=NULL;values$comparisonFixed=c("x","a");values$cluster="id";data=jamovi_mixed_test_data()}
    pb=jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOptions",asNamespace("jmvcore")))
    template=jmvcore:::RProtoBuf_new(get("jamovi.coms.AnalysisOption",asNamespace("jmvcore")))
    pb$names=names(values);pb$options=lapply(values,jamovi_mixed_saved_option,option_template=template,group_template=pb)
    factory=get(paste0(module,"Options"),asNamespace("flexplot"));class=get(paste0(module,"Class"),asNamespace("flexplot"))
    options=factory$new();options$fromProtoBuf(pb)
    reader=jamovi_reporting_read;environment(reader)=list2env(list(data=data),parent=environment(reader))
    host=function(options){analysis=class$new(options=options,data=NULL);analysis$.setReadDatasetSource(reader);analysis$.setReadDatasetHeaderSource(reader);analysis}
    analysis=host(options);expect_true(analysis$run())
    analysis$results$comparison$getColumn("p")$setTitle("p (old verbose heading)")
    analysis$results$comparison$setNote("stale","Obsolete generic restriction")
    analysis$results$comparison$addFootnote("p","Obsolete generic restriction",rowNo=1)
    cached=analysis$results$asProtoBuf(status=4L,includeState=FALSE)
    restored=factory$new();restored$fromProtoBuf(options$asProtoBuf())
    reopened=host(restored);objects=reopened$results$items
    reopened$results$fromProtoBuf(cached,character(),character())
    expect_identical(reopened$results$items,objects)
    expect_true(reopened$run())
    table=reopened$results$comparison
    expect_identical(table$getColumn("p")$title,"p")
    expect_false(any(grepl("Obsolete",jamovi_test_table_notes(table))))
    expected=table$asDF;notes=jamovi_test_table_notes(table)
    expect_true(reopened$run());expect_identical(table$asDF,expected)
    expect_identical(jamovi_test_table_notes(table),notes)
    expect_true(jamovi_reporting_update(reopened,list(compare=FALSE)))
    expect_equal(table$rowCount,0L);expect_length(jamovi_test_table_notes(table),0L)
    expect_true(jamovi_reporting_update(reopened,list(compare=TRUE)))
    expect_identical(jamovi_test_table_notes(table),notes)
  }
})

test_that("family and valid-invalid-valid transitions publish only current native footnotes", {
  jmvcore:::initProtoBuf()
  analysis=jamovi_reporting_host(flexplot:::glmgeneralizedOptions$new(out="y_bin",preds=c("x","a"),family="binomial",graphic=FALSE),small)
  for(family in c("binomial","poisson","Gamma","gaussian","binomial")) {
    outcome=switch(family,binomial="y_bin",poisson="y_pois",Gamma="y_gam","y")
    expect_true(jamovi_reporting_update(analysis,list(family=family,out=outcome)))
    table=analysis$results$coefficients
    text=paste(jamovi_test_table_notes(table),collapse=" ")
    expect_identical(names(table$notes)[1],"model")
    if(family!="binomial")expect_false(grepl("one-SD change in the fitted predictor contrast|Success event",text))
    if(family!="Gamma")expect_false(grepl("not response ratios",text,fixed=TRUE))
    if(family!="negativebinomial")expect_false(grepl("estimated size (theta)",text,fixed=TRUE))
    expected=table$asProtoBuf()$table
    expect_true(jamovi_reporting_update(analysis,list(out=NULL)))
    empty=table$asProtoBuf()$table
    expect_length(empty$notes,0L)
    for(column in empty$columns)expect_length(column$cells,0L)
    expect_true(jamovi_reporting_update(analysis,list(out=outcome)))
    expect_identical(vapply(table$asProtoBuf()$table$notes,function(note)note$note,character(1)),vapply(expected$notes,function(note)note$note,character(1)))
  }
})
