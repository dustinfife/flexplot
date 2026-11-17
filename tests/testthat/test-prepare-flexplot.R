test_that("public preparation reuses automatic formula selection without requiring a fitted model", {
  for (predictors in list("x",c("x","a"),c("x","a","b","z"),c("a","b"))) {
    prepared = prepare_flexplot(small,outcome="y",predictors=predictors)
    expect_s3_class(prepared,"flexplot_preparation")
    expect_identical(prepared$version,1L)
    expect_identical(as.character(prepared$formula),
      as.character(make_flexplot_formula(predictors,"y",small)))
    expect_identical(prepared$variables$predictors,predictors)
    expect_equal(prepared$grid,unique(bin_variables(generate_predictors(small,prepared$formula,
      make.formula("y",predictors)),3,NULL,prepared$binning$variables,prepared$binning$breaks)))
    actual = flexplot(prepared$formula,prepared$data,bins=prepared$binning$bins,
      breaks=prepared$binning$breaks,labels=prepared$binning$labels,
      return_data=TRUE,suppress_smooth=TRUE)
    expect_equal(prepared$plot_data,actual)
  }
})

test_that("preparation preserves existing reference values, factor metadata and fitted contrasts", {
  d = small
  model = lm(y~x+a+b+z,d,contrasts=list(a=contr.sum(2)))
  prepared = prepare_flexplot(d,formula=y~x,model=model)
  expect_identical(prepared$variables$numeric,c("x","z"))
  expect_identical(prepared$variables$categorical,c("a","b"))
  expect_equal(prepared$reference$z,mean(d$z))
  for (variable in c("a","b")) {
    expected = names(which.max(table(d[[variable]])))
    expect_identical(as.character(prepared$reference[[variable]]),expected)
    expect_identical(prepared$factor_levels[[variable]],model$xlevels[[variable]])
  }
  expect_identical(prepared$contrasts,model$contrasts)
  expect_identical(names(prepared$grid),c("x","a","b","z"))
  expect_true(all(prepared$grid$z==mean(d$z)))
  expect_identical(prepared$model_formula,formula(model))
})

test_that("custom layouts, binning and complete-case eligibility match public flexplot", {
  d = small
  d$y[1] = NA
  d$z[2] = NA
  prepared = prepare_flexplot(d,model=y~x+a+b+z,formula=y~x+b|z,
    breaks=list(z=c(-Inf,0,.7,Inf)),labels=list(z=c("low","middle","high")))
  expect_identical(prepared$rows,which(complete.cases(d[c("y","x","a","b","z")])))
  expect_identical(prepared$roles,list(x="x",group="b",panels="z"))
  expect_identical(prepared$display_roles,list(x="x",group="b",panels="z_binned"))
  expect_equal(prepared$plot_data,flexplot(prepared$formula,prepared$data,
    breaks=prepared$binning$breaks,labels=prepared$binning$labels,
    return_data=TRUE,suppress_smooth=TRUE))
  expect_identical(levels(prepared$plot_data$z_binned),c("low","middle","high"))
  bins = cut(prepared$data$z,c(-Inf,0,.7,Inf),include.lowest=TRUE)
  representatives = round(tapply(prepared$data$z,bins,mean),3)
  expect_equal(sort(unique(prepared$grid$z)),sort(as.numeric(representatives)))
})

test_that("categorical and low-cardinality numeric axes retain display types and ordering", {
  d = small
  d$a = ordered(d$a,levels=rev(levels(d$a)))
  prepared = prepare_flexplot(d,model=y~a+b,formula=y~a+b)
  expect_true(is.ordered(prepared$plot_data$a))
  expect_identical(levels(prepared$plot_data$a),levels(d$a))
  d$x = rep(1:3,length.out=nrow(d))
  prepared = prepare_flexplot(d,model=y~x+b,formula=y~x+b)
  expect_true(is.numeric(prepared$grid$x))
  expect_true(is.ordered(prepared$plot_data$x))
  expect_identical(levels(prepared$plot_data$x),c("1","2","3"))
})

test_that("preparation handles model transformations and literal nonstandard names safely", {
  prepared = prepare_flexplot(small,model=y~x+I(x^2)+a,formula=y~x)
  expect_equal(prepared$grid$x,small$x)
  d = small
  names(d)[names(d)=="x"] = "numeric predictor"
  names(d)[names(d)=="a"] = "group:literal"
  prepared = prepare_flexplot(d,outcome="y",predictors=c("numeric predictor","group:literal","b","z"))
  expect_true(all(all.vars(prepared$formula) %in% names(d)))
  expect_true("group:literal" %in% all.vars(prepared$formula))
  expect_error(prepare_flexplot(small,formula=y~log(x)),"untransformed visualization")
  expect_error(prepare_flexplot(small,model=log(y)~x),"untransformed outcome")
  expect_error(prepare_flexplot(small,model=y~x,formula=y~z),"specified predictor set")
  expect_error(prepare_flexplot(small,outcome="missing"),"outcome column")
})


test_that("preparing reference profiles is deterministic and does not fit observed relationships", {
  set.seed(18)
  rng = .Random.seed
  local_mocked_bindings(flexplot_magnet_fit=function(...) stop("Unexpected fit"),
    flexplot_smooth_layer=function(...) stop("Unexpected smooth"))
  expect_no_error(prepare_flexplot(small,model=y~x+a+b+z))
  expect_no_error(prepare_flexplot(small,model=y_bin~a+b,formula=y_bin~a+b))
  expect_identical(.Random.seed,rng)
  expect_true("prepare_flexplot" %in% getNamespaceExports("flexplot"))
})


test_that("preparation of a fitted binomial model verifies its response coding", {
  model = glm(died~willpower+minutes.fighting,avengers,family=binomial())
  expect_no_error(prepare_flexplot(avengers,model=model))
  altered = avengers
  altered$died = factor(altered$died,levels=rev(levels(altered$died)))
  expect_error(prepare_flexplot(altered,model=model),"outcome coding")
})
