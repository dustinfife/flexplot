lm_automatic_fixture = function() {
  withr::with_seed(746, {
    data = as.data.frame(matrix(rnorm(1200),200,6))
    names(data) = paste0("x",1:6)
    data$a = factor(rep(c("A","B","C"),length.out=nrow(data)))
    data$y = 2+.3*data$x1-.2*data$x2+.5*data$x5+rnorm(nrow(data),sd=.6)
    data
  })
}

test_that("five-predictor jamovi automatic model plots use the fitted formula", {
  skip_if_not_installed("jmvcore")
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  analysis = lm_quadratic_analysis(data=lm_automatic_fixture(),preds=paste0("x",1:5),graphic=TRUE)
  expect_true(analysis$run())
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
})

test_that("automatic and explicit jamovi LM plots preserve model inference and added-variable semantics", {
  skip_if_not_installed("jmvcore")
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  data = lm_automatic_fixture()
  data$x2[c(3,17,38)] = NA_real_
  data$y[62] = NA_real_
  literal = c("has space","a+b","tick`name","back\\slash","log(y)")
  for (i in seq_along(literal)) data[[literal[i]]] = data[[paste0("x",i)]]
  capture = new.env(parent=emptyenv())
  original = flexplot:::jamovi_glm_model_plot
  local_mocked_bindings(jamovi_glm_model_plot=function(model,...) {
    capture$model = model;capture$plot = original(model,...);capture$plot
  },.package="flexplot")
  cases = lapply(1:6,function(n)list(preds=paste0("x",seq_len(n))))
  cases = c(cases,list(
    list(preds=c("x1","x2","x3","x4","a")),
    list(preds=paste0("x",1:5),modelTerms=c(as.list(paste0("x",1:5)),list(c("x1","x2")))),
    list(preds=paste0("x",1:5),polynomials=TRUE,quadratic="x1"),
    list(preds=literal)))
  for (case in cases) {
    analysis = do.call(lm_quadratic_analysis,c(list(data=data,graphic=TRUE),case))
    expect_true(analysis$run())
    formula = analysis$results$plot$state$formula
    independent = stats::lm(formula,data=data)
    for (explicit in c(FALSE,TRUE)) {
      if (explicit) expect_true(jamovi_reporting_update(analysis,list(plotX=case$preds[1])))
      expect_no_warning(expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot)))
      model = capture$model;plot = capture$plot
      jamovi_expect_lm_coefficients(analysis,independent)
      expect_equal(stats::coef(model),stats::coef(independent),tolerance=1e-12)
      expect_equal(stats::residuals(model),stats::residuals(independent),tolerance=1e-12)
      expect_equal(stats::predict(model,interval="confidence"),stats::predict(independent,interval="confidence"),tolerance=1e-12)
      expect_equal(stats::confint(model),stats::confint(independent),tolerance=1e-12)
      expect_identical(stats::nobs(model),stats::nobs(independent))
      expect_identical(all.vars(stats::formula(model)),all.vars(formula))
      expect_no_warning(ggplot2::ggplotGrob(plot))
      if (!explicit && length(case$preds)>4L) {
        # Existing AVPs residualize against the other raw predictors and add the
        # observed outcome mean. Formula retrieval must not change this estimand.
        selected = utils::tail(all.vars(formula),1)
        others = setdiff(all.vars(formula)[-1],selected)
        nuisance = stats::lm(make.formula(as.name("y"),lapply(others,as.name)),data=stats::model.frame(model))
        expected = unname(stats::residuals(nuisance))+mean(stats::model.response(stats::model.frame(model)))
        expect_equal(plot$data$residuals,expected,tolerance=1e-12)
        expect_identical(all.vars(plot$formula),c("residuals",selected))
      }
      if (explicit) {
        curves = Filter(function(layer)is.data.frame(layer$data) && "prediction" %in% names(layer$data) &&
          inherits(layer$geom,"GeomLine"),plot$layers)
        expect_length(curves,1L)
        curve = curves[[1]]$data
        frame = stats::model.frame(independent)
        expected = vapply(curve[[case$preds[1]]],function(x) {
          profile = data[rownames(frame),,drop=FALSE];profile[[case$preds[1]]] = x
          fit = mean(stats::predict(independent,newdata=profile))
          design = stats::model.matrix(stats::delete.response(stats::terms(independent)),profile,
            contrasts.arg=independent$contrasts,xlev=independent$xlevels)
          contrast = colMeans(design)
          se = sqrt(drop(contrast %*% stats::vcov(independent) %*% contrast))
          radius = stats::qt(.975,stats::df.residual(independent))*se
          c(fit=fit,lower=fit-radius,upper=fit+radius)
        },numeric(3))
        expect_equal(curve$prediction,unname(expected["fit",]),tolerance=1e-10)
        expect_equal(curve$prediction.lwr,unname(expected["lower",]),tolerance=1e-10)
        expect_equal(curve$prediction.upr,unname(expected["upper",]),tolerance=1e-10)
        bands = Filter(function(layer)inherits(layer$geom,"GeomRibbon"),plot$layers)
        expect_length(bands,1L)
        expect_equal(bands[[1]]$data$prediction.lwr,curve$prediction.lwr)
        expect_equal(bands[[1]]$data$prediction.upr,curve$prediction.upr)
      }
    }
  }
})

test_that("core LM formula access preserves its environment and literal names", {
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  data = lm_automatic_fixture()
  literal = c("has space","a+b","tick`name","back\\slash","log(y)","stop('LM_NAME_EXECUTED')")
  names(data)[1:6] = literal
  formula = make.formula(as.name("y"),lapply(literal,as.name))
  model = stats::lm(formula,data=data)
  before = model
  capture = new.env(parent=emptyenv());original = flexplot:::added.plot
  local_mocked_bindings(added.plot=function(formula,...) {
    capture$formula = formula;original(formula,...)
  },.package="flexplot")
  plot = visualize(model,plot="model")
  expect_identical(model$call[[2]],as.name("formula"))
  expect_identical(capture$formula,stats::formula(model))
  expect_identical(environment(capture$formula),environment(formula))
  expect_identical(all.vars(capture$formula),c("y",literal))
  expect_identical(model,before)
  reference = added.plot(stats::formula(model),data=stats::model.frame(model))+ggplot2::labs(title="Analysis Plot")
  expect_equal(plot$data,reference$data)
  expect_equal(ggplot2::ggplot_build(plot)$data,ggplot2::ggplot_build(reference)$data)
  expect_no_warning(ggplot2::ggplotGrob(plot))
})

test_that("automatic LM plotting leaves comparison estimates and predictions unchanged", {
  skip_if_not_installed("jmvcore")
  previous_theme = ggplot2::theme_get()
  withr::defer(ggplot2::theme_set(previous_theme))
  withr::local_pdf(file=tempfile(fileext=".pdf"))
  data = lm_automatic_fixture();data$x6[1:20] = NA_real_
  analysis = lm_quadratic_analysis(data=data,preds=paste0("x",1:6),graphic=TRUE,compare=TRUE,
    modelTerms=as.list(paste0("x",1:6)),comparisonTerms=as.list(paste0("x",1:5)),comparisonX="x1")
  expect_true(analysis$run())
  state = analysis$results$comparisonplot$state
  rows = analysis$results$comparison$asDF
  full = stats::lm(y~x1+x2+x3+x4+x5+x6,data=state$data)
  reduced = stats::lm(y~x1+x2+x3+x4+x5,data=state$data)
  expect_equal(stats::coef(state$model1),stats::coef(full),tolerance=1e-12)
  expect_equal(stats::coef(state$model2),stats::coef(reduced),tolerance=1e-12)
  # model.comparison() retains its established three-decimal table precision.
  expect_equal(rows$aic,round(c(stats::AIC(full),stats::AIC(reduced)),3),tolerance=1e-12)
  expect_equal(rows$bic,round(c(stats::BIC(full),stats::BIC(reduced)),3),tolerance=1e-12)
  expect_equal(state$p,stats::anova(reduced,full)$`Pr(>F)`[2],tolerance=1e-12)
  expect_true(analysis$.__enclos_env__$private$.plot(analysis$results$plot))
  expect_true(analysis$.__enclos_env__$private$.comparisonplot(analysis$results$comparisonplot))
  expect_identical(analysis$results$comparison$asDF,rows)
  plot = flexplot:::jamovi_glm_comparison_plot(state)
  for (layer in plot$layers) {
    if (!is.data.frame(layer$data) || !all(c("prediction","model") %in% names(layer$data))) next
    for (i in 1:2) {
      curve = layer$data[startsWith(as.character(layer$data$model),paste0("model",i," (")),,drop=FALSE]
      if (!nrow(curve)) next
      model = list(full,reduced)[[i]]
      expected = vapply(curve$x1,function(x) {
        profile = state$data;profile$x1 = x;mean(stats::predict(model,newdata=profile))
      },numeric(1))
      expect_equal(curve$prediction,expected,tolerance=1e-10)
    }
  }
})
