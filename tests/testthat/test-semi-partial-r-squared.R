semi_partial_reference = function(model, reduced, data) {
  eligible = data[match(rownames(model.frame(model)),rownames(data)),,drop=FALSE]
  summary(model)$r.squared-summary(lm(reduced,eligible))$r.squared
}

test_that("unique contributions are independent of predictor order", {
  set.seed(12)
  data = data.frame(x=rnorm(45))
  data$z = data$x+rnorm(45,sd=.2)
  data$y = 1+data$x+2*data$z+rnorm(45)
  full = lm(y~x+z,data)
  result = compute_semi_partial(full)
  expected = c(x=semi_partial_reference(full,y~z,data),z=semi_partial_reference(full,y~x,data))
  expect_equal(result,expected,tolerance=1e-12)
  expect_equal(result[["x"]],.006176745,tolerance=1e-8)
  reversed = compute_semi_partial(lm(y~z+x,data))
  expect_equal(result,reversed[names(result)],tolerance=1e-12)
  # The previous sequential contribution grossly overstated x's unique contribution.
  sequential = anova(full)$`Sum Sq`[1]/sum(anova(full)$`Sum Sq`)
  expect_gt(sequential-result[["x"]],.8)
  before = predict(full)
  compute_semi_partial(full)
  expect_identical(predict(full),before)
})

test_that("orthogonal numeric and complete factor terms use drop-one model spaces", {
  set.seed(804)
  orthogonal = qr.Q(qr(cbind(1,matrix(rnorm(180),ncol=2))))
  data = data.frame(x=orthogonal[,2],z=orthogonal[,3])
  data$y = .7*data$x+1.3*data$z+rnorm(90,sd=.1)
  full = lm(y~x+z,data)
  result = compute_semi_partial(full)
  expect_equal(result,c(x=semi_partial_reference(full,y~z,data),
    z=semi_partial_reference(full,y~x,data)),tolerance=1e-12)
  expect_equal(sum(result),summary(full)$r.squared,tolerance=1e-12)
  data = data.frame(a=factor(rep(c("C","A","B"),c(5,15,40))),x=rnorm(60),y=rnorm(60))
  contrasts(data$a) = contr.sum(3)
  data$y[2] = NA
  data$x[7] = NA
  data$a[12] = NA
  full = lm(y~a+x,data)
  result = compute_semi_partial(full)
  expect_equal(result,c(a=semi_partial_reference(full,y~x,data),
    x=semi_partial_reference(full,y~a,data)),tolerance=1e-12)
  expect_equal(length(result),2L)
  expect_equal(result,compute_semi_partial(lm(y~x+a,data))[names(result)],tolerance=1e-12)
})

test_that("interaction contributions respect marginality without predictor bundles", {
  full = lm(y~x*z+a,small)
  result = compute_semi_partial(full)
  expect_true(all(is.na(result[c("x","z")])))
  expect_equal(result[["x:z"]],semi_partial_reference(full,y~x+z+a,small),tolerance=1e-12)
  expect_equal(result[["a"]],semi_partial_reference(full,y~x*z,small),tolerance=1e-12)
  expect_match(attr(result,"note"),"interactions")
  expect_false(any(grepl("combined",names(result))))
  full = lm(y~a*b+x,small)
  result = compute_semi_partial(full)
  expect_true(all(is.na(result[c("a","b")])))
  expect_equal(result[["a:b"]],semi_partial_reference(full,y~a+b+x,small),tolerance=1e-12)
})

test_that("quadratic spaces distinguish combined predictor and incremental curvature", {
  data = small
  data$y[2] = NA
  data$z[5] = NA
  formulas = list(y~z+x+I(x^2),y~I(x^2)+x+z,y~z+poly(x,2),
    y~poly(x,degree=2L,raw=TRUE)+z,y~z+stats::poly(x,2))
  for (formula in formulas) {
    full = lm(formula,data)
    result = compute_semi_partial(full)
    expect_false("x" %in% names(result))
    expect_equal(result[["z"]],semi_partial_reference(full,y~x+I(x^2),data),tolerance=1e-12)
    expect_equal(result[["x (combined polynomial)"]],semi_partial_reference(full,y~z,data),tolerance=1e-12)
    expect_equal(unname(result[grepl("incremental curvature",names(result))]),
      semi_partial_reference(full,y~x+z,data),tolerance=1e-12)
  }
  full = lm(y~x*z+I(x^2),small)
  result = compute_semi_partial(full)
  expect_true(is.na(result[["x (combined polynomial)"]]))
  expect_equal(result[["I(x^2) (incremental curvature)"]],
    semi_partial_reference(full,y~x*z,small),tolerance=1e-12)
})

test_that("jamovi displays unique contributions and blanks for protected terms", {
  skip_if_not_installed("jmvcore")
  data = small
  options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","z","a"),
    modelTerms=list("x","z","a",c("x","z")),graphic=FALSE)
  analysis = flexplot:::glmbasicClass$new(options=options,data=data)
  expect_true(analysis$run())
  table = analysis$results$rsq$asDF
  full = lm(y~x*z+a,data)
  expect_true(all(is.na(table$Estimate[match(c("x","z"),table$var)])))
  expect_equal(table$Estimate[table$var=="x:z"],semi_partial_reference(full,y~x+z+a,data),tolerance=1e-12)
  expect_match(analysis$results$rsq$notes$hierarchy$note,"interactions")
  # Reopening the same options preserves the row meanings and values.
  reopened = flexplot:::glmbasicClass$new(options=options,data=data)
  expect_true(reopened$run())
  expect_equal(reopened$results$rsq$asDF,table)
})

test_that("jamovi polynomial rows distinguish combined and incremental contributions", {
  skip_if_not_installed("jmvcore")
  full = lm(y~z+x+I(x^2),small)
  analysis = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
    out="y",preds=c("x","z"),graphic=FALSE),data=small)
  analysis$.__enclos_env__$private$.rsq(list(rsq=summary(full)$r.squared,
    semi.p=compute_semi_partial(full),correlation=NA_real_),preds=c("x","z"))
  table = analysis$results$rsq$asDF
  expect_false("x" %in% table$var)
  expect_equal(table$Estimate[table$var=="x (combined polynomial)"],
    semi_partial_reference(full,y~z,small),tolerance=1e-12)
  expect_equal(table$Estimate[table$var=="I(x^2) (incremental curvature)"],
    semi_partial_reference(full,y~x+z,small),tolerance=1e-12)
  expect_match(analysis$results$rsq$notes$polynomial$note,"Curvature is incremental")
})

test_that("higher-degree decompositions are unavailable rather than breaking hierarchy", {
  for (formula in list(y~z+x+I(x^2)+I(x^3),y~z+poly(x,3))) {
    full = lm(formula,small)
    result = compute_semi_partial(full)
    expect_equal(result[["z"]],semi_partial_reference(full,if (length(result)==4L) y~x+I(x^2)+I(x^3) else y~poly(x,3),small),tolerance=1e-12)
    expect_true(all(is.na(result[names(result)!="z"])))
    expect_match(attr(result,"note"),"higher-degree")
  }
})
