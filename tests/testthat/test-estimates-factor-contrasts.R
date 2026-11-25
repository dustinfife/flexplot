contrast_reference = function(model, variable) {
  data = model.frame(model)
  levels = levels(data[[variable]])
  profile = data[rep(1L,length(levels)),,drop=FALSE]
  for (name in setdiff(names(data),all.vars(formula(model))[1])) {
    if (is.numeric(data[[name]])) profile[[name]] = mean(data[[name]]) else {
      mode = names(which.max(table(data[[name]])))
      profile[[name]] = factor(mode,levels=levels(data[[name]]),ordered=is.ordered(data[[name]]))
    }
  }
  profile[[variable]] = factor(levels,levels=levels,ordered=is.ordered(data[[variable]]))
  matrix = model.matrix(delete.response(terms(model)),profile,contrasts.arg=model$contrasts)
  pairs = which(lower.tri(matrix(0,length(levels),length(levels))),arr.ind=TRUE)
  L = matrix[pairs[,1],,drop=FALSE]-matrix[pairs[,2],,drop=FALSE]
  list(estimate=drop(L%*%coef(model)),se=sqrt(diag(L%*%vcov(model)%*%t(L))),
    names=paste(levels[pairs[,1]],levels[pairs[,2]],sep="-"))
}

test_that("one-way differences use unadjusted t intervals for balanced, unbalanced and sparse groups", {
  set.seed(701)
  for (counts in list(c(15,15,15),c(5,13,27),c(1,12,20))) {
    data = data.frame(a=factor(rep(c("C","A","B"),counts),levels=c("C","A","B")))
    data$y = rnorm(nrow(data))+as.numeric(data$a)
    model = lm(y~a,data)
    result = estimates(model,mc=FALSE)$difference.matrix
    reference = contrast_reference(model,"a")
    critical = qt(.975,df.residual(model))
    expect_equal(result$comparison,reference$names)
    expect_equal(result$difference,unname(reference$estimate),tolerance=1e-12)
    expect_equal(result$lower,unname(reference$estimate-critical*reference$se),tolerance=1e-12)
    expect_equal(result$upper,unname(reference$estimate+critical*reference$se),tolerance=1e-12)
    expect_equal((result$upper-result$lower)/(2*critical),unname(reference$se),tolerance=1e-12)
    expect_equal(result$cohens.d,unname(reference$estimate/sigma(model)),tolerance=1e-12)
    expect_identical(unname(attr(result,"adjustments")),"Pointwise t")
  }
})

test_that("confounded ANCOVA uses adjusted covariance and includes zero", {
  set.seed(119)
  data = data.frame(a=factor(rep(c("A","B"),each=30)))
  data$x = ifelse(data$a=="A",0,4)+rnorm(60,sd=.25)
  data$y = 1+2*(data$a=="B")+.5*data$x+rnorm(60,sd=.7)
  model = lm(y~x+a,data)
  result = estimates(model,mc=FALSE)$difference.matrix
  expect_equal(result$difference,unname(coef(model)["aB"]),tolerance=1e-12)
  expect_equal(c(result$lower,result$upper),unname(confint(model)["aB",]),tolerance=1e-12)
  expect_lt(result$lower,0)
  old_radius = qtukey(.95,2,df.residual(model))*sigma(model)*sqrt(sum(1/table(data$a)))
  expect_gt(result$difference-old_radius,0)
  expect_identical(unname(attr(result,"adjustments")),"Pointwise t")
  skip_if_not_installed("jmvcore")
  options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),graphic=FALSE)
  analysis = flexplot:::glmbasicClass$new(options=options,data=data)
  expect_true(analysis$run())
  reported = analysis$results$diff$asDF
  expect_equal(c(reported$lower,reported$upper),round(unname(confint(model)["aB",]),2))
  expect_match(analysis$results$diff$notes$intervals$note,"95% t confidence intervals",fixed=TRUE)
  saved = flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),modelTerms=list("x","a"),graphic=FALSE)
  reopened = flexplot:::glmbasicClass$new(options=saved,data=data)
  expect_true(reopened$run())
  expect_equal(reopened$results$diff$asDF,reported)
})

test_that("conditional pairs use unadjusted covariance t contrasts with interactions and custom coding", {
  set.seed(702)
  data = data.frame(a=factor(rep(c("C","A","B"),c(8,30,51)),levels=c("C","A","B")),
    b=factor(rep(c("low","high"),length.out=89)),x=rnorm(89),z=rnorm(89))
  data$y = .4+.3*data$x+.2*data$z+.8*(data$a=="B")+.5*data$x*(data$a=="A")+rnorm(89)
  data$x[5] = NA
  for (coding in list(contr.sum(3),contr.helmert(3),contr.treatment(3,base=2))) {
    contrasts(data$a) = coding
    for (formula in list(y~a+x+z+b,y~a*x+z+b,y~a*b+x+z)) {
      model = lm(formula,data)
      reference = contrast_reference(model,"a")
      result = estimates(model,mc=FALSE)$difference.matrix
      result_a = result[seq_along(reference$estimate),]
      critical = qt(.975,df.residual(model))
      expect_equal(result_a$comparison,reference$names)
      expect_equal(unname(result_a$difference),unname(reference$estimate),tolerance=1e-10)
      expect_equal(unname(result_a$lower),unname(reference$estimate-critical*reference$se),tolerance=1e-10)
      expect_equal(unname(result_a$upper),unname(reference$estimate+critical*reference$se),tolerance=1e-10)
      expect_identical(attr(result,"adjustments")[["a"]],"Pointwise t")
    }
  }
})

test_that("unadjusted intervals retain nonestimable pairs as missing", {
  set.seed(703)
  data = data.frame(a=factor(rep(c("A","B","C"),each=10)),
    b=factor(rep(c("G","G","H"),each=10)),y=rnorm(30))
  model = lm(y~a+b,data)
  profiles = data.frame(a=factor(c("A","B","C"),levels=levels(data$a)),
    b=factor("G",levels=levels(data$b)))
  result = flexplot:::lm_factor_contrasts(model,"a",profiles,levels(data$a))
  expect_identical(is.na(result$difference),c(FALSE,TRUE,TRUE))
  expect_equal(result$difference[1],unname(coef(model)["aB"]),tolerance=1e-12)
  expect_equal(c(result$lower[1],result$upper[1]),unname(confint(model)["aB",]),tolerance=1e-12)
})

test_that("weighted contrasts retain covariance SEs and residual t degrees of freedom", {
  set.seed(704)
  data = data.frame(a=factor(rep(c("A","B","C"),c(12,19,27))),x=rnorm(58),w=runif(58,.2,3))
  data$y = as.numeric(data$a)+.4*data$x+rnorm(58)
  model = lm(y~a+x,data,weights=w)
  reference = contrast_reference(model,"a")
  result = estimates(model,mc=FALSE)$difference.matrix
  critical = qt(.975,df.residual(model))
  expect_equal(result$difference,unname(reference$estimate),tolerance=1e-12)
  expect_equal(result$lower,unname(reference$estimate-critical*reference$se),tolerance=1e-12)
  expect_equal(result$upper,unname(reference$estimate+critical*reference$se),tolerance=1e-12)
  expect_equal(result$cohens.d,unname(reference$estimate/sigma(model)),tolerance=1e-12)
})

test_that("jamovi reports all three unadjusted pairwise intervals and restores them", {
  skip_if_not_installed("jmvcore")
  set.seed(705)
  data = data.frame(a=factor(rep(c("A","B","C"),c(11,17,23))),x=rnorm(51))
  data$y = as.numeric(data$a)+.4*data$x+rnorm(51)
  reference = contrast_reference(lm(y~a+x,data),"a")
  critical = qt(.975,48)
  options = flexplot:::glmbasicOptions$new(out="y",preds=c("a","x"),graphic=FALSE)
  analysis = flexplot:::glmbasicClass$new(options=options,data=data)
  expect_true(analysis$run())
  reported = analysis$results$diff$asDF
  expect_equal(reported$lower,round(unname(reference$estimate-critical*reference$se),2))
  expect_equal(reported$upper,round(unname(reference$estimate+critical*reference$se),2))
  reopened = flexplot:::glmbasicClass$new(options=flexplot:::glmbasicOptions$new(
    out="y",preds=c("a","x"),modelTerms=list("a","x"),graphic=FALSE),data=data)
  expect_true(reopened$run())
  expect_equal(reopened$results$diff$asDF,reported)
})
