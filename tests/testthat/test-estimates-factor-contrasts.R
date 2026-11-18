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

test_that("one-way differences agree with TukeyHSD for balanced, unbalanced and sparse groups", {
  set.seed(701)
  for (counts in list(c(15,15,15),c(5,13,27),c(1,12,20))) {
    data = data.frame(a=factor(rep(c("C","A","B"),counts),levels=c("C","A","B")))
    data$y = rnorm(nrow(data))+as.numeric(data$a)
    model = lm(y~a,data)
    result = estimates(model,mc=FALSE)$difference.matrix
    reference = TukeyHSD(aov(model))$a
    expect_equal(result$comparison,rownames(reference))
    expect_equal(result$difference,unname(reference[,"diff"]),tolerance=1e-12)
    expect_equal(result$lower,unname(reference[,"lwr"]),tolerance=1e-12)
    expect_equal(result$upper,unname(reference[,"upr"]),tolerance=1e-12)
    expect_identical(unname(attr(result,"adjustments")),"Tukey-Kramer")
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
  expect_identical(unname(attr(result,"adjustments")),"Pointwise t (one comparison)")
  skip_if_not_installed("jmvcore")
  options = flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),graphic=FALSE)
  analysis = flexplot:::glmbasicClass$new(options=options,data=data)
  expect_true(analysis$run())
  reported = analysis$results$diff$asDF
  expect_equal(c(reported$lower,reported$upper),round(unname(confint(model)["aB",]),2))
  expect_match(analysis$results$diff$notes$intervals$note,"Pointwise t")
  saved = flexplot:::glmbasicOptions$new(out="y",preds=c("x","a"),modelTerms=list("x","a"),graphic=FALSE)
  reopened = flexplot:::glmbasicClass$new(options=saved,data=data)
  expect_true(reopened$run())
  expect_equal(reopened$results$diff$asDF,reported)
})

test_that("adjusted pairs use Bonferroni covariance contrasts with interactions and custom coding", {
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
      critical = qt(1-.05/(2*length(reference$estimate)),df.residual(model))
      expect_equal(result_a$comparison,reference$names)
      expect_equal(unname(result_a$difference),unname(reference$estimate),tolerance=1e-10)
      expect_equal(unname(result_a$lower),unname(reference$estimate-critical*reference$se),tolerance=1e-10)
      expect_equal(unname(result_a$upper),unname(reference$estimate+critical*reference$se),tolerance=1e-10)
      expect_identical(attr(result,"adjustments")[["a"]],"Bonferroni")
    }
  }
})

test_that("Bonferroni counts only estimable pairs in the fitted design space", {
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
