# Independent data/reference layer: never calls flexplot fitting/prediction helpers.
e2e_fixtures = function() {
  had_seed = exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
  if (had_seed) previous_seed = get(".Random.seed", envir=.GlobalEnv)
  previous_kind = RNGkind()
  on.exit({
    do.call(RNGkind, as.list(previous_kind))
    if (had_seed) assign(".Random.seed", previous_seed, envir=.GlobalEnv) else
      if (exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)) rm(".Random.seed", envir=.GlobalEnv)
  }, add=TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(20261120)
  sizes = rep(12:27, length.out=24)
  n = sum(sizes)
  d = data.frame(row_id=seq_len(n), id=factor(rep(sprintf("C%02d", 1:24), sizes)))
  d$x = rnorm(n)
  d$z = rnorm(n)
  d$correlated = .85*d$x+sqrt(1-.85^2)*rnorm(n)
  d$binary = factor(sample(rep(c("No", "Yes"), length.out=n)), levels=c("No", "Yes"))
  d$group = factor(sample(rep(c("middle", "low", "high"), length.out=n)), levels=c("middle", "low", "high"))
  d$ordered = ordered(d$group, levels=c("middle", "low", "high"))
  b = as.numeric(d$binary == "Yes")
  g = c(middle=0, low=-.6, high=.9)[as.character(d$group)]
  eta = .4+.8*d$x-.45*d$z+.7*b+g
  d$y = eta+rnorm(n, sd=.6)
  d$quadratic_y = eta+.85*d$x^2+rnorm(n, sd=.6)
  d$interaction_y = eta+1.1*d$x*d$z+1.3*d$x*b+rnorm(n, sd=.6)
  probability = plogis(-.35+.65*d$x-.3*d$z+.45*b+.5*d$x*b)
  d$binary_y = rbinom(n, 1, probability)
  d$factor_y = factor(d$binary_y, levels=0:1, labels=c("No", "Yes"))
  d$ordered_y = ordered(d$factor_y, levels=c("No", "Yes"))
  d$count_y = rpois(n, exp(.4+.3*d$x-.15*d$z+.2*b))
  d$nb_y = rnbinom(n, mu=exp(.5+.35*d$x-.2*d$z+.3*b), size=1.5)
  d$gamma_y = rgamma(n, shape=6, scale=exp(.8+.2*d$x-.15*d$z+.2*b)/6)
  d$gamma_inverse_y = rgamma(n, shape=10, scale=(1/(2+.12*d$x+.1*d$z))/10)
  intercept = rnorm(24, sd=.9)
  slope = .2*intercept+rnorm(24, sd=.45)
  d$random_intercept_y = eta+intercept[d$id]+rnorm(n, sd=.45)
  d$mixed_y = eta+intercept[d$id]+slope[d$id]*d$x+rnorm(n, sd=.45)
  u_category = runif(n)
  d$category_y = factor(ifelse(u_category < .3, "Disagree",
    ifelse(u_category < .3+.7*plogis(.3+.8*b+g), "Agree", "Neutral")),
    levels=c("Disagree", "Neutral", "Agree"))
  d$x_missing = d$x; d$x_missing[seq(7, n, 17)] = NA_real_
  d$z_missing = d$z; d$z_missing[seq(3, n, 13)] = NA_real_
  d$y_missing = d$y; d$y_missing[seq(11, n, 19)] = NA_real_
  d$binary_missing = d$binary_y; d$binary_missing[seq(5, n, 23)] = NA_integer_
  d$constant = 1
  d$x_duplicate = 2*d$x
  d[["has space"]] = d$x
  d[["a+b"]] = d$z
  d[["tick`name"]] = d$x
  d[["back\\slash"]] = d$z
  d[["log(y)"]] = d$x
  d[["1"]] = d$z
  d[["stop('must not execute')"]] = d$x
  # Reuse the exact existing common-sample regression DGP (not its fit helper).
  set.seed(925)
  common = data.frame(id=factor(rep(1:16, each=15)), x=rep(seq(-1.5,1.5,length.out=15),16), z=rnorm(240),
    a=ordered(rep(c("low","middle","high"),80),levels=c("middle","low","high")))
  u = rnorm(16,sd=.8); v = .25*u+rnorm(16,sd=.45)
  common$y = .3+.6*common$x+.2*common$z+.4*(common$a=="high")+.3*common$x*(common$a=="high")+
    .15*common$x^2+u[common$id]+v[common$id]*common$x+rnorm(240,sd=.4)
  common$z[1:60] = NA_real_
  separation = data.frame(x=rep(c(-1,1),20), y=rep(0:1,20))
  rank = d[1:60,c("y","x","x_duplicate","constant")]
  sparse = data.frame(x=factor(c("A","A","B","C"),levels=c("C","A","B","unused")),
    group=factor(c("g1","g2","g1","g2")), y=c(1,2,4,7), binary_y=c(0,1,0,1))
  singular = expand.grid(x=c(-1,1), replicate=1:8, id=factor(1:8))
  singular$y = .5+.8*singular$x+rep(c(-.2,.2),each=2,length.out=nrow(singular))
  invalid = data.frame(x=seq(-1,1,length.out=30), nonbinary=rep(0:2,10), negative_count=rep(c(-1,0,2),10),
    noninteger_count=rep(c(.2,1,2),10), nonpositive_gamma=rep(c(0,1,2),10), constant_y=1, infinite_y=c(Inf,1:29))
  paired = data.frame(condition=factor(rep(c("Before","After"),each=30),levels=c("Before","After")),
    y=c(seq_len(30)/10,seq_len(30)/10+.5))
  list(primary=d, common_sample=common, separation=separation, rank_deficient=rank,
    sparse=sparse, singular=singular, invalid=invalid, paired=paired)
}

e2e_reference_fit = function(specification, data) {
  variables = unique(unlist(lapply(specification$formulas, function(text) all.vars(stats::as.formula(text)))))
  eligible = data[stats::complete.cases(data[,variables,drop=FALSE]),,drop=FALSE]
  fits = lapply(seq_along(specification$formulas), function(i) {
    formula = stats::as.formula(specification$formulas[[i]])
    estimation = if (is.null(specification$estimations)) specification$estimation else specification$estimations[[i]]
    switch(specification$engine,
      lm=stats::lm(formula, eligible, na.action=stats::na.fail),
      lmer=lme4::lmer(formula, eligible, REML=identical(estimation,"REML"), na.action=stats::na.fail),
      glm=stats::glm(formula, eligible, family=switch(specification$family,
        gaussian=stats::gaussian(), binomial=stats::binomial(), poisson=stats::poisson(),
        gammaLog=stats::Gamma("log"), Gamma=stats::Gamma("inverse")), na.action=stats::na.fail),
      negativebinomial=MASS::glm.nb(formula, eligible, na.action=stats::na.fail))
  })
  results = lapply(fits, function(model) {
    mixed = inherits(model,"merMod")
    beta = if (mixed) lme4::fixef(model) else stats::coef(model)
    covariance = as.matrix(stats::vcov(model))
    se = sqrt(diag(covariance))
    critical = if (inherits(model,"lm") && !inherits(model,"glm")) stats::qt(.975,stats::df.residual(model)) else
      if (inherits(model,"glm") && model$family$family %in% c("gaussian","Gamma")) stats::qt(.975,stats::df.residual(model)) else stats::qnorm(.975)
    predict_response = function(profile, re=FALSE) {
      if (mixed) return(as.numeric(stats::predict(model,profile,re.form=if(re) NULL else NA)))
      as.numeric(stats::predict(model,profile,type="response"))
    }
    # Explicit full-reference standardization at diagnostic x coordinates.
    marginal = if ("x" %in% names(eligible) && !anyNA(beta)) vapply(c(-1,0,1),function(value) {
      profile = eligible; profile$x = value; mean(predict_response(profile))
    },numeric(1)) else numeric()
    inference = summary(model)$coefficients
    index = match(names(beta),rownames(inference))
    value = list(n=nrow(eligible), rows=rownames(eligible), coefficient_names=names(beta), coefficients=beta, se=se, vcov=covariance,
      statistic=unname(inference[index,3]), p=if(ncol(inference)>=4L) unname(inference[index,4]) else rep(NA_real_,length(beta)),
      interval_method=if(mixed) "normal-Wald" else if(critical==stats::qnorm(.975)) "normal" else "t",
      lower=beta-critical*se, upper=beta+critical*se, residual_df=stats::df.residual(model),
      logLik=as.numeric(stats::logLik(model)), likelihood_df=attr(stats::logLik(model),"df"),
      AIC=stats::AIC(model), BIC=stats::BIC(model), observed_predictions=predict_response(eligible),
      diagnostic_x=if(length(marginal)) c(-1,0,1) else numeric(), marginal_predictions=marginal,
      prediction_limitation=if(anyNA(beta)) "Aliased coefficients: diagnostic marginal profiles are not assumed estimable." else NULL)
    if (inherits(model,"lm") && !inherits(model,"glm")) value$r_squared = summary(model)$r.squared
    if (inherits(model,"negbin")) value$theta = model$theta
    if (mixed) {
      value$variance_components = as.data.frame(lme4::VarCorr(model))
      value$REML = lme4::isREML(model)
      value$singular = lme4::isSingular(model)
      value$groups = nlevels(lme4::getME(model,"flist")[[1]])
      value$conditional_predictions = predict_response(eligible,TRUE)
      # Independent CAS oracle: remove every fitted Zb contribution, retaining fixed part + residual.
      value$adjusted_outcomes = as.numeric(stats::model.response(stats::model.frame(model))-
        lme4::getME(model,"Z") %*% lme4::getME(model,"b"))
    }
    value
  })
  comparison = NULL
  if (length(fits)==2L) {
    ml = lapply(fits,function(model) if(inherits(model,"merMod") && lme4::isREML(model)) lme4::refitML(model) else model)
    comparison = list(AIC=vapply(ml,stats::AIC,numeric(1)),BIC=vapply(ml,stats::BIC,numeric(1)),
      logLik=vapply(ml,function(model)as.numeric(stats::logLik(model)),numeric(1)),
      likelihood_df=vapply(ml,function(model)as.numeric(attr(stats::logLik(model),"df")),numeric(1)))
    comparison$BIC_BF_1_over_2 = exp((comparison$BIC[2]-comparison$BIC[1])/2)
    # Reporting eligibility is specified by each scenario; never infer validity from a finite statistic.
  }
  list(eligible_rows=rownames(eligible),omitted=nrow(data)-nrow(eligible),models=results,comparison=comparison)
}
