cas_model_line_index = function(p) {
  which(vapply(p$layers, function(layer) inherits(layer$geom, "GeomLine") &&
                 identical(layer$aes_params$colour, "#bf0303"), logical(1)))
}

cas_model_ghost_index = function(p) {
  which(vapply(p$layers, function(layer) identical(layer$aes_params$colour, "gray"), logical(1)))
}

expect_cas_reference_ghost = function(p, reference) {
  built = ggplot2::ggplot_build(p)
  main = built$data[[cas_model_line_index(p)]]
  ghost = built$data[[cas_model_ghost_index(p)]]
  layout = built$layout$layout
  is_reference = rep(TRUE, nrow(layout))
  for (variable in names(reference)) {
    is_reference = is_reference & as.character(layout[[variable]]) == reference[[variable]]
  }
  expect_equal(sum(is_reference), 1)
  source = main[main$PANEL == layout$PANEL[is_reference], ]
  expect_setequal(unique(main$PANEL), layout$PANEL)
  expect_setequal(unique(ghost$PANEL), layout$PANEL[!is_reference])
  for (panel in unique(ghost$PANEL)) {
    actual = ghost[ghost$PANEL == panel, ]
    expect_equal(actual$x, source$x)
    expect_equal(actual$y, source$y, tolerance=1e-10)
    expect_equal(length(unique(actual$group)), 1)
  }
  invisible(built)
}

test_that("CAS continuous moderator curves use full-data bin representatives", {
  m = lme4::lmer(y ~ x*z + x:I(z^2) + (1 | id), small_mixed)
  p = cluster_adjusted_scatter(y ~ x | z, m, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  curve = built$data[[cas_model_line_index(p)]]
  beta = lme4::fixef(m)
  slopes = numeric()
  for (panel in built$layout$layout$PANEL) {
    bin = built$layout$layout$z_binned[built$layout$layout$PANEL == panel]
    z = mean(p$data$z[p$data$z_binned == bin])
    actual = curve[curve$PANEL == panel, ]
    expected = beta["(Intercept)"] + beta["x"]*actual$x + beta["z"]*z +
      beta["x:z"]*actual$x*z + beta["x:I(z^2)"]*actual$x*z^2
    expect_equal(actual$y, unname(expected), tolerance=1e-10)
    slopes = c(slopes, beta["x"] + beta["x:z"]*z + beta["x:I(z^2)"]*z^2)
    # A representative-value prediction differs from averaging nonlinear
    # predictions over the entire within-panel z distribution.
    average_square = mean(p$data$z[p$data$z_binned == bin]^2)
    averaged = expected + beta["x:I(z^2)"]*actual$x*(average_square-z^2)
    expect_gt(max(abs(actual$y-averaged)), 1e-5)
  }
  expect_gt(diff(range(slopes)), 1e-3)
  sampled = cluster_adjusted_scatter(y ~ x | z, m, sample=2, jitter=FALSE)
  expect_equal(sampled$layers[[cas_model_line_index(sampled)]]$data,
               p$layers[[cas_model_line_index(p)]]$data)
  reference = as.character(p$data$z_binned[which.min(abs(p$data$z))])
  ghost = cluster_adjusted_scatter(y ~ x | z, m, ghost.line="gray", ghost.reference=list(z=0), jitter=FALSE)
  expect_cas_reference_ghost(ghost, list(z_binned=reference))
})

test_that("CAS keeps one observation-weighted nuisance distribution across panels", {
  d = small_mixed
  d$w = d$x^2 + seq_len(nrow(d))/nrow(d)
  m = lme4::lmer(y ~ x*z + w + x:w + z:w + I(w^2) + (1 | id), d)
  p = cluster_adjusted_scatter(y ~ x | z, m, jitter=FALSE)
  built = ggplot2::ggplot_build(p)
  line = built$data[[cas_model_line_index(p)]]
  beta = lme4::fixef(m)
  for (panel in built$layout$layout$PANEL) {
    bin = built$layout$layout$z_binned[built$layout$layout$PANEL == panel]
    z = mean(p$data$z[p$data$z_binned == bin])
    actual = line[line$PANEL == panel, ]
    expected = beta["(Intercept)"] + beta["x"]*actual$x + beta["z"]*z +
      beta["x:z"]*actual$x*z + beta["w"]*mean(d$w) +
      beta["x:w"]*actual$x*mean(d$w) + beta["z:w"]*z*mean(d$w) +
      beta["I(w^2)"]*mean(d$w^2)
    expect_equal(actual$y, unname(expected), tolerance=1e-10)
  }
})

test_that("CAS categorical moderator ghosts copy conditional model curves", {
  d = small_mixed
  d$a = factor(d$a, levels=c("yes", "no"), ordered=TRUE)
  m = lme4::lmer(y ~ x*a + z + a:z + (1 | id), d)
  local_mocked_bindings(flexplot_ghost_smooth_data=function(...) stop("Descriptive ghost attempted"),
                         .package="flexplot")
  p = cluster_adjusted_scatter(y ~ x | a, m, ghost.line="gray", ghost.reference=list(a="yes"), jitter=FALSE)
  built = expect_cas_reference_ghost(p, list(a="yes"))
  expect_identical(as.character(built$layout$layout$a), c("yes", "no"))
  expect_false(any(vapply(p$layers, function(layer) inherits(layer$stat,"StatSmooth"), logical(1))))
  main = built$data[[cas_model_line_index(p)]]
  slopes = numeric()
  for (panel in built$layout$layout$PANEL) {
    category = built$layout$layout$a[built$layout$layout$PANEL == panel]
    actual = main[main$PANEL == panel, ]
    expected = numeric(nrow(actual))
    for (i in seq_len(nrow(actual))) {
      profile = model.frame(m)
      profile$x[] = actual$x[i]
      profile$a[] = category
      expected[i] = mean(predict(m, newdata=profile, re.form=NA))
    }
    expect_equal(actual$y, expected, tolerance=1e-10)
    slopes = c(slopes, diff(range(actual$y))/diff(range(actual$x)))
  }
  expect_gt(diff(range(slopes)), 1e-3)
  descriptive = cluster_adjusted_scatter(y ~ x | a, m, ghost.line="gray",
                                         ghost.reference=list(a="yes"), smooth=TRUE, jitter=FALSE)
  expect_cas_reference_ghost(descriptive, list(a="yes"))
  expect_true(inherits(tail(descriptive$layers,1)[[1]]$stat,"StatSmooth"))
})

test_that("CAS numeric ghosts share custom bins and preserve fitted coordinates", {
  m = lme4::lmer(y ~ x*z + (x | id), small_mixed)
  p = cluster_adjusted_scatter(y ~ x | z, m, ghost.line="gray", ghost.reference=list(z=0),
          breaks=list(z=c(-.5,.5)), labels=list(z=c("low","middle","high")), jitter=FALSE)
  expect_cas_reference_ghost(p,list(z_binned="middle"))
  curve = p$layers[[cas_model_line_index(p)]]$data
  for (label in levels(p$data$z_binned)) {
    representative = mean(p$data$z[p$data$z_binned == label])
    expect_equal(unique(curve$z[curve$z_binned == label]), representative)
  }
  sampled = cluster_adjusted_scatter(y ~ x | z, m, ghost.line="gray", ghost.reference=list(z=0),
          breaks=list(z=c(-.5,.5)), labels=list(z=c("low","middle","high")), sample=3, jitter=FALSE)
  expect_equal(sampled$layers[[cas_model_ghost_index(sampled)]]$data,
               p$layers[[cas_model_ghost_index(p)]]$data)
  linear_method = cluster_adjusted_scatter(y ~ x | z, m, ghost.line="gray", ghost.reference=list(z=0),
          breaks=list(z=c(-.5,.5)), labels=list(z=c("low","middle","high")), method="lm", jitter=FALSE)
  expect_equal(linear_method$layers[[cas_model_ghost_index(linear_method)]]$data,
               p$layers[[cas_model_ghost_index(p)]]$data)
  expect_no_warning(ggplot2::ggplotGrob(p))
})

test_that("CAS handles two displayed moderators and rejects unsupported prediction ghosts", {
  m = lme4::lmer(y ~ x*z*a + (1 | id), small_mixed)
  p = cluster_adjusted_scatter(y ~ x | z + a, m, ghost.line="gray",
                ghost.reference=list(z=0,a="yes"), jitter=FALSE)
  bins = p$data$z_binned[which.min(abs(p$data$z))]
  built = expect_cas_reference_ghost(p, list(z_binned=as.character(bins),a="yes"))
  curve = p$layers[[cas_model_line_index(p)]]$data
  expected = predict(m, newdata=curve, re.form=NA)
  expect_equal(curve$prediction, unname(expected), tolerance=1e-10)
  expect_equal(length(unique(built$data[[cas_model_line_index(p)]]$PANEL)), 6)
  expect_error(cluster_adjusted_scatter(y ~ x + a | z, m, ghost.line="gray"), "second-axis grouping")
  expect_error(cluster_adjusted_scatter(y ~ x, m, ghost.line="gray"), "conditioning variable")
  grouped = cluster_adjusted_scatter(y ~ x + a | z, m, jitter=FALSE)
  group_curve = tail(grouped$layers,1)[[1]]$data
  expect_equal(group_curve$prediction, unname(predict(m,newdata=group_curve,re.form=NA)))
  expect_identical(levels(group_curve$a),levels(small_mixed$a))
})

test_that("CAS categorical x and row facets use the same prediction ghost coordinates", {
  m = lme4::lmer(y ~ a*b + (1 | id), small_mixed)
  p = cluster_adjusted_scatter(y ~ a | b, m, ghost.line="gray", ghost.reference=list(b="mild"), jitter=FALSE)
  expect_cas_reference_ghost(p,list(b="mild"))
  row = cluster_adjusted_scatter(y ~ a | + b, m, ghost.line="gray", ghost.reference=list(b="mild"), jitter=FALSE)
  expect_cas_reference_ghost(row,list(b="mild"))
})

test_that("CAS propagates model prediction failures without evaluating moderator names", {
  d = small_mixed
  moderator = "z\\` + I({cas_moderator_probe(); y}) #"
  names(d)[names(d) == "z"] = moderator
  marker = new.env(parent=emptyenv())
  marker$called = FALSE
  cas_moderator_probe = function() {
    marker$called = TRUE
    stop("Moderator name executed")
  }
  f = make.formula("y", call("*", as.name("x"), as.name(moderator)), "(1 | id)")
  m = lme4::lmer(f, d)
  # The fitted model's own prediction method fails on this escaped name even
  # when supplied its original frame. CAS must report it, never refit a model.
  original = tryCatch(predict(m,newdata=model.frame(m),re.form=NA),error=identity)
  result = tryCatch(cluster_adjusted_scatter(jamovi_formula("y","x",moderator),m,
                ghost.line="gray",ghost.reference=setNames(list(0),moderator),jitter=FALSE),error=identity)
  if (inherits(original,"error")) {
    expect_s3_class(result,"error")
    expect_match(conditionMessage(result),"CAS fixed-effects prediction failed")
    expect_match(conditionMessage(result),conditionMessage(original),fixed=TRUE)
  } else {
    curve = result$layers[[cas_model_line_index(result)]]$data
    expect_equal(curve$prediction,unname(predict(m,newdata=curve,re.form=NA)))
  }
  expect_false(marker$called)
})

test_that("Gaussian binary CAS observations do not trigger internal magnet fitting", {
  d = small_mixed
  d$y = as.numeric(d$y > 0)
  m = suppressMessages(lme4::lmer(y ~ a + (1 | id),d))
  local_mocked_bindings(flexplot_magnet_object=function(...) stop("Unexpected magnet refit"),.package="flexplot")
  p = cluster_adjusted_scatter(y ~ a,m,adjust_re="none",jitter=FALSE)
  expect_equal(p$data$y_adj,d$y)
  expect_equal(length(p$layers),3)
  curve = p$layers[[cas_model_line_index(p)]]$data
  expect_equal(curve$prediction,as.numeric(predict(m,re.form=NA))[match(as.character(curve$a),as.character(d$a))])
})

test_that("CAS prediction ghosts preserve ordinary non-syntactic moderator names", {
  for (name in c("moderator with space","z\\name")) {
    d = small_mixed
    names(d)[names(d) == "z"] = name
    m = lme4::lmer(make.formula("y",call("*",as.name("x"),as.name(name)),"(1 | id)"),d)
    p = cluster_adjusted_scatter(jamovi_formula("y","x",name),m,
                ghost.line="gray",ghost.reference=setNames(list(0),name),jitter=FALSE)
    column = paste0(name,"_binned")
    bin = as.character(p$data[[column]][which.min(abs(p$data[[name]]))])
    expect_cas_reference_ghost(p,setNames(list(bin),column))
    curve = p$layers[[cas_model_line_index(p)]]$data
    expect_equal(curve$prediction,unname(predict(m,newdata=curve,re.form=NA)))
  }
})

test_that("CAS rejects prediction profiles outside a nonlinear model's domain", {
  d = subset(small_mixed, x > z + .2)
  m = suppressMessages(lme4::lmer(y ~ x + z + log(x-z) + (1 | id),d))
  profile = model.frame(m)
  profile$x[] = min(d$x)
  profile$z[] = max(d$z)
  expect_error(cas_predict_fixed(m,profile),"finite fixed-effects prediction")
})

test_that("CAS nuisance columns cannot overwrite prediction metadata", {
  d = small_mixed
  d$prediction = d$a
  d$model = d$b
  m = lme4::lmer(y ~ x*z + prediction + model + (1 | id),d)
  p = cluster_adjusted_scatter(y ~ x | z,m,ghost.line="gray",ghost.reference=list(z=0),jitter=FALSE)
  curve = p$layers[[cas_model_line_index(p)]]$data
  expect_true(is.numeric(curve$prediction))
  expect_false(anyNA(curve$prediction))
  expect_identical(levels(curve$model),"fixed effects")
  expected = numeric(nrow(curve))
  for (i in seq_len(nrow(curve))) {
    profile = model.frame(m)
    profile$x[] = curve$x[i]
    profile$z[] = curve$z[i]
    expected[i] = mean(predict(m,newdata=profile,re.form=NA))
  }
  expect_equal(curve$prediction,expected,tolerance=1e-10)
  bin = as.character(p$data$z_binned[which.min(abs(p$data$z))])
  expect_cas_reference_ghost(p,list(z_binned=bin))
})

test_that("CAS can copy a model reference curve from an empty observed facet", {
  d = small_mixed
  d$g = factor(ifelse(d$z > median(d$z),"high","low"),levels=c("low","high"))
  d = d[!(d$a == "yes" & d$g == "high"), ]
  m = lme4::lmer(y ~ x*a + g + (1 | id),d)
  p = cluster_adjusted_scatter(y ~ x | a + g,m,ghost.line="gray",
                               ghost.reference=list(a="yes",g="high"),jitter=FALSE)
  built = expect_cas_reference_ghost(p,list(a="yes",g="high"))
  layout = built$layout$layout
  reference_panel = layout$PANEL[layout$a == "yes" & layout$g == "high"]
  expect_false(any(built$data[[1]]$PANEL == reference_panel))
  curve = p$layers[[cas_model_line_index(p)]]$data
  expect_equal(curve$prediction,unname(predict(m,newdata=curve,re.form=NA)))
})
