test_that("jamovi menu order preserves analysis identities and the Flexplot icon route", {
  root = normalizePath(test_path("..",".."))
  module = yaml::read_yaml(file.path(root,"jamovi","0000.yaml"))
  expect_identical(vapply(module$analyses,`[[`,character(1),"name"),
    c("flexplota","glmbasic","glmgeneralized","mixedmodel"))
  expect_identical(vapply(module$analyses,`[[`,character(1),"menuTitle"),
    c("Flexplot","Linear Model","Generalized Linear Model","Mixed Models"))
  expect_true(all(vapply(module$analyses,function(analysis) identical(analysis$menuGroup,"Flexplot"),logical(1))))
  for (name in c("FlexplotLogo-sm.png","FlexplotLogo.png"))
    expect_true(file.exists(file.path(root,"inst","icons",name)))
})
