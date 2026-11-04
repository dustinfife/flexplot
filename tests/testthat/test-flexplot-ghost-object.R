
testthat::test_that(
  "Ghost line repeats the reference fit across facets", {
    
    d = data.frame(
      x=rep(1:10, 2),
      group=factor(rep(c("A", "B"), each=10)),
      y=c(
        2 * (1:10) + 3,
        4 * (1:10) + 10
      )
    )
    
    withr::local_options(flexplot.object_renderer=TRUE)
    
    p = flexplot(
      y ~ x | group,
      data=d,
      ghost.line="gray",
      ghost.reference=list(group="A"),
      method="lm"
    )
    
    testthat::expect_s3_class(p, "ggplot")
    
    built = ggplot2::ggplot_build(p)
    
    ghost_idx = which(vapply(
      p$layers,
      function(layer) {
        inherits(layer$geom, "GeomLine") &&
          identical(layer$aes_params$colour, "gray")
      },
      logical(1)
    ))
    
    testthat::expect_length(ghost_idx, 1)
    
    ghost_data = built$data[[ghost_idx]]
    
    testthat::expect_equal(
      length(unique(ghost_data$PANEL)),
      2
    )
    
    testthat::expect_equal(
      unique(round(ghost_data$y - 2 * ghost_data$x, 6)),
      3
    )
  }
)


testthat::test_that(
  "Ghost lines work with multiple conditioning variables", {
    
    d = expand.grid(
      x=1:10,
      group=factor(c("A", "B")),
      treatment=factor(c("Control", "Treatment"))
    )
    
    d$y = with(d,
               2*x +
                 ifelse(group == "B", 10, 0) +
                 ifelse(treatment == "Treatment", 20, 0)
    )
    
    withr::local_options(flexplot.object_renderer=TRUE)
    
    p = flexplot(
      y ~ x | group + treatment,
      data=d,
      ghost.line="gray",
      ghost.reference=list(
        group="A",
        treatment="Control"
      ),
      method="lm"
    )
    
    testthat::expect_s3_class(p, "ggplot")
    
    built = ggplot2::ggplot_build(p)
    
    ghost_idx = which(vapply(
      p$layers,
      function(layer) {
        inherits(layer$geom, "GeomLine") &&
          identical(layer$aes_params$colour, "gray")
      },
      logical(1)
    ))
    
    testthat::expect_length(ghost_idx, 1)
    
    ghost_data = built$data[[ghost_idx]]
    
    # Four panels: A/Control, B/Control,
    # A/Treatment, B/Treatment
    testthat::expect_equal(
      length(unique(ghost_data$PANEL)),
      4
    )
    
    # The reference relationship is y = 2*x.
    # It should be identical in all four panels.
    for (panel in unique(ghost_data$PANEL)) {
      
      panel_data = ghost_data[
        ghost_data$PANEL == panel,
        ,
        drop=FALSE
      ]
      
      testthat::expect_equal(
        panel_data$y,
        2 * panel_data$x,
        tolerance=1e-6
      )
    }
  }
)
