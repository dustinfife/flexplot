test_that("categorical mixed-model lines omit singleton groups within panels", {
  withr::local_seed(61)
  
  # Guarantee that School 2305 is included in the sampled schools.
  selected_schools = c("1906", "2305", "2336")
  
  test_data = droplevels(
    math[as.character(math$School) %in% selected_schools, ]
  )
  
  model = lme4::lmer(
    MathAch ~ SES + Sex + (SES | School),
    data=test_data
  )
  
  formulas = list(
    MathAch ~ Sex | SES + School,
    MathAch ~ Sex + School | SES
  )
  
  for (i in seq_along(formulas)) {
    set.seed(61)
    p = visualize(
      model,
      formula=formulas[[i]],
      plot="model",
      jitter=FALSE
    )
    
    expect_s3_class(p$layers[[2]]$geom, "GeomPoint")
    expect_s3_class(p$layers[[3]]$geom, "GeomLine")
    
    points = p$layers[[2]]$data
    lines = p$layers[[3]]$data
    
    # Identify groups using the actual faceting structure.
    keys = paste(points$School, points$SES_binned)
    counts = table(keys)
    keep = as.numeric(counts[keys]) > 1
    
    expect_true(any(keep))
    
    # Only the line rows are filtered. Points remain unchanged.
    expect_identical(names(lines), setdiff(names(points), ".group"))
    
    for (variable in names(lines)) {
      expect_identical(lines[[variable]], points[[variable]][keep])
    }
    
    built = ggplot2::ggplot_build(p)
    layout = built$layout$layout
    markers = built$data[[2]]
    curves = built$data[[3]]
    
    expect_equal(nrow(markers), nrow(points))
    expect_equal(nrow(curves), sum(keep))
    
    # Every drawn line must contain at least two observations
    # within its own panel and group.
    groups = split(
      curves,
      interaction(curves$PANEL, curves$group, drop=TRUE)
    )
    
    expect_true(all(vapply(groups, nrow, integer(1)) > 1L))
    
    # Verify that line coordinates match the retained point summaries.
    expected = points[keep, ]
    
    panel = if (i == 1) {
      match(
        paste(expected$School, expected$SES_binned),
        paste(layout$School, layout$SES_binned)
      )
    } else {
      match(expected$SES_binned, layout$SES_binned)
    }
    
    x = match(
      as.character(expected$Sex),
      built$layout$panel_scales_x[[1]]$get_limits()
    )
    
    expected_coordinates = paste(
      layout$PANEL[panel],
      x,
      round(expected$MathAch, 10)
    )
    
    actual_coordinates = paste(
      curves$PANEL,
      as.numeric(curves$x),
      round(curves$y, 10)
    )
    
    expect_setequal(actual_coordinates, expected_coordinates)
    
    # Preserve separate lines for separate schools.
    for (id in unique(curves$PANEL)) {
      expect_equal(
        length(unique(curves$group[curves$PANEL == id])),
        length(unique(expected$School[layout$PANEL[panel] == id]))
      )
    }
    
    # When singleton groups occur, retain their points but omit
    # those specific panel/group combinations from line drawing.
    if (any(!keep)) {
      singleton_keys = unique(keys[!keep])
      singleton_points = points[keys %in% singleton_keys, ]
      
      expect_gt(nrow(singleton_points), 0)
      expect_false(any(
        paste(lines$School, lines$SES_binned) %in% singleton_keys
      ))
      
      if (i == 1) {
        singleton_panels = layout$PANEL[
          paste(layout$School, layout$SES_binned) %in% singleton_keys
        ]
        
        expect_true(any(markers$PANEL %in% singleton_panels))
        expect_false(any(curves$PANEL %in% singleton_panels))
      }
    }
    
    # Fixed effects use group=1, even when School is a second-axis group.
    # Count the actual marker rows per rendered panel rather than per school.
    fixed_markers = built$data[[4]]
    fixed_curves = built$data[[5]]
    fixed_counts = table(fixed_markers$PANEL)
    fixed_keep = as.numeric(fixed_counts[as.character(fixed_markers$PANEL)]) > 1L
    expect_equal(nrow(fixed_markers), nrow(p$layers[[4]]$data))
    expect_equal(nrow(fixed_curves), sum(fixed_keep))
    if (i == 1) expect_true(any(!fixed_keep))
    expect_equal(
      sort(paste(fixed_curves$PANEL, fixed_curves$x, round(fixed_curves$y, 10))),
      sort(paste(fixed_markers$PANEL[fixed_keep], fixed_markers$x[fixed_keep],
                   round(fixed_markers$y[fixed_keep], 10)))
    )
    expect_true(all(fixed_curves$group == 1L))
    expect_true(all(fixed_curves$colour == "black"))
    expect_true(all(fixed_curves$linetype == 1L))

    # Every line layer must be drawable within its actual PANEL/group pairs.
    for (j in seq_along(p$layers)) {
      if (!inherits(p$layers[[j]]$geom, "GeomLine")) next
      counts = dplyr::count(built$data[[j]], PANEL, group)
      expect_true(all(counts$n >= 2L))
    }
    expect_no_message(ggplot2::ggplotGrob(p))
  }
})
