jasp_theme_dispatch_probe = function(plot) {
  plot + ggplot2::labs(subtitle="JASP theme selected")
}

test_that("all active JASP theme selectors expose the supported values", {
  for (file in c("Flexplot.qml", "linmod.qml", "mixedmod.qml", "glinmod.qml")) {
    qml = readLines(system.file("qml", file, package="flexplot", mustWork=TRUE))
    start = grep('name: "theme"', qml, fixed=TRUE)
    expect_length(start, 1)
    entry = qml[start+1L]
    values = regmatches(entry, gregexpr('"[^"]+"', entry))[[1]]
    expect_identical(gsub('"', '', values, fixed=TRUE),
                       c("JASP", "Black and white", "Minimal", "Classic", "Dark"))
  }
})

test_that("JASP standard themes retain their existing appearance and plotted data", {
  plot = ggplot2::ggplot(small, ggplot2::aes(x=x, y=y, colour=b, group=b)) +
    ggplot2::geom_point() + ggplot2::geom_line()
  themes = list("Black and white"=ggplot2::theme_bw(), Minimal=ggplot2::theme_minimal(),
                  Classic=ggplot2::theme_classic(), Dark=ggplot2::theme_dark())
  for (name in names(themes)) {
    actual = theme_it(plot, name)
    expected = plot + themes[[name]] + ggplot2::theme(text=ggplot2::element_text(size=18))
    expect_equal(actual$theme, expected$theme)
    expect_equal(ggplot2::ggplot_build(actual)$data, ggplot2::ggplot_build(expected)$data)
    expect_identical(actual$labels, expected$labels)
    expect_no_warning(ggplot2::ggplotGrob(actual))
  }
  # Exercise the JASP dispatch branch without requiring its host/font runtime.
  local_mocked_bindings(themeJasp=jasp_theme_dispatch_probe)
  actual = theme_it(plot, "JASP")
  expect_identical(actual$labels$subtitle, "JASP theme selected")
  expect_equal(ggplot2::ggplot_build(actual)$data, ggplot2::ggplot_build(plot)$data)
  expect_error(theme_it(plot, "unknown"), "Invalid theme provided: unknown", fixed=TRUE)
})

test_that("legacy Tron theme values render identically to Dark", {
  plot = ggplot2::ggplot(small, ggplot2::aes(x=x, y=y, colour=b, group=b)) +
    ggplot2::geom_point() + ggplot2::geom_line()
  dark = theme_it(plot, "Dark")
  expected = ggplot2::ggplot_build(dark)
  for (name in c("Tron", "tron", "TRON")) {
    actual = theme_it(plot, name)
    built = ggplot2::ggplot_build(actual)
    expect_equal(actual$theme, dark$theme)
    expect_equal(built$data, expected$data)
    expect_equal(built$layout$layout, expected$layout$layout)
    expect_identical(actual$labels, dark$labels)
    expect_no_warning(ggplot2::ggplotGrob(actual))
  }
})
