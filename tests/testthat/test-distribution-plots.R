test_that("distribution summary respects group order", {
  data <- data.frame(
    engine = c("GNU", "Sage", "GNU", "Sage"),
    value = c(1, 4, 3, 2)
  )

  result <- metric_distribution_summary(
    data = data,
    value_column = "value",
    group_column = "engine",
    group_levels = c("Sage", "GNU")
  )

  expect_identical(
    as.character(result$group),
    c("Sage", "GNU")
  )
  expect_equal(result$mean, c(3, 2))
  expect_equal(result$median, c(3, 2))
})


test_that("nice distribution limits extend beyond the maximum", {
  expect_equal(
    nice_distribution_limits(
      8.114,
      padding_fraction = 0.10
    ),
    c(0, 9)
  )

  expect_equal(
    nice_distribution_limits(
      9.7,
      padding_fraction = 0.20
    ),
    c(0, 12)
  )

  limit_1_6 <- nice_distribution_limits(
    1.6,
    padding_fraction = 0.10
  )

  expect_equal(limit_1_6[[1L]], 0)
  expect_gt(limit_1_6[[2L]], 1.6)
})


test_that("pseudo-log breaks use readable canonical values", {
  result <- pseudo_log_distribution_breaks(
    limits = c(0, 12),
    minimum_positive = 0.01,
    max_breaks = 20
  )

  expect_true(all(c(0, 0.01, 0.02, 0.05, 0.1, 1, 2, 5, 10) %in% result))
  expect_false(any(result > 12))
})


test_that("density distribution accepts the scale directly", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggiraph")
  skip_if_not_installed("patchwork")

  data <- data.frame(
    group = rep(c("A", "B"), each = 5),
    value = c(1, 2, 3, 4, 5, 2, 3, 4, 5, 6)
  )

  plot <- plot_density_distributions(
    data = data,
    value_column = "value",
    group_column = "group",
    group_levels = c("A", "B"),
    group_colours = c(A = "#915634", B = "#3D5F87"),
    value_formatter = function(x) sprintf("%.1f", x),
    colours = c(
      surface = "#FFFDFC",
      text = "#1B2040",
      text_strong = "#111B35",
      text_muted = "#5F626E",
      grid = "#D8DBE5",
      mean = "#117733",
      median = "#8A6A25"
    ),
    x_scale = "linear",
    figure_title = "Example"
  )

  expect_true(inherits(plot, "patchwork"))

  expect_error(
    plot_density_distributions(
      data = data,
      value_column = "value",
      group_column = "group",
      group_levels = c("A", "B"),
      group_colours = c(A = "#915634", B = "#3D5F87"),
      value_formatter = identity,
      colours = c(
        surface = "#FFFDFC",
        text = "#1B2040",
        text_strong = "#111B35",
        text_muted = "#5F626E",
        grid = "#D8DBE5",
        mean = "#117733",
        median = "#8A6A25"
      ),
      x_scale = "unsupported"
    ),
    "arg"
  )
})
