test_that("plot_er_comparison returns a ggplot", {
  example_data <- data.frame(
    engine = c(
      "Sage",
      "GNU"
    ),
    er = c(
      2.413,
      2.105
    ),
    stringsAsFactors = FALSE
  )

  plot <- plot_er_comparison(
    data = example_data
  )

  expect_s3_class(
    plot,
    "ggplot"
  )
})

test_that("plot_er_comparison validates required columns", {
  example_data <- data.frame(
    engine = c(
      "Sage",
      "GNU"
    ),
    stringsAsFactors = FALSE
  )

  expect_error(
    plot_er_comparison(
      data = example_data
    ),
    "Missing required column"
  )
})

test_that("plot_er_comparison validates ER values", {
  example_data <- data.frame(
    engine = c(
      "Sage",
      "GNU"
    ),
    er = c(
      2.4,
      -1
    ),
    stringsAsFactors = FALSE
  )

  expect_error(
    plot_er_comparison(
      data = example_data
    ),
    "ER values must be finite numeric values"
  )
})
