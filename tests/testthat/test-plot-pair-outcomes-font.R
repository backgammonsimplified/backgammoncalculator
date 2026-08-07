test_that("plot_pair_outcomes accepts an explicit publication font", {
  plot <- plot_pair_outcomes(
    sage_match_wins = 7,
    gnu_match_wins = 13,
    sage_pair_sweeps = 1,
    tied_pairs = 5,
    gnu_pair_sweeps = 4,
    sage_logo = grid::circleGrob(),
    gnu_logo = grid::circleGrob(),
    font_family = "sans"
  )

  expect_s3_class(plot, "ggplot")
})

test_that("plot_pair_outcomes validates font_family", {
  expect_error(
    plot_pair_outcomes(
      sage_match_wins = 7,
      gnu_match_wins = 13,
      sage_pair_sweeps = 1,
      tied_pairs = 5,
      gnu_pair_sweeps = 4,
      sage_logo = grid::circleGrob(),
      gnu_logo = grid::circleGrob(),
      font_family = ""
    ),
    "`font_family` must be one non-empty string",
    fixed = TRUE
  )
})
