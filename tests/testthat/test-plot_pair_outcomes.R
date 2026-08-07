test_that("plot_pair_outcomes returns a ggplot object", {
  plot <- plot_pair_outcomes(
    sage_match_wins = 7,
    gnu_match_wins = 13,
    sage_pair_sweeps = 1,
    tied_pairs = 5,
    gnu_pair_sweeps = 4,
    sage_logo = grid::circleGrob(),
    gnu_logo = grid::circleGrob()
  )

  expect_s3_class(
    plot,
    "ggplot"
  )

  built <- ggplot2::ggplot_build(
    plot
  )

  expect_true(
    length(built$data) >= 5L
  )
})

test_that("plot_pair_outcomes rejects invalid counts", {
  expect_error(
    plot_pair_outcomes(
      sage_match_wins = -1,
      gnu_match_wins = 13,
      sage_pair_sweeps = 1,
      tied_pairs = 5,
      gnu_pair_sweeps = 4,
      sage_logo = grid::circleGrob(),
      gnu_logo = grid::circleGrob()
    ),
    "non-negative whole numbers"
  )

  expect_error(
    plot_pair_outcomes(
      sage_match_wins = 7.5,
      gnu_match_wins = 13,
      sage_pair_sweeps = 1,
      tied_pairs = 5,
      gnu_pair_sweeps = 4,
      sage_logo = grid::circleGrob(),
      gnu_logo = grid::circleGrob()
    ),
    "non-negative whole numbers"
  )
})

test_that("plot_pair_outcomes requires grob logos", {
  expect_error(
    plot_pair_outcomes(
      sage_match_wins = 7,
      gnu_match_wins = 13,
      sage_pair_sweeps = 1,
      tied_pairs = 5,
      gnu_pair_sweeps = 4,
      sage_logo = "sage.png",
      gnu_logo = grid::circleGrob()
    ),
    "`sage_logo` must be a grid grob"
  )
})

test_that("plot_pair_outcomes requires named outcome colours", {
  expect_error(
    plot_pair_outcomes(
      sage_match_wins = 7,
      gnu_match_wins = 13,
      sage_pair_sweeps = 1,
      tied_pairs = 5,
      gnu_pair_sweeps = 4,
      sage_logo = grid::circleGrob(),
      gnu_logo = grid::circleGrob(),
      colours = c(
        "#d0a128",
        "#d9dde5",
        "#203d63"
      )
    ),
    "named values"
  )
})
